/*! \file kdtree-inst.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_KDTREE_INST_HXX_
#define _DIDEROT_KDTREE_INST_HXX_

#ifndef _DIDEROT_KDTREE_HXX_
# error kdtree-inst.hxx should not be directly included
#endif

#include <stack>

#include "query-util.hxx"

namespace diderot {

    template <const uint32_t D, typename REAL, typename SA>
    kdtree<D,REAL,SA>::kdtree (const SA *strands)
        : _nStrands(strands->num_alive()), _poolSz(strands->num_alive()),
          _parts(nullptr), _pool(nullptr), _strands(strands)
    {
// FIXME: for "create_array" programs, we don't need the extra space!
        uint32_t nStrands = strands->num_alive();
        this->_partsSz = nStrands + (nStrands >> 2);
        this->_parts = new sid_t[this->_partsSz];
        uint32_t pix = 0;
        for (auto ix = this->_strands->begin_alive();
            ix != this->_strands->end_alive();
            ix = this->_strands->next_alive(ix))
        {
            assert (pix < this->_partsSz);
            this->_parts[pix++] = this->_strands->id(ix);
        }

        uint32_t reqNumNodes = 2 * ((nStrands + MIN_LEAF_SIZE - 1) / MIN_LEAF_SIZE) - 1;
        this->_pool = new node[reqNumNodes];
        this->_poolSz = reqNumNodes;
    }

    template <const uint32_t D, typename REAL, typename SA>
    kdtree<D,REAL,SA>::~kdtree ()
    {
        delete[] _parts;
        delete[] _pool;
    }

    template <const uint32_t D, typename REAL, typename SA>
    void kdtree<D,REAL,SA>::swap_parts (index_t i, index_t j)
    {
        std::swap (this->_parts[i], this->_parts[j]);
    }

  // partition _parts[lo..hi] such that _parts[lo..ix-1] < X <= _parts[ix..hi], where
  // X is the initial value of _strands->strands(_parts[pivotIx])->pos()[axis] and
  // ix is the return value.
  //
    template <const uint32_t D, typename REAL, typename SA>
    typename kdtree<D,REAL,SA>::index_t kdtree<D,REAL,SA>::partition (
        uint32_t axis, index_t lo, index_t hi, index_t pivotIx)
    {
        uint32_t inIdx = this->_strands->in_state_index();
        REAL X = this->strand(pivotIx)->pos(inIdx)[axis];

      // move pivot element to end
        this->swap_parts(pivotIx, hi);

        index_t jx = lo;
      // skip over leading values that are already in the right place
        while ((jx < hi) && (this->strand(jx)->pos(inIdx)[axis] < X)) {
            jx++;
        }
        assert (this->strand(jx)->pos(inIdx)[axis] >= X);
        index_t ix = jx;
        while (jx < hi) {
          // INV: strands from lo to ix-1 are < X
          //      strands from ix to jx-1 are >= X
            if (this->strand(jx)->pos(inIdx)[axis] < X) {
                this->swap_parts (ix, jx);
                ix++;
            }
            jx++;
        }

        this->swap_parts (ix, hi);

        return ix;
    }

  // partition _parts[lo..hi] into _parts[lo..m] and _parts[m+1..hi] such that the strand
  // with id _parts[m] has the median position on the specified axis.
  // We use the "Quick Select" method (https://en.wikipedia.org/wiki/Quickselect)
  //
    template <const uint32_t D, typename REAL, typename SA>
    typename kdtree<D,REAL,SA>::index_t kdtree<D,REAL,SA>::median (
        uint32_t axis, index_t lo, index_t hi)
    {
        assert (hi - lo >= STRANDS_PER_LEAF);
      // the mid-point of the interval [lo..hi]
        index_t mid = (hi + lo) >> 1;

      // partition until we are within +/- 25% of the mid-point
        uint32_t tol = ((mid - lo) >> 2);

        while (true) {
            index_t pivotIx = this->partition (axis, lo, hi, mid);
            if (std::abs(static_cast<int>(pivotIx) - static_cast<int>(mid)) <= tol) {
                return pivotIx;
            }
            else if (pivotIx < mid) {
                lo = pivotIx + 1;
            }
            else {
                hi = pivotIx - 1;
            }
        }

    }

    template <const uint32_t D, typename REAL, typename SA>
    typename kdtree<D,REAL,SA>::index_t kdtree<D,REAL,SA>::builder (
        uint32_t axis, index_t lo, index_t hi)
    {
        assert (lo <= hi);

      // allocate the node
        uint32_t nd = this->_nextNode++;
        assert (nd < this->_poolSz);

        index_t n = hi - lo + 1;
        if (n <= kdtree<D,REAL,SA>::STRANDS_PER_LEAF) {
          // allocate a leaf
            this->_pool[nd]._lc = 0;
            this->_pool[nd]._u._leaf._first = lo;
            this->_pool[nd]._u._leaf._last = hi;
//std::cout << "allocate leaf for " << lo << ".." << hi << " (" << n << ")\n";
        }
        else {
            index_t mid = this->median (axis, lo, hi);
//std::cout << "allocate node for " << lo << ".." << mid << ".." << hi << " (" << (mid-lo) << ":" << (hi-mid) << ")\n";
            // INV: strands indexed by _parts[lo..mid-1] are < strand[_parts[mid]]
            // and strand[_parts[mid]] <= strands indexed by _parts[mid+1..hi]
            this->_pool[nd]._u._nd._id = this->_parts[mid];
            this->_pool[nd]._u._nd._axis = axis;
            axis = (axis + 1) % D;
            this->_pool[nd]._lc = builder (axis, lo, mid-1);
            this->_pool[nd]._rc = builder (axis, mid+1, hi);
        }

        return nd;
    }

  // update the _parts array
    template <const uint32_t D, typename REAL, typename SA>
    void kdtree<D,REAL,SA>::update_strands ()
    {
        const SA *strands = this->_strands;
        uint32_t nStrands = strands->num_alive();
        if (this->_nStrands >= nStrands) {
          // # of strands has shrunk from last call to update_strands
            uint32_t pix = 0;
            for (auto ix = strands->begin_alive();
                ix != strands->end_alive();
                ix = strands->next_alive(ix))
            {
                assert (pix < nStrands);
                this->_parts[pix++] = strands->id(ix);
            }
            this->_nStrands = nStrands;
        }
        else if (this->_nStrands < nStrands) {
          // # of strands has grown from last call to update_strands
            if (this->_partsSz < nStrands) {
              // need to reallocate the _parts array
                this->_partsSz = nStrands + (nStrands >> 2);
                delete[] this->_parts;
                this->_parts = new sid_t[this->_partsSz];
            }
            uint32_t pix = 0;
            for (auto ix = strands->begin_alive();
                ix != strands->end_alive();
                ix = strands->next_alive(ix))
            {
                assert (pix < nStrands);
                this->_parts[pix++] = strands->id(ix);
            }

          // a conservative bound on the number of nodes is 2*(ceil(nStrands / MIN_LEAF_SIZE)) - 1
            uint32_t reqNumNodes = 2 * ((nStrands + MIN_LEAF_SIZE - 1) / MIN_LEAF_SIZE) - 1;
            if (this->_poolSz < reqNumNodes) {
              // grow the pool of nodes
                delete[] this->_pool;
                this->_pool = new node[reqNumNodes];
                this->_poolSz = reqNumNodes;
            }
            this->_nStrands = nStrands;
        }
    }

    template <const uint32_t D, typename REAL, typename SA>
    inline void kdtree<D,REAL,SA>::rebuild ()
    {
        this->_nextNode = 0;
        uint32_t root = this->builder (0, 0, this->_strands->num_alive()-1);
        assert (root == 0);
    }

//#define CHECK_KD_TREE_QUERY
    template <const uint32_t D, typename REAL, typename SA>
    dynseq<typename kdtree<D,REAL,SA>::sid_t> kdtree<D,REAL,SA>::sphere_query (
        const kdtree<D,REAL,SA>::strand_t *self, const REAL center[D], REAL radius)
    {
        dynseq<sid_t> result;
#ifdef CHECK_KD_TREE_QUERY
        std::vector<const strand_t *> res1, res2;
#endif

      // return empty sequence on empty sphere
        if (radius <= 0.0) {
            return result;
        }

      // stack of nodes for which we must still visit
        std::stack<const node *> stk;

        REAL radius2 = radius * radius;
        const node *nd = this->root();
        uint32_t inIdx = this->_strands->in_state_index();
        do {
            if (nd->isLeaf()) {
              // check strands in leaf to see if they are within the sphere
                for (index_t i = nd->_u._leaf._first;  i <= nd->_u._leaf._last;  i++) {
                    const strand_t *strand = this->strand(i);
                    if ((self != strand)
                    && __details::within_sphere<D,REAL>(strand->pos(inIdx), center, radius2)) {
                      // add the strand to the result list
                        result.append (this->_parts[i]);
#ifdef CHECK_KD_TREE_QUERY
                        res1.push_back(strand);
#endif
                    }
                }
                if (! stk.empty()) {
                  // continue searching
                    nd = stk.top ();
                    stk.pop ();
                }
                else {
                    nd = nullptr;
                }
            }
            else {
                uint32_t axis = nd->axis();
                const strand_t *strand = this->strand(nd);
                const REAL *sPos = strand->pos(inIdx);
                REAL sPosX = sPos[axis];
                if (center[axis] < sPosX - radius) {
                  // nd and nodes on right must be outside sphere
                    nd = this->left(nd);
                }
                else if (sPosX + radius < center[axis]) {
                  // nd and nodes on left must be outside sphere
                    nd = this->right(nd);
                }
                else {
                    if ((self != strand)
                    && __details::within_sphere<D,REAL>(sPos, center, radius2)) {
                      // add the strand to the result list
                        result.append (nd->_u._nd._id);
#ifdef CHECK_KD_TREE_QUERY
                        res1.push_back(strand);
#endif
                    }
                    stk.push(this->right(nd));  // visit right child later
                    nd = this->left(nd);
                }
            }
        } while (nd != nullptr);

#ifdef CHECK_KD_TREE_QUERY
        { /* brute-force computation of neighbors */
            const SA *strands = this->_strands;

            for (int i = 0;  i < this->_nStrands;  i++) {
                const strand_t *strand = this->strand(i);
                if ((self != strand)
                && __details::within_sphere<D,REAL>(strand->pos(inIdx), center, radius2)) {
                  // add the strand to the result list
                    res2.push_back(strand);
                }
            }

          // compare results
            bool ok = res1.size() == res2.size();
            if (! ok) {
                std::cout << "** problem with sphere_query: different numbers of strands\n";
            }
            for (auto it = res1.cbegin();  ok && (it != res1.cend());  ++it) {
                bool found = false;
                for (auto jt = res2.cbegin();  jt != res2.cend();  ++jt) {
                    if (*it == *jt) {
                        found = true;
                        break;
                    }
                }
                ok = found;
                if (! ok) {
                    std::cout << "** problem with sphere_query: strand " << *it
                        << " is missing from brute-force result\n";
                }
            }

          // check for an error
            if (! ok) {
                std::ofstream outS("dump.txt");
                outS << "**** Error in kdtree::sphere_query (_, [" << center[0];
                for (int i = 1;  i < D;  i++) {
                    outS << ", " << center[i];
                }
                outS << "], " << radius << ") ****\n";
                outS << "** kdtree result (" << res1.size() << " strands): \n";
                for (auto it = res1.cbegin();  it != res1.cend();  ++it) {
                    const REAL *pos = (*it)->pos(inIdx);
                    outS << "   " << (*it) << " [" << pos[0];
                    for (int i = 1;  i < D;  i++) {
                        outS << ", " << pos[i];
                    }
                    outS << "]; distance = "
                        << __details::distance<D,REAL>(center, (*it)->pos(inIdx)) << "\n";
                }
                outS << "** brute-force result (" << res2.size() << " strands): \n";
                for (auto it = res2.cbegin();  it != res2.cend();  ++it) {
                    const REAL *pos = (*it)->pos(inIdx);
                    outS << "   " << (*it) << " [" << pos[0];
                    for (int i = 1;  i < D;  i++) {
                        outS << ", " << pos[i];
                    }
                    outS << "]; distance = "
                        << __details::distance<D,REAL>(center, (*it)->pos(inIdx)) << "\n";
                }
                outS << "** all strands\n";
                for (index_t ix = strands->begin_alive();
                    ix != strands->end_alive();
                    ix = strands->next_alive(ix))
                {
                    const strand_t *strand = strands->strand(ix);
                    const REAL *pos = strand->pos(inIdx);
                    if (self == strand) { outS << ">> "; } else { outS << "   "; }
                    outS << ix << ". sid = " << strands->id(ix) << "; pos = [" << pos[0];
                    for (int i = 1;  i < D;  i++) {
                        outS << ", " << pos[i];
                    }
                    outS << "]; status = " << strands->status(ix) << "\n";
                }
                outS << "** kd-tree\n";
                this->print (outS, 1, this->root());
                outS << "****\n";
                outS.close();
                std::cout << "   see dump.txt for details\n";
                exit (1);
            }
        }
#endif // CHECK_KD_TREE_QUERY

        return result;
    }

    template <const uint32_t D, typename REAL, typename SA>
    void kdtree<D,REAL,SA>::print (std::ostream &outS, uint32_t depth, const node *nd)
    {
        uint32_t inIdx = this->_strands->in_state_index();
        for (int i = 0;  i < depth;  i++) {
            outS << "  ";
        }
        if (nd->isLeaf()) {
            outS << "Leaf: {";
            for (int i = nd->_u._leaf._first;  i < nd->_u._leaf._last;  i++) {
                if (i != nd->_u._leaf._first) {
                    outS << ", ";
                }
                sid_t id = this->_parts[i];
                const REAL *pos = this->_strands->id_to_strand(id)->pos(inIdx);
                outS << id << " @ [" << pos[0];
                for (int i = 1;  i < D;  i++) { outS << "," << pos[i]; }
                outS << "]";
            }
            outS << "}\n";
        }
        else {
            const REAL *pos = this->_strands->id_to_strand(nd->_u._nd._id)->pos(inIdx);
            outS << "Node: axis = " << nd->axis() << "; id = " << nd->_u._nd._id
                << " @ [" << pos[0];
            for (int i = 1;  i < D;  i++) { outS << "," << pos[i]; }
            outS << "]\n";
            this->print (outS, depth+1, this->left(nd));
            this->print (outS, depth+1, this->right(nd));
        }
    }

    template <const uint32_t D, typename REAL, typename SA>
    void kdtree<D,REAL,SA>::print (std::ostream &outS)
    {
        this->print (outS, 0, this->root());
    }

} // namespace diderot

#endif // !_DIDEROT_KDTREE_INST_HXX_
