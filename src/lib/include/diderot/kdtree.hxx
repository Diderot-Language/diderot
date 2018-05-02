/*! \file kdtree.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_KDTREE_HXX_
#define _DIDEROT_KDTREE_HXX_

#ifndef _DIDEROT_BASE_HXX_
#include "diderot/base.hxx"
#endif

namespace diderot {

  //! kdtrees are parameterized over the spatial dimension D, the real type, and the
  //! strand_array type
    template <const uint32_t D, typename REAL, typename SA>
    struct kdtree {
        using strand_t = typename SA::strand_t;
        using index_t = typename SA::index_t;
        using sid_t = typename SA::sid_t;

// QUESTION: should this value be setable from the command-line?
        static const int STRANDS_PER_LEAF = 20;
      // bound on minimum leaf size based in the 25% tolerance used with partitioning
        static const int MIN_LEAF_SIZE = ((STRANDS_PER_LEAF >> 1) - (STRANDS_PER_LEAF >> 3) - 1);

      //! nodes in the kdtree
        struct node {
            uint32_t    _lc;    // _pool[_lc] is left child; 0 for leaf
            uint32_t    _rc;    // _pool[_rc] is right child; undefined for leaf
            union {
                struct {
                    uint32_t    _first; // _parts[_first.._last] are the IDs of
                    uint32_t    _last;  // the strands in this leaf
                } _leaf;
                struct {
                    uint32_t    _axis;  // split axis
                    sid_t       _id;    // Strand ID of split node
                } _nd;
            } _u;
            bool isLeaf () const { return this->_lc == 0; }
            uint32_t axis () const { return this->_u._nd._axis; }
        };

        uint32_t        _nStrands;      // number of alive strands in _strands array
        uint32_t        _partsSz;       // number of entries in _parts array
        uint32_t        _poolSz;        // number of nodes in _pool array
        uint32_t        _nextNode;      // next node to allocate from pool
        sid_t           *_parts;        // array of strand IDs; used to partition
                                        // strands by position.  Note that the indices
                                        // of this array are not the same as the strand
                                        // array indices
        node            *_pool;         // pool of available nodes
        const SA        *_strands;

        explicit kdtree (const SA *strands);
        ~kdtree ();

      // update the _parts array
        void update_strands ();

      // rebuild operation
        void rebuild ();

        dynseq<sid_t> sphere_query (const strand_t * self, const REAL center[D], REAL radius);

      // navigation
        const node *root () const { return &this->_pool[0]; }
        const node *left (const node *nd) const { return &this->_pool[nd->_lc]; }
        const node *right (const node *nd) const { return &this->_pool[nd->_rc]; }
        const strand_t *strand (const node *nd) const
        {
            return this->_strands->id_to_strand(nd->_u._nd._id);
        }
        const strand_t *strand (index_t ix) const
        {
            assert (ix < this->_partsSz);
            return this->_strands->id_to_strand(this->_parts[ix]);
        }

      /***** support for tree building *****/

      // swap parts[i] with parts[j]
        void swap_parts (index_t i, index_t j);

      // partition _parts[lo..hi] such that _parts[lo..ix-1] < X <= _parts[ix..hi], where
      // X is the initial value of _strands->strands(_parts[pivotIx])->pos()[axis] and
      // ix is the return value.
      //
        index_t partition (uint32_t axis, index_t lo, index_t hi, index_t pivotIx);

      // partition _parts[lo..hi] into _parts[lo..m] and _parts[m+1..hi] such that the strand
      // with id _parts[m] has the median position on the specified axis.
      // We use the "Quick Select" method (https://en.wikipedia.org/wiki/Quickselect)
      //
        index_t median (uint32_t axis, index_t lo, index_t hi);

      // build a tree for the strands with indices from lo to hi
        index_t builder (uint32_t axis, index_t lo, index_t hi);

        void print (std::ostream &outS, uint32_t depth, const node *nd);
        void print (std::ostream &outS = std::cout);

    };

} // namespace Diderot

#include "kdtree-inst.hxx"

#endif // !_DIDEROT_KDTREE_HXX_
