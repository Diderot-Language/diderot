/*! \file brute-force-query.hxx
 *
 * \author John Reppy
 *
 * This is a brute-force implementation (i.e., O(n^2) of spatial queries for testing
 * purposes.  It is designed to match the kdtree API (see kdtree.hxx), but is implementend
 * using strand-to-strand comparisons.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_BRUTE_FORCE_QUERY_HXX_
#define _DIDEROT_BRUTE_FORCE_QUERY_HXX_

#ifndef _DIDEROT_BASE_HXX_
#include "diderot/base.hxx"
#endif

#include "query-util.hxx"

namespace diderot {

  //! kdtrees are parameterized over the spatial dimension D, the real type, and the
  //! strand_array type
    template <const uint32_t D, typename REAL, typename SA>
    struct kdtree {
        using strand_t = typename SA::strand_t;
        using index_t = typename SA::index_t;
        using sid_t = typename SA::sid_t;

        const SA        *_strands;
        uint32_t        _nStrands;      // number of alive strands in _strands array
        uint32_t        _partsSz;       // number of entries in _parts array
        sid_t           *_parts;        // array of strand IDs of the strands that are
                                        // alive at the start of the superstep

        explicit kdtree (const SA *strands);
        ~kdtree ();

        void update_strands ();
        void rebuild () { }

        dynseq<sid_t> sphere_query (const strand_t * self, const REAL pos[D], REAL radius);
    };

  // constructor
    template <const uint32_t D, typename REAL, typename SA>
    kdtree<D,REAL,SA>::kdtree (const SA *strands)
        : _strands(strands), _nStrands(strands->num_alive()), _parts(nullptr)
    {
// FIXME: for "create_array" programs, we don't need the extra space!
        this->_partsSz = this->_nStrands + (this->_nStrands >> 2);
        this->_parts = new sid_t[this->_partsSz];
        uint32_t pix = 0;
        for (auto ix = this->_strands->begin_alive();
            ix != this->_strands->end_alive();
            ix = this->_strands->next_alive(ix))
        {
            assert (pix < this->_partsSz);
            this->_parts[pix++] = this->_strands->id(ix);
        }
    }

  // destructor
    template <const uint32_t D, typename REAL, typename SA>
    kdtree<D,REAL,SA>::~kdtree ()
    {
        delete[] _parts;
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
            this->_nStrands = nStrands;
        }
    }

  // sphere_query
  //
    template <const uint32_t D, typename REAL, typename SA>
    dynseq<typename kdtree<D,REAL,SA>::sid_t> kdtree<D,REAL,SA>::sphere_query (
        const kdtree<D,REAL,SA>::strand_t *self, const REAL center[D], REAL radius)
    {
        dynseq<sid_t> result;

      // return empty sequence on empty sphere
        if (radius <= 0.0) {
            return result;
        }

        const SA *strands = this->_strands;
        REAL radius2 = radius * radius;
        uint32_t inIdx = strands->in_state_index();

        for (index_t ix = strands->begin_alive();
            ix != strands->end_alive();
            ix = strands->next_alive(ix))
        {
            assert(strands->status(ix) <= kDie);
            const strand_t *strand = strands->strand(ix);
            if ((self != strand)
            && __details::within_sphere<D,REAL>(strand->pos(inIdx), center, radius2)) {
              // add the strand to the result list
                result.append (strands->id(ix));
            }
        }

        return result;
    }

} // namespace diderot

#endif // !_DIDEROT_BRUTE_FORCE_QUERY_HXX_
