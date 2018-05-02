/*! \file query-util.hxx
 *
 * Utility functions to support spatial queries.  Used by both the kdtree implementation
 * and the brute-force implementation.
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_QUERY_UTIL_HXX_
#define _DIDEROT_QUERY_UTIL_HXX_

namespace diderot {

    namespace __details {

      // distance function
        template <const uint32_t D, typename REAL>
        REAL distance (const REAL pos1[D], const REAL pos2[D])
        {
            float sum = REAL(0);
            for (uint32_t i = 0;  i < D;  i++) {
                REAL d = pos1[i] - pos2[i];
                sum += (d * d);
            }
            return std::sqrt(sum);
        }

      // generic test for two points being within a given radius^2
        template <const uint32_t D, typename REAL>
        bool within_sphere (const REAL pos1[D], const REAL pos2[D], REAL radius2)
        {
            float sum = REAL(0);
            for (uint32_t i = 0;  i < D;  i++) {
                REAL d = pos1[i] - pos2[i];
                sum += (d * d);
            }
            return sum < radius2;
        }

      // generic test for two points being within a specified rectangular radius?
        template <const uint32_t D, typename REAL>
        bool within_box (const REAL pos1[D], const REAL pos2[D], REAL radius)
        {
            for (uint32_t i = 0;  i < D;  i++) {
                if (std::abs(pos1 - pos2) > radius) {
                    return false;
                }
            }
            return true;
        }

    } // namespace __details

} // namespace diderot

#endif //! _DIDEROT_QUERY_UTIL_HXX_
