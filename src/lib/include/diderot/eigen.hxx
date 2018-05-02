/*! \file eigen.hxx
 *
 * This file contains utility definitions for the various Eigen vector/value routines.
 * The routines themselves are included in the source file based on need.
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_EIGEN_HXX_
#define _DIDEROT_EIGEN_HXX_

#ifndef _DIDEROT_DIDEROT_HXX_
#  error eigen.hxx should not be directly included
#endif

namespace diderot {

    namespace __details {

#if defined(DIDEROT_SINGLE_PRECISION)
        const float EPSILON = (16.0f * FLT_EPSILON);
#else
        const double EPSILON = (16.0 * DBL_EPSILON);
#endif

        const int ROOT_DOUBLE = 1;
        const int ROOT_TWO = 2;
        const int ROOT_TRIPLE = 2;              /* ell_cubic_root_triple */
        const int ROOT_SINGLE_DOUBLE = 3;       /* ell_cubic_root_single_double */
        const int ROOT_THREE = 4;               /* ell_cubic_root_three */

        template <typename REAL>
        inline void normalize2 (REAL *v)
        {
            REAL s = std::sqrt (v[0]*v[0] + v[1]*v[1]);
            if (s > EPSILON) {
                s = REAL(1.0) / s;
                v[0] *= s;
                v[1] *= s;
            }
        }

        template <typename REAL>
        inline void normalize3 (REAL *v)
        {
            REAL s = std::sqrt (v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
            if (s > EPSILON) {
                s = REAL(1.0) / s;
                v[0] *= s;
                v[1] *= s;
                v[2] *= s;
            }
        }

    } // namespace __details

} // namespace diderot

#endif // !_DIDEROT_EIGEN_HXX_
