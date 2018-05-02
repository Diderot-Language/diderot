/*! \file base.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_BASE_HXX_
#define _DIDEROT_BASE_HXX_

#include "config.h"

// teem headers
#include <teem/air.h>
#include <teem/biff.h>
#include <teem/nrrd.h>

// C library headers
#include <cmath>
#include <cstring>
#include <cassert>

// C++ std library headers
#include <string>
#include <fstream>
#include <ostream>
#include <iostream>
#include <iomanip>

namespace diderot {

    enum strand_status {
        kActive = 0,            //!< a currently running strand
        kNew = 1,               //!< a newly created, but not yet active, strand
        kStabilize,             //!< a strand that will be stable at the end of
                                //!< the current step
        kStable,                //!< a stable strand
        kDie,                   //!< a dying strand
        kDead                   //!< a dead strand
    };

  // predicates on strand_status values
    inline bool activeSts (strand_status sts) { return (sts <= kNew); }
    inline bool notActiveSts (strand_status sts) { return (sts > kNew); }
    inline bool aliveSts (strand_status sts) { return (sts < kDie); }
    inline bool notAliveSts (strand_status sts) { return (sts >= kDie); }

  // standard vector types
    template <typename REAL> using real2 = REAL[2];
    template <typename REAL> using real3 = REAL[3];

  // standard matrix types
    template <typename REAL> using mat2x2 = REAL[4];
    template <typename REAL> using mat3x3 = REAL[9];
    template <typename REAL> using mat4x4 = REAL[16];

    namespace __details {

        struct nrrd_type_info {
            bool isFloat;
            unsigned int sizeb;
            const char *name;
        };

        extern struct nrrd_type_info nrrd_type_info[nrrdTypeLast];

    } // namespace __details

} // namespace diderot

// cach-line size (default Xeon has 64-byte lines)
#ifndef CACHE_LINE_SIZE
#define CACHE_LINE_SIZE         64
#endif

// cache-line alignment
#define CACHE_ALIGN     __attribute__((aligned(CACHE_LINE_SIZE)))

// disable inlining for a function
#define NO_INLINE       __attribute__((noinline))

#endif // !_DIDEROT_BASE_HXX_
