/*! \file util.hxx
 *
 * \author John Reppy
 *
 * Various utility definitions.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_UTIL_HXX_
#define _DIDEROT_UTIL_HXX_

namespace diderot {

  // similar to the std::array type, but stripped down
    template <typename T, size_t SZ>
    struct array {
        typedef T value_type;
        typedef value_type &reference;
        typedef const value_type &const_reference;

        value_type      _elems[SZ];

        reference operator[] (size_t i) noexcept { return _elems[i]; }
        const_reference operator[] (size_t i) const noexcept { return _elems[i]; }

        value_type *data () noexcept { return _elems; }

        const value_type* data () const noexcept { return _elems; }

        size_t size () const noexcept { return SZ; }

    };

    inline float ceiling (float x) { return ::ceil(x); }
    inline double ceiling (double x) { return ::ceil(x); }
    inline float floor (float x) { return ::floorf(x); }
    inline double floor (double x) { return ::floor(x); }
    inline float round (float x) { return ::roundf(x); }
    inline double round (double x) { return ::round(x); }
    inline float trunc (float x) { return ::truncf(x); }
    inline double trunc (double x) { return ::trunc(x); }

    inline float sign (float x) { return (x > 0) - (x < 0); }
    inline double sign (double x) { return (x > 0) - (x < 0); }

    namespace __details {

      //! set the properties of an output stream (precision and alphabool)
        inline void config_ostream (std::ostream &os)
        {
            os << std::boolalpha;
#ifdef DIDEROT_SINGLE_PRECISION
      // based on https://en.wikipedia.org/wiki/Single-precision_floating-point_format
            os << std::setprecision(9);
#else /* DIDEROT_DOUBLE_PRECISION */
      // based on https://en.wikipedia.org/wiki/Double-precision_floating-point_format
            os << std::setprecision(17);
#endif
        }

    } // namespace __details

} // namespace diderot

#endif // !_DIDEROT_UTIL_HXX_
