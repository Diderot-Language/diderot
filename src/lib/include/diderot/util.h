/*! \file util.h
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

#ifndef _DIDEROT_UTIL_H_
#define _DIDEROT_UTIL_H_

namespace diderot {

  //! similar to the std::array type, but stripped down
    template <typename T, size_t SZ>
    struct array {
        typedef T value_type;
        typedef value_type &reference;
        typedef const value_type &const_reference;

        value_type _elems[SZ];

        //! default constructor
        HOST_DEVICE array() = default;

        template <typename... Args>
        HOST_DEVICE array (Args&&... values) : _elems{values...} {}

        HOST_DEVICE array (array *copy)
        {
            std::copy(
                std::begin(copy->_elems),
                std::end(copy->_elems),
                std::begin(_elems));
        }

        HOST_DEVICE reference operator[] (size_t i) noexcept
        {
            return _elems[i];
        }
        HOST_DEVICE const_reference operator[] (size_t i) const noexcept
        {
            return _elems[i];
        }

        HOST_DEVICE value_type *data () noexcept { return _elems; }

        HOST_DEVICE const value_type* data () const noexcept { return _elems; }

        HOST_DEVICE size_t size () const noexcept { return SZ; }

#ifdef DIDEROT_TARGET_CUDA
        cudaError_t copyToDeviceAsync (array<T, SZ> *tgt)
        {
            return cudaMemcpyAsync(
                tgt->_elems, this->_elems,
                sizeof(T) * SZ,
                cudaMemcpyHostToDevice);
        }

        cudaError_t copyToDevice(array<T, SZ> *tgt)
        {
            return cudaMemcpy(
                tgt->_elems,
                this->_elems,
                sizeof(T) * SZ,
                cudaMemcpyHostToDevice);
        }
#endif // DIDEROT_TARGET_CUDA
    };

    HOST_DEVICE inline float ceiling (float x) { return ::ceil(x); }
    HOST_DEVICE inline double ceiling (double x) { return ::ceil(x); }
    HOST_DEVICE inline float floor (float x) { return ::floorf(x); }
    HOST_DEVICE inline double floor (double x) { return ::floor(x); }
    HOST_DEVICE inline float round (float x) { return ::roundf(x); }
    HOST_DEVICE inline double round (double x) { return ::round(x); }
    HOST_DEVICE inline float trunc (float x) { return ::truncf(x); }
    HOST_DEVICE inline double trunc (double x) { return ::trunc(x); }

    HOST_DEVICE inline float sign (float x) { return (x > 0) - (x < 0); }
    HOST_DEVICE inline double sign (double x) { return (x > 0) - (x < 0); }

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

#endif // !_DIDEROT_UTIL_H_
