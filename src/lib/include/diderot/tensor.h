/*! \file tensor.h
 *
 * \author John Reppy
 *
 * Base classes for the generated tensor_shape and tensor_ref_shape classes generated
 * by the compiler.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_TENSOR_H_
#define _DIDEROT_TENSOR_H_

namespace diderot {

    template <typename REAL, const int N>
    struct tensor_ref {
        const REAL *_data;
        HOST_DEVICE tensor_ref () : _data(nullptr) { }
        HOST_DEVICE tensor_ref (const REAL *src) : _data(src) { }
        HOST_DEVICE REAL const &operator[] (uint32_t i) const
        {
            return this->_data[i];
        }
        HOST_DEVICE const REAL *base ()
        {
            return this->_data;
        }
        HOST_DEVICE const REAL *addr (uint32_t i = 0) const
        {
            return &this->_data[i];
        }
    };

    template <typename REAL, const int N>
    struct tensor {
        REAL _data[N];
        HOST_DEVICE tensor () { }
        HOST_DEVICE tensor(const tensor &t)
        {
            this->copy(t.base());
        }
        HOST_DEVICE tensor (std::initializer_list< REAL > const &il)
        {
            this->copy(il);
        }
        HOST_DEVICE tensor (const REAL *src)
        {
            this->copy(src);
        }
        HOST_DEVICE ~tensor () = default;

        HOST_DEVICE void copy (const REAL *src)
        {
#ifdef DIDEROT_TARGET_CUDA
            for (int i=0; i< N; i++) {
                this->base()[i] = src[i];
            }
#else
            std::memcpy(this->_data, src, N*sizeof(REAL));
#endif // DIDEROT_TARGET_CUDA
        }
        HOST_DEVICE void copy (std::initializer_list< REAL > const &il)
        {
            int32_t i = 0;
            for (auto it = il.begin(); it != il.end(); ++i, ++it) {
                this->_data[i] = *it;
            }
        }
        HOST_DEVICE REAL &operator[] (uint32_t i)
        {
            return this->_data[i];
        }
        HOST_DEVICE REAL const &operator[] (uint32_t i) const
        {
            return this->_data[i];
        }
        HOST_DEVICE REAL *base ()
        {
            return &this->_data[0];
        }
        HOST_DEVICE const REAL *base () const
        {
            return &this->_data[0];
        }
        HOST_DEVICE const REAL *addr (uint32_t i = 0) const
        {
            return &this->_data[i];
        }
    };

} // namespace diderot

#endif // !_DIDEROT_TENSOR_H_
