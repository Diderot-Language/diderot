/*! \file tensor.hxx
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

#ifndef _DIDEROT_TENSOR_HXX_
#define _DIDEROT_TENSOR_HXX_

namespace diderot {

    template <typename REAL, const int N>
    struct tensor_ref {
        const REAL *_data;
        tensor_ref () : _data(nullptr) { }
        tensor_ref (const REAL *src) : _data(src) { }
        REAL const &operator[] (uint32_t i) const
        {
            return this->_data[i];
        }
        const REAL *base ()
        {
            return this->_data;
        }
        const REAL *addr (uint32_t i = 0) const
        {
            return &this->_data[i];
        }
    };

    template <typename REAL, const int N>
    struct tensor {
        REAL _data[N];
        tensor () { }
        tensor (std::initializer_list< REAL > const &il)
        {
            int32_t i = 0;
            for (auto it = il.begin(); it != il.end(); ++i, ++it) {
                this->_data[i] = *it;
            }
        }
        tensor (const REAL *src)
        {
            std::memcpy(this->_data, src, N*sizeof(REAL));
        }
        ~tensor () { }
        void copy (const REAL *src)
        {
            std::memcpy(this->_data, src, N*sizeof(REAL));
        }
        void copy (std::initializer_list< REAL > const &il)
        {
            int32_t i = 0;
            for (auto it = il.begin(); it != il.end(); ++i, ++it) {
                this->_data[i] = *it;
            }
        }
        REAL &operator[] (uint32_t i)
        {
            return this->_data[i];
        }
        REAL const &operator[] (uint32_t i) const
        {
            return this->_data[i];
        }
        REAL *base ()
        {
            return &this->_data[0];
        }
        const REAL *base () const
        {
            return &this->_data[0];
        }
        const REAL *addr (uint32_t i = 0) const
        {
            return &this->_data[i];
        }
    };

} // namespace diderot

#endif // !_DIDEROT_TENSOR_HXX_
