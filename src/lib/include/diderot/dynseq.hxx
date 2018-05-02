/*! \file dynseq.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_DYNSEQ_HXX_
#define _DIDEROT_DYNSEQ_HXX_

#include <memory>
#include <vector>
#include <initializer_list>
#include <teem/nrrd.h>

namespace diderot {

    namespace __details {
        template <typename T> using load_fn_ptr = T (*) (const void *);
    } // __details

    template <typename T> struct dynseq_traits;
    // using value_type = ...;
    // using base_type = ...;
    // static __details::load_fn_ptr<base_type> get_load_fn (int ty);
    // static const bool scalar = ...;
    // static const uint32_t values_per_elem = ...;

    template <typename T>
    class dynseq {
      public:

        using traits = dynseq_traits<T>;

      //! construct an empty sequence
        dynseq ();
      //! construct an uninitialized sequence with the given number of elements
        dynseq (uint32_t n);
      //! construct a sequence from a fixed-size array
        dynseq (uint32_t n, const T *data);
      //! construct a sequence from an array initializer (e.g., `dynseq{1, 2, 3}`)
        dynseq (std::initializer_list<T> il);
      //! construct a sequence for a range lo..hi (integer types only)
        dynseq (T const &lo, T const &hi);
      //! copy constructor; this just copies the reference
        dynseq (const dynseq &seq);
      //! copy constructor with additional space reserved
        dynseq (const dynseq &seq, uint32_t n);

        ~dynseq () { };

      // load a sequence from a nrrd or nrrd file; return true on error
        bool load (world_base *wrld, const Nrrd *nin);
        bool load (world_base *wrld, std::string const &file);

      // assign from an initializer list
        dynseq &operator= (std::initializer_list<T> il);

        uint32_t length () const { return this->_seq->size(); }

        T operator[] (uint32_t i) const { return (*this->_seq)[i]; }

        typename std::vector<T>::const_iterator cbegin() const noexcept
        {
            return this->_seq->cbegin();
        }
        typename std::vector<T>::const_iterator cend() const noexcept
        {
            return this->_seq->cend();
        }

      // destructive operations for adding elements to a sequence
        dynseq &prepend (T const &x);
        dynseq &prepend (const void *datap);
        dynseq &append (T const &x);
        dynseq &append (const void *datap);
        dynseq &concat (dynseq const &seq);

      // wrappers for the above operations
        static dynseq prepend (T const &x, dynseq<T> const &seq)
        {
            dynseq result(seq, 1);
            return result.prepend(x);
        }
        static dynseq append (dynseq const &seq, T const &x)
        {
            dynseq result(seq, 1);
            return result.append(x);
        }
        static dynseq concat (dynseq const &seq1, dynseq<T> const &seq2)
        {
            dynseq result(seq1, seq2.length());
            return result.concat(seq2);
        }

        void *data () const { return this->_seq->data(); }

      // copy the contents of the sequence to the specified address and return the address
      // immediately following the copied data
        char *copy_to (char *cp);

      private:
        std::shared_ptr<std::vector<T>> _seq;
    };

  /***** inline functions *****/

    template <typename T>
    inline dynseq<T>::dynseq ()
        : _seq(std::make_shared<std::vector<T>>(std::vector<T>()))
    { }

    template <typename T>
    inline dynseq<T>::dynseq (uint32_t nelems)
        : _seq(std::make_shared<std::vector<T>>(std::vector<T>(nelems)))
    { }

    template <typename T>
    inline dynseq<T>::dynseq (uint32_t n, const T *data)
        : _seq(std::make_shared<std::vector<T>>(std::vector<T>()))
    {
        this->_seq->reserve (n);
        for (int i = 0;  i < n;  i++) {
            this->_seq->push_back(data[i]);
        }
    }

    template <typename T>
    inline dynseq<T>::dynseq (std::initializer_list<T> il)
        : _seq(std::make_shared<std::vector<T>>(std::vector<T>(il)))
    { }

    template <typename T>
    inline dynseq<T>::dynseq (dynseq const &seq)
        : _seq(seq._seq)
    { }

} // namespace diderot

#include "dynseq-inst.hxx"

#endif // !_DIDEROT_DYNSEQ_HXX_
