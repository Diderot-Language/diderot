/*! \file dynseq-inst.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_DYNSEQ_INST_HXX_
#define _DIDEROT_DYNSEQ_INST_HXX_

#ifndef _DIDEROT_DYNSEQ_HXX_
# error dynseq-inst.hxx should not be directly included
#endif

namespace diderot {

  //! construct a sequence for a range lo..hi (integer types only)
    template <typename T>
    dynseq<T>::dynseq (T const &lo, T const &hi)
        : _seq(std::make_shared<std::vector<T>>(std::vector<T>()))
    {
        if (lo <= hi) {
            this->_seq->reserve (hi - lo + 1);
            for (T i = lo;  i <= hi;  ++i) {
                this->_seq->push_back(i);
            }
        }
    }

  //! copy constructor with additional space reserved
    template <typename T>
    dynseq<T>::dynseq (const dynseq &seq, uint32_t n)
        : _seq(std::make_shared<std::vector<T>>(std::vector<T>(*(seq._seq))))
    {
        this->_seq->reserve (n);
    }

    //! return the contents of a nrrd as a dynamic sequence
    //! \param wrld the world; used to report errors
    //! \param nin the source nrrd to copy
    //! \param sts error status; set to true if there was an error
    template <typename T>
    bool dynseq<T>::load (world_base *wrld, const Nrrd *nin)
    {
        uint32_t nValuesPerElem = dynseq<T>::traits::values_per_elem;
        size_t baseSz = sizeof(typename dynseq<T>::traits::base_type);
        size_t elemSz = baseSz * nValuesPerElem;

        if (wrld->check_seq_nrrd (nin, nValuesPerElem)) {
            return true;
        }

//          if (dynseq<T>::traits::check_type(nin->type)) {
//              wrld->error ("element type mismatch in nrrd file");
//              sts = true;
//              return dynseq<T>();
//          }

      // get the number of elements
        size_t nElems;
        if (nValuesPerElem == 1) {
            nElems = nin->axis[0].size;
        }
        else {
            nElems = nin->axis[1].size;
        }

      // allocate the result sequence object
        this->_seq->resize(nElems);

      // initialize the sequence from the nrrd
        typename dynseq<T>::traits::base_type *dst =
            reinterpret_cast<typename dynseq<T>::traits::base_type *>(this->data());

        if (__details::nrrd_type_info[nin->type].sizeb != baseSz) {
          // this is the slow path; we have to convert values as they are copied from the nrrd
            __details::load_fn_ptr<typename dynseq<T>::traits::base_type> loadFn =
                dynseq<T>::traits::load_fn_tbl[nin->type];
            char *src = (char *)nin->data;
            size_t srcElemSz = __details::nrrd_type_info[nin->type].sizeb;
            for (size_t i = 0;  i < nElems * nValuesPerElem;  i++) {
                *dst++ = loadFn(src);
                src += srcElemSz;
            }
        }
        else {
          // this is the fast path, where we can just do a bulk memcpy
            std::memcpy (dst, nin->data, nElems * elemSz);
        }

        return false;

    }

    template <typename T>
    bool dynseq<T>::load (world_base *wrld, std::string const &file)
    {
        Nrrd *nin = wrld->load_nrrd_file (file);
        if (nin == nullptr) {  // there was an error, so we return the empty sequence
            return true;
        }
        else {
            this->load (wrld, nin);
            nrrdNuke(nin);
            return false;
        }
    }

    template <typename T>
    dynseq<T> &dynseq<T>::operator= (std::initializer_list<T> il)
    {
        std::shared_ptr<std::vector<T>> _seq;

        this->_seq->clear();
        this->_seq->reserve(il.size());
        for (auto it = il.begin();  it != il.end();  ++it) {
            this->_seq->push_back (*it);
        }
        return *this;
    }

    template <typename T>
    dynseq<T> &dynseq<T>::prepend (T const &x)
    {
        this->_seq->insert (this->_seq->begin(), x);
        return *this;
    }

    template <typename T>
    dynseq<T> &dynseq<T>::prepend (const void *datap)
    {
        this->_seq->insert (this->_seq->begin(), T());
        std::memcpy (this->_seq->data(), datap, sizeof(T));
        return *this;
    }

    template <typename T>
    dynseq<T> &dynseq<T>::append (T const &x)
    {
        this->_seq->push_back(x);
        return *this;
    }

    template <typename T>
    dynseq<T> &dynseq<T>::append (const void *datap)
    {
        size_t nbytes = this->_seq->size() * sizeof(T); // before adding space for element!
        this->_seq->push_back(T());
        std::memcpy (reinterpret_cast<char *>(this->_seq->data())+nbytes, datap, sizeof(T));
        return *this;
    }

    template <typename T>
    dynseq<T> &dynseq<T>::concat (dynseq<T> const &seq)
    {
        this->_seq->reserve (this->length() + seq.length());
        for (auto it = seq.cbegin(); it != seq.cend(); ++it) {
            this->_seq->push_back (*it);
        }
        return *this;
    }

    template <typename T>
    char *dynseq<T>::copy_to (char *cp)
    {
        size_t nbytes = this->_seq->size() * sizeof(T);
        std::memcpy (cp, this->_seq->data(), nbytes);
        return cp + nbytes;
    }

  // printer for dynamic sequences
    template <typename T>
    static std::ostream& operator<< (std::ostream &outs, dynseq<T> const &seq)
    {
        outs << "{";
        size_t n = seq.length();
        if (n > 0) {
            outs << seq[0];
            for (size_t i = 1;  i < n;  i++) {
                outs << "," << seq[i];
            }
        }
        outs << "}";
        return outs;
    }

} // namespace diderot

#endif // !_DIDEROT_DYNSEQ_INST_HXX_
