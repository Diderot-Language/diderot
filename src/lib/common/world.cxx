/*! \file world.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include <cstdlib>
#include "diderot/base.hxx"
#include "diderot/world.hxx"

#ifdef DIDEROT_TARGET_PARALLEL
#include "diderot/parallel.hxx"
#endif

namespace diderot {

    world_base::world_base (std::string const &name, bool isArr, int nAxes)
        : _name(name), _errors(biffMsgNew("DIDEROT")), _stage(POST_NEW),
          _verbose(false), _is_array(isArr), _naxes(nAxes),
          _base(new uint32_t[nAxes]), _size(new uint32_t[nAxes]),
          _printTo(nullptr), _run_time(0)
    {
#ifdef DIDEROT_TARGET_PARALLEL
        this->_sched = new diderot::scheduler();
#endif
#ifdef DIDEROT_ENABLE_LOGGING
        this->_log_file = nullptr;
#endif
    }

    world_base::~world_base ()
    {
        biffMsgNix (this->_errors);
        delete[] this->_base;
        delete[] this->_size;
        if (this->_printTo != nullptr) {
            this->_printTo->close();
            delete this->_printTo;
        }
#ifdef DIDEROT_TARGET_PARALLEL
        delete this->_sched;
#endif
#ifdef DIDEROT_ENABLE_LOGGING
        if (this->_log_file != nullptr) delete this->_log_file;
#endif
    }

    //! Check that a nrrd has the expected structure for loading into a dynamic sequence
    //! \param nin the nrrd to check
    //! \param nValuesPerElem the expected number of values per element
    //! \return true on error, false otherwise
    bool world_base::check_seq_nrrd (const Nrrd *nin, uint32_t nValuesPerElem)
    {
      // check the structure of the nrrd file
        if (nin->spaceDim != 0) {
            this->error ("expected nrrd-file space dimension of 0, but found %d\n",
                nin->spaceDim);
            return true;
        }

        if (nValuesPerElem == 1) {
          // check sequence of scalars
            if (nin->dim != 1) {
                this->error ("expected nrrd dimension of 1, but found %d\n", nin->dim);
                return true;
            }
        }
        else {
          // check sequence of aggregates
            if (nin->dim != 2) {
                this->error ("expected nrrd dimension of 2, but found %d\n", nin->dim);
                return true;
            }
            else if (nin->axis[0].size != nValuesPerElem) {
                this->error ("expected %d values per sequence element, but found %d\n",
                    nValuesPerElem, nin->axis[0].size);
                return true;
            }
        }

        if (__details::nrrd_type_info[nin->type].sizeb == 0) {
            this->error ("bogus element type %d in nrrd\n", nin->type);
            return true;
        }

        return false;
    }

    // load a nrrd from a file
    Nrrd *world_base::load_nrrd_file (std::string const &filename)
    {
      /* create a nrrd; at this point it is just an empty container */
        Nrrd *nin = nrrdNew();

      /* read in the nrrd from the file */
        if (nrrdLoad(nin, filename.c_str(), nullptr) != 0) {
            char *msg = biffGetDone(NRRD);
            biffMsgAdd (this->_errors, msg);
            std::free (msg);
            return nullptr;
        }

        return nin;

    }

    bool world_base::normalize_nrrd_meta_data (Nrrd *nin)
    {
#ifdef HAVE_NRRDMETADATANORMALIZE
        int lostMeasureFrame = 0;
        int sts = nrrdMetaDataNormalize (
                nin, nin, nrrdMetaDataCanonicalVersionAlpha,
                AIR_FALSE, AIR_FALSE, AIR_FALSE, 1.0, &lostMeasureFrame);
        if (sts != 0) {
          // propagate the error message from nrrdMetaDataNormalize
            char *msg = biffGetDone(NRRD);
            biffMsgAdd (this->_errors, msg);
            nrrdNuke (nin);
            return true;
        }
        else if (lostMeasureFrame != 0) {
// FIXME: we should figure out a mechanism to pass warnings back up the call chain.
            std::cerr << "WARNING: input data measurement frame lost" << std::endl;
        }
#else
#  error must have nrrdMetaDataNormalize
#endif

        return false;
    }

    void world_base::error (const char *fmt, ...)
    {
        char buf[1024];
        va_list ap;

    // NOTE: if there was a biffMsg function that worked with va_lists, then we
    // could avoid the buffer
        va_start (ap, fmt);
        vsnprintf (buf, sizeof(buf), fmt, ap);
        va_end (ap);

        biffMsgAdd (this->_errors, buf);
    }

} // namespace Diderot
