/*! \file world.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_WORLD_HXX_
#define _DIDEROT_WORLD_HXX_

#include <iostream>
#include <teem/biff.h>
#include <teem/nrrd.h>

namespace diderot {

#ifdef DIDEROT_ENABLE_LOGGING
    namespace log { class file; }
#endif

    // stages of execution of a program
    //
    enum execution_state {
        POST_NEW = 0,                       // world allocated, but not initialized
        POST_INIT = 1,                      // Init function has been called
        POST_CREATE = 2,                    // globals and strands have been initialized
        RUNNING = 3,                        // program is running
        DONE = 4                            // program has terminated
    };

    // the base struct for the world type
    //
    struct world_base {
        std::string         _name;          // the program name
        biffMsg             *_errors;       // holds error messages (if any)
        execution_state     _stage;         // current stage of program execution
        bool                _verbose;       // true if running in verbose mode
        bool                _is_array;      // are the strands in an array or collection?
        uint32_t            _naxes;         // depth of iteration nesting
        uint32_t            *_base;         // nAxes array of base indices
        uint32_t            *_size;         // nAxes array of iteration sizes
        std::ofstream       *_printTo;      // file to direct printed output to (default is cout).
        double              _run_time;      // total time in seconds spent running Diderot code
#ifdef DIDEROT_TARGET_PARALLEL
        struct scheduler    *_sched;        // scheduler info for parallel target
#endif
#ifdef DIDEROT_ENABLE_LOGGING
        log::file           *_log_file;     // log file for recording events
#endif

        world_base (std::string const &name, bool isArr, int nAxes);
        ~world_base ();

        std::ostream & print()
        {
            return (this->_printTo == nullptr) ? std::cout : *this->_printTo;
        }

        void error (const char *fmt, ...);

        const char *get_errors ()
        {
            return biffMsgStrGet(this->_errors);
        }

      //! Load a nrrd file into memory
      //! \param filename the path to the nrrd file
      //! \return the pointer to the nrrd file
        Nrrd *load_nrrd_file (std::string const &filename);

      //! Check that a nrrd has the expected structure for loading into a dynamic sequence
      //! \param nin the nrrd to check
      //! \param nValuesPerElem the expected number of values per element
      //! \return true on error, false otherwise
        bool check_seq_nrrd (const Nrrd *nin, uint32_t nValuesPerElem);

      //! Normalize the meta-data of a nrrd
      //! return true on error, false otherwise
        bool normalize_nrrd_meta_data (Nrrd *nin);

    }; // struct world_base

} // namespace diderot

#endif // !_DIDEROT_WORLD_HXX_
