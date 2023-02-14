/*! \file logging.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include "diderot/base.h"
#include "diderot/log-file.h"
#include "diderot/parallel.h"

#ifndef DIDEROT_ENABLE_LOGGING
#  error expected DIDEROT_ENABLE_LOGGING to be defined
#endif

namespace diderot {

    namespace log {

        file::file (std::string const &name, const diderot::scheduler *sched)
            : _ofs(name), _nbuffers(sched->_numWorkers+1), _buffers(0)
        {
            if (! this->_ofs.good()) {
                std::cerr << "unable to open logging output file \"" << name << "\"\n";
                exit (1);
            }

          // initialize the per-worker buffers
            this->_buffers = new buffer* [this->_nbuffers];
            for (int i = 0;  i < this->_nbuffers;  i++) {
                this->_buffers[i] = new buffer;
                this->_buffers[i]->_worker = i;
                this->_buffers[i]->_next = 0;
                this->_buffers[i]->_seqNum = 0;
            }

          // initialize the file header
            file_hdr hdr;
            bzero (&hdr, sizeof(file_hdr));
            hdr._magic          = file_hdr::MAGIC;
            hdr._version[0]     = DIDEROT_LOG_VERSION_MAJOR;
            hdr._version[1]     = DIDEROT_LOG_VERSION_MINOR;
            hdr._version[2]     = DIDEROT_LOG_VERSION_PATCH;
            hdr._version[3]     = 0;  // DIDEROT_LOG_VERSION_FAT
            hdr._hdrSzB         = sizeof(file_hdr);
            hdr._eventSzB       = sizeof(event);
            hdr._bufSzB         = buffer::SIZEB;
            time_t tim = time(0);
            ctime_r (&tim, hdr._date);
            hdr._date[24]       = '\0';  /* zero out '\n' */
            set_time_stamp (&(hdr._startTime));
#if HAVE_MACH_ABSOLUTE_TIME
            hdr._tsKind         = TS_MACH_ABSOLUTE;
            strncpy(hdr._clockName, "mach_absolute_time", sizeof(hdr._clockName)-1);
            hdr._resolution     = 1;
#elif HAVE_CLOCK_GETTIME
            hdr._tsKind         = TS_TIMESPEC;
#ifdef CLOCK_MONOTONIC
            strncpy(hdr._clockName, "clock_gettime(CLOCK_MONOTONIC)", sizeof(hdr._clockName)-1);
#else
            strncpy(hdr._clockName, "clock_gettime(CLOCK_REALTIME)", sizeof(hdr._clockName)-1);
#endif
            struct timespec res;
#ifdef CLOCK_MONOTONIC
            clock_getres(CLOCK_MONOTONIC, &res);
#else
            clock_getres(CLOCK_REALTIME, &res);
#endif
            hdr._resolution     = res.tv_nsec;
#else
            hdr._tsKind         = TS_TIMEVAL;
            strncpy(hdr._clockName, "gettimeofday", sizeof(hdr._clockName)-1);
            hdr._resolution     = 1000;
#endif
            hdr._nNodes         = sched->_numHWNodes;
            hdr._nCores         = sched->_numHWCores;
            hdr._nWorkers       = sched->_numWorkers;

          // write the header block
            this->_ofs.write (reinterpret_cast<const char *>(&hdr), sizeof(hdr));
            this->_ofs.flush ();

          // initialize the mutex
            pthread_mutex_init (&this->_lock, nullptr);
        }

        file::~file ()
        {
          // flush out any remaining buffers,
            for (int i = 0;  i < this->_nbuffers;  i++) {
                if (this->_buffers[i]->_next > 0) {
                    this->output_buffer(this->_buffers[i]);
                }
            }

          // close the file
            this->_ofs.close();

          // free memory
            for (int i = 0;  i < this->_nbuffers;  i++) {
                delete[] this->_buffers[i];
            }
            delete[] this->_buffers;
        }

        void file::output_buffer (buffer *buf)
        {
            pthread_mutex_lock (&this->_lock);
              // write the buffer to the output file
                this->_ofs.write (reinterpret_cast<const char *>(buf), sizeof(buffer));
// TODO: check for I/O error
            pthread_mutex_unlock (&this->_lock);

          // reset buffer
            buf->_next = 0;
            buf->_seqNum++;
        }

    } // namespace log

} //namespace diderot
