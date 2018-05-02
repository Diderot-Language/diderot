/*! \file log-file.hxx
 *
 * \author John Reppy
 *
 * A relatively light-weight logging system for tracking parallel performance
 * in Diderot executables.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * This code was ported from the Manticore project (http://manticore.cs.uchicago.edu)
 */

#ifndef _DIDEROT_LOG_FILE_HXX_
#define _DIDEROT_LOG_FILE_HXX_

#include "config.h"
#if defined(HAVE_MACH_ABSOLUTE_TIME)
#  include <mach/mach_time.h>
#elif defined(HAVE_CLOCK_GETTIME)
#  include <time.h>
#else
#  include <sys/time.h>
#endif
#include <fstream>
#include <pthread.h>

/* define the predefined log-event codes */
#include "log-events.hxx"

namespace diderot {

    struct scheduler;

    namespace log {

      // different formats of timestamps
        enum {
            TS_TIMEVAL,                 // struct timeval returned by gettimeofday
            TS_TIMESPEC,                // struct timespec returned by clock_gettime
            TS_MACH_ABSOLUTE            // uint64_t returned by mach_absolute_time
        };

      // time values for TS_TIMEVAL or TS_TIMESPEC
        struct time_value {
            int32_t    _sec;            // whole seconds
            int32_t    _frac;           // fractional seconds (either uSec or nSec)
        };

        union time_stamp {
            time_value  _tv;            // TS_TIMEVAL or TS_TIMESPEC
            uint64_t    _mach;          // TS_MACH_ABSOLUTE
        };

      // WARNING: the following struct needs to have the same layout in 64-bit
      // and 32-bit builds, so be careful about alignment and make sure that
      // the total size is a multiple of 8 bytes.
      //
        struct file_hdr {
            static const uint64_t MAGIC = 0x44696465726F7458;   // "DiderotX"

            uint64_t    _magic;         // [  0] to identify log files
            uint8_t     _version[4];    // [  8] major:minor:patch:fat?
            uint32_t    _hdrSzB;        // [ 12] size of this struct
            uint32_t    _eventSzB;      // [ 16] size of the event struct
            uint32_t    _bufSzB;        // [ 20] size of buffer struct
            time_stamp  _startTime;     // [ 24] start time for run
            char        _date[32];      // [ 32] the date of the run (as reported by ctime(3))
            uint32_t    _tsKind;        // [ 64] timestamp format
            char        _clockName[32]; // [ 68] a string describing the clock
            uint32_t    _resolution;    // [100] clock resolution in nanoseconds
            uint32_t    _nNodes;        // [104] number of nodes
            uint32_t    _nCores;        // [108] number of cores
            uint32_t    _nWorkers;      // [112] number of worker threads
                                        // [114]
        };

      // an event in the log file; usually these are 16-bytes, but if DIDEROT_FAT_LOG_EVENTS
      // is set, then we add 16 bytes more space for extra data
        struct event {
            time_stamp  _ts;            // [ 0] 8-byte time stamp
            uint32_t    _strand;        // [ 8] internal strand ID
            uint16_t    _event;         // [12] event code
#ifdef DIDEROT_FAT_LOG_EVENTS
            uint16_t    _data[9];       // [16] 18 bytes of event-specific data
#else
            uint16_t    _pad;           // [14] padding to 16 bytes
#endif
        };

      // a per-worker buffer of events
        struct buffer {
            static const size_t SIZEB = 8*1024;
            static const size_t NUM_EVENTS = (SIZEB / sizeof(event)) - 1;

            uint32_t    _worker;        // ID+1 of worker that owns this buffer; 0 for controller
            uint32_t    _next;          // next entry to use in the log[] array
            uint32_t    _seqNum;        // sequence number of this buffer
            char        _pad[sizeof(event) - 12];
            event       _log[NUM_EVENTS];
        };

#ifndef LOG_VIEWER

      // the log output file (for the Diderot runtime system only)
      //
        class file {
          public:
            file (std::string const &name, const diderot::scheduler *sched);
            ~file ();

          /*! \brief get the pointer to the next log entry for the worker
           *  \param worker the worker
           *  \return the address of the next log entry.
           */
            event *next_event (uint32_t id)
            {
                buffer *buf = this->_buffers[id];
                do {
                    int index = buf->_next;
                    if (index < buffer::NUM_EVENTS) {
                        buf->_next++;
                        return &(buf->_log[index]);
                    }
                    this->output_buffer (buf);
                 } while (true);
            }

          private:
            std::ofstream       _ofs;           // output file
            int                 _nbuffers;      // number of buffers (== # workers + 1)
            buffer              **_buffers;     // log buffers; 0 is for controller, 1 is for
                                                // worker 0, etc.
            pthread_mutex_t     _lock;          // lock used to protect I/O operations

            void output_buffer (buffer *buf);
        };

#endif // !LOG_VIEWER

    } // namespace log

} // namespace diderot

#endif // !_DIDEROT_LOG_FILE_HXX_
