/*! \file barrier.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_BARRIER_HXX_
#define _DIDEROT_BARRIER_HXX_

#ifdef DIDEROT_ENABLE_LOGGING
#include "logging.hxx"
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif

namespace diderot {

  // abstract interface for barrier synchronization
    class CACHE_ALIGN barrier {
      public:
        barrier ();
        ~barrier ();
        bool init (uint32_t numThreads);
        void wait (IF_LOGGING( struct world_base *wrld, int32_t id ));

      private:
#if defined(HAVE_PTHREAD_BARRIER_INIT)
        pthread_barrier_t       _barrier;
#elif defined(FAST_SYNCHRONIZATION)

#  error TODO

#else
        uint32_t                _nThreads;
        uint32_t                _nWaiting;
        pthread_mutex_t         _lock;
        pthread_cond_t          _waiting;
#endif
    };

#if defined(HAVE_PTHREAD_BARRIER_INIT)
    inline barrier::barrier ()
    { }

    inline barrier::~barrier ()
    {
        pthread_barrier_destroy (&this->_barrier);
    }

    inline bool barrier::init (uint32_t numThreads)
    {
        pthread_barrier_init (&this->_barrier, nullptr, numThreads);
    }

    inline void barrier::wait (IF_LOGGING(struct world_base *wrld, int32_t id))
    {
        IF_LOGGING( LogBarrierWait(wrld, id+1); )
        pthread_barrier_wait (&this->_barrier);
        IF_LOGGING( LogBarrierResume(wrld, id+1); )
    }

#elif defined(FAST_SYNCHRONIZATION)

#  error TODO

#else
    inline barrier::barrier ()
        : _nThreads(0), _nWaiting(0)
    {
        pthread_mutex_init (&this->_lock, nullptr);
        pthread_cond_init (&this->_waiting, nullptr);
    }

    inline barrier::~barrier ()
    {
        pthread_mutex_destroy (&this->_lock);
        pthread_cond_destroy (&this->_waiting);
    }

    inline bool barrier::init (uint32_t numThreads)
    {
        assert (this->_nWaiting == 0);
        this->_nThreads = numThreads;
        return false;
    }

    inline void barrier::wait (IF_LOGGING( struct world_base *wrld, int32_t id ))
    {
        IF_LOGGING( LogBarrierWait(wrld, id+1); )
        pthread_mutex_lock (&this->_lock);
            if (++(this->_nWaiting) == this->_nThreads) {
                this->_nWaiting = 0;
                pthread_cond_broadcast (&this->_waiting);
            }
            else {
                pthread_cond_wait (&this->_waiting, &this->_lock);
            }
        pthread_mutex_unlock (&this->_lock);
        IF_LOGGING( LogBarrierResume(wrld, id+1); )
    }
#endif

} // namespace diderot

#undef IF_LOGGING

#endif //! _DIDEROT_BARRIER_HXX_
