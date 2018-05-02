/*! \file leader-barrier.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_LEADER_BARRIER_HXX_
#define _DIDEROT_LEADER_BARRIER_HXX_


#ifdef DIDEROT_ENABLE_LOGGING
#include "logging.hxx"
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif

namespace diderot {

  //! this struct implements a a barrier synchronization where one of the threads
  //! is designated as a *leader*.  When all of the threads have blocked on the
  //! barrier, then the *leader* is woken.  The leader can then execute atomic
  //! code before releasing the other threads.
    struct CACHE_ALIGN leader_barrier {
        uint32_t        _nWorkers;      //!< number of threads in the pool (including the leader)
        uint32_t        _nWaiting;      //!< number of threads waiting
        pthread_mutex_t _lock;          //!< lock to protect this struct
        pthread_cond_t  _leaderWait;    //!< used by the leader to wait
        pthread_cond_t  _waiting;       //!< used by non-leader threads to wait

        leader_barrier ()
            : _nWaiting(0), _nWorkers(0)
        {
            pthread_mutex_init (&this->_lock, nullptr);
            pthread_cond_init (&this->_leaderWait, nullptr);
            pthread_cond_init (&this->_waiting, nullptr);
        }

        ~leader_barrier ()
        {
            pthread_mutex_destroy (&this->_lock);
            pthread_cond_destroy (&this->_leaderWait);
            pthread_cond_destroy (&this->_waiting);
        }

      // initialize the barrier for the given number of workers
        void init (uint32_t nw) { this->_nWorkers = nw; }

      // invoke this function to wait; the boolean argument is true for the leader
      // returns true if the calling thread is the leader
        bool wait (bool isLeader IF_LOGGING(, struct world_base *wrld, int32_t id ))
        {
            IF_LOGGING( LogLeaderBarrierWait(wrld, id+1); )
            pthread_mutex_lock (&this->_lock);
                this->_nWaiting++;
                if (isLeader) {
                    if (this->_nWaiting < this->_nWorkers) {
                        pthread_cond_wait (&this->_leaderWait, &this->_lock);
                    }
                }
                else {
                    if (this->_nWaiting == this->_nWorkers) {
                      // all workers are at the barrier, so wake up the leader
                        pthread_cond_signal (&this->_leaderWait);
                    }
                    pthread_cond_wait (&this->_waiting, &this->_lock);
                }
            pthread_mutex_unlock (&this->_lock);
            IF_LOGGING( LogLeaderBarrierResume(wrld, id+1); )

            return isLeader;
        }

      // wait until all workers are waiting and then signal them to go
        void release (IF_LOGGING( struct world_base *wrld, int32_t id ))
        {
            IF_LOGGING( LogLeaderBarrierRelease(wrld, id+1); )
            this->_nWaiting = 0;
            pthread_cond_broadcast (&this->_waiting);
        }

      // regular barrier wait (no leader)
        void all_wait (IF_LOGGING(struct world_base *wrld, int32_t id ))
        {
            IF_LOGGING( LogLeaderBarrierAllWait(wrld, id+1); )
            pthread_mutex_lock (&this->_lock);
                this->_nWaiting++;
                if (this->_nWaiting < this->_nWorkers) {
                    pthread_cond_wait (&this->_waiting, &this->_lock);
                }
                else {
                    this->_nWaiting = 0;
                    pthread_cond_broadcast (&this->_waiting);
                }
            pthread_mutex_unlock (&this->_lock);
            IF_LOGGING( LogLeaderBarrierAllResume(wrld, id+1); )
        }

    }; // struct leader_barrier

} // namespace diderot

#undef IF_LOGGING

#endif // !_DIDEROT_LEADER_BARRIER_HXX_
