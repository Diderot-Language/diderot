/*! \file parallel.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_PARALLEL_HXX_
#define _DIDEROT_PARALLEL_HXX_

#ifndef DIDEROT_TARGET_PARALLEL
#  error "parallel.hxx included but target is not parallel"
#endif

#ifndef _DIDEROT_BASE_HXX_
#include "base.hxx"
#endif
#ifndef _DIDEROT_WORLD_HXX_
#include "world.hxx"
#endif

#include <atomic>

typedef std::atomic<uint32_t> atomic_uint32_t;

#include <pthread.h>
#include "leader-barrier.hxx"
#include "worker-gate.hxx"

namespace diderot {

    struct scheduler {
        using task = void (*)(void *);

        struct CACHE_ALIGN worker_info {
            uint16_t            _id;            //!< index into thread _info[] array
                                                // location info for worker (if affinity is
                                                // supported)
            uint16_t            _node;          //!< node id
            uint16_t            _core;          //!< core id for node
            uint16_t            _thd;           //!< thread id for core
            world_base          *_wrld;         //!< world pointer
            scheduler           *_sched;        //!< scheduler data pointer
            void                *_data;         //!< per-worker data pointer for current task
        };

                                           // information about the available CPUs
        int             _numHWNodes;            //!< \brief number of (possibly multicore) processors
        int             _numHWCores;            //!< \brief total number of (possibly
                                                //!  mulithreaded) cores
        int             _numHWThreads;          //!< \brief total number of hardware threads
        int             _numCoresPerNode;       //!< \brief number of cores per thread
        int             _numThdsPerCore;        //!< \brief number of threads per core

        uint32_t        _numWorkers;            //!< number of worker threads in the pool; this is
                                                //!  decremented as workers terminate
        pthread_t       *_workers;              //!< thread IDs of workers
        worker_info     *_info;                 //!< per-thread info structure
        task            _task;                  //!< holds current task for workers
        uint32_t        _workSize;              //!< size of a block of work
        bool            _done;                  //!< flag to signal that the program is done.

        pthread_mutex_t _prLock;                //!< lock to protect printing
        worker_gate     _gate;                  //!< gate for coordinator/worker synchronization
        leader_barrier  _bspBar;                //!< barrier to implement BSP semantics

        scheduler ();
        ~scheduler ();

      //! \brief get information about the host CPU configuration
      //! \return true if there is an error
        bool get_cpu_info (world_base *wrld);

      //! set the number of workers
        void set_num_workers (uint32_t nw);

      //! \brief create the pool of workers
      //! \param wrld the world
      //! \return true if there is an error
        bool create_workers (world_base *wrld);

      //! \brief shut down the workers
        void shutdown (world_base *wrld);

      //! \brief return the info for the specified worker
        worker_info &worker (uint32_t id) { return this->_info[id]; }

      //! set the node/cpu affinity of the calling thread
        bool set_affinity (uint32_t node, uint32_t cpu);

      //! get the node & cpu of the calling thread
        bool get_cpu (uint32_t *node, uint32_t *cpu);
    };

  // compute the size of scheduler blocks base on the number of workers and the
  // initial number of strands
    uint16_t sched_block_size (uint32_t nWorkers, uint32_t nStrands);

} // namespace diderot

#endif //! _DIDEROT_PARALLEL_HXX_
