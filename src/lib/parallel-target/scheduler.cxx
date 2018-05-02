/*! \file scheduler.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 */

#include "diderot/parallel.hxx"

#if defined (__APPLE__)
#  include <sys/sysctl.h>
#elif defined(HAVE__PROC_CPUINFO)
#  include <cstdio>  // for sscanf
#  include <fstream>
#  ifdef HAVE_LIBNUMA
#    include <numa.h>
#  endif
#endif
#include <errno.h>
#ifdef HAVE_LINUX_GETCPU_H
#include <linux/getcpu.h>
#endif
#ifdef HAVE_SYS_GETCPU
#include <unistd.h>
#include <sys/syscall.h>
#endif

#ifdef DIDEROT_ENABLE_LOGGING
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif

namespace diderot {

    bool scheduler::get_cpu_info (world_base *wrld)
    {
#if defined(HAVE__PROC_CPUINFO)
      /* Get information from /proc/cpuinfo.  The interesting
       * fields are:
       *
       *    processor       : <id>          # logical processor/thread id
       *    physical id     : <id>          # node id
       *    core id         : <id>          # core id (per node)
       *    cpu cores       : <n>           # number of cores per node
       *    siblings        : <n>           # number of execution units per node
       */
        std::ifstream cpuinfo("/proc/cpuinfo");
        if (cpuinfo.good()) {
            int maxProcId = 0, maxNodeId = 0, maxCoreId = 0, nCores = 0, nSibs = 0;
            char buf[1024];
            cpuinfo.getline (buf, sizeof(buf));
            while (cpuinfo.good()) {
                int tmp = 0;
                if (sscanf(buf, "processor : %d", &tmp) == 1) {
                    maxProcId = (tmp > maxProcId) ? tmp : maxProcId;
                }
                else if (sscanf(buf, "physical id : %d", &tmp) == 1) {
                    maxNodeId = (tmp > maxNodeId) ? tmp : maxNodeId;
                }
                else if (sscanf(buf, "core id : %d", &tmp) == 1) {
                    maxCoreId = (tmp > maxCoreId) ? tmp : maxCoreId;
                }
                else if (sscanf(buf, "cpu cores : %d", &tmp) == 1) {
                    if (tmp != 0) {
                        nCores = tmp;
                    } else if (tmp != nCores) {
                        wrld->error ("inconsistent core counts %d != %d\n",
                            nCores, tmp);
                        return true;
                    }
                }
                else if (sscanf(buf, "siblings : %d", &tmp) == 1) {
                    nSibs = (tmp > nSibs) ? tmp : nSibs;
                }
                cpuinfo.getline (buf, sizeof(buf));
            }
            cpuinfo.close();
            this->_numHWNodes = maxNodeId + 1;
            this->_numHWCores = (maxCoreId + 1) * this->_numHWNodes;
            this->_numHWThreads = maxProcId + 1;
          // some AMD processors (Bulldozer) report a number of cores that is half
          // the max core ID + 1.  In this case, we use the maxCoreId to compute the
          // number of cores, since these are not hyperthreaded processors.
            if (nCores < maxCoreId + 1) {
                nCores = maxCoreId + 1;
            }
            this->_numCoresPerNode = nCores;
            this->_numThdsPerCore = this->_numHWThreads / this->_numHWCores;
          // check consistency
            if (this->_numHWCores != this->_numHWNodes * nCores) {
                wrld->error ("inconsistent core counts: %d cores != %d nodes * %d cores/node\n",
                    this->_numHWCores, this->_numHWNodes, nCores);
                return true;
            }
          // set default number of workers
            this->_numWorkers = this->_numHWCores;

            return false;
        }
        else {
            wrld->error ("unable to determine the number of processors\n");
            return true;
        }
#elif defined(__APPLE__)
        size_t      len = sizeof(int);

      /* the number of nodes */
        if (sysctlbyname("hw.packages", &(this->_numHWNodes), &len, 0, 0) < 0) {
            if (errno == ENOENT) {
              // "hw.packages" is not known
                this->_numHWNodes = 1;
            }
            else {
                wrld->error ("unable to determine the number of nodes\n");
                return true;
            }
        }

      /* the number of cores */
        if (sysctlbyname("hw.physicalcpu", &(this->_numHWCores), &len, 0, 0) < 0) {
            wrld->error ("unable to determine the number of physical CPUs\n");
            return true;
        }

      /* the number of hardware threads */
        if (sysctlbyname("hw.logicalcpu", &(this->_numHWThreads), &len, 0, 0) < 0) {
            if (errno == ENOENT) {
              // "hw.packages" is not known
                this->_numHWThreads = this->_numHWCores;
            }
            else {
                wrld->error ("unable to determine the number of logical CPUs\n");
                return true;
            }
        }

        this->_numCoresPerNode = this->_numHWCores / this->_numHWNodes;
        this->_numThdsPerCore = this->_numHWThreads / this->_numHWCores;

      // set default number of workers
        this->_numWorkers = this->_numHWCores;

        return false;
#else
        this->_numWorkers = 1;
        return true;
#endif
    }

  // set the node/cpu affinity of the calling thread
    bool scheduler::set_affinity (uint32_t node, uint32_t cpu)
    {
     // set thread affinity for worker (if supported)
#ifdef HAVE_PTHREAD_SETAFFINITY_NP
        cpu_set_t       cpus;
        CPU_ZERO(&cpus);
        uint32_t cpuId = node;
        cpuId = (this->_numCoresPerNode * cpuId) + cpu;
        cpuId = this->_numThdsPerCore * cpuId;
      // allow worker to run on any thead
        for (int i = 0;  i < this->_numThdsPerCore;  i++) {
            CPU_SET(cpuId+i, &cpus);
        }
        if (pthread_setaffinity_np (pthread_self(), sizeof(cpu_set_t), &cpus) == -1) {
            return true;
        }
#elif HAVE_LIBNUMA
        if (numa_run_on_node (node) == -1) {
            return true;
        }
#endif
        return false;
    }

  // get the node & cpu of the calling thread
    bool scheduler::get_cpu (uint32_t *nodeRet, uint32_t *cpuRet)
    {
#if defined(HAVE_GETCPU)
        unsigned cpu, node;
        if (getcpu(&cpu, &node, nullptr) == 0) {
            *nodeRet = node;
            *cpuRet = cpu;
            return false;
        }
        else {
            return true;
        }
#elif HAVE_SYS_GETCPU
        unsigned cpu, node;
        if (syscall(SYS_getcpu, &cpu, &node, nullptr) == 0) {
            *nodeRet = node;
            *cpuRet = cpu;
            return false;
        }
        else {
            return true;
        }
#elif defined(HAVE_SCHED_GETCPU)
        int cpu = sched_getcpu();
        if (cpu >= 0) {
            *nodeRet = cpu / (this->_numCoresPerNode * this->_numThdsPerCore);
            *cpuRet = cpu;
            return false;
        }
        else {
            return true;
        }
#else
        return true;  // no info
#endif
    }

    static void *worker_main (void *data)
    {
        auto            *myInfo = static_cast<scheduler::worker_info *>(data);
        scheduler       *sched = myInfo->_sched;

      // set thread affinity for worker (if supported)
        if (sched->set_affinity(myInfo->_node, myInfo->_core)) {
            myInfo->_wrld->error("Warning: unable to set affinity for worker %d on cpu %d.%d\n",
                myInfo->_id, myInfo->_node, myInfo->_core);
        }

      // report this worker's location in verbose mode
        if (myInfo->_wrld->_verbose) {
            uint32_t cpu, node;
            if (! sched->get_cpu(&node, &cpu)) {
                pthread_mutex_lock (&sched->_prLock);
                    std::cout << "** Worker" << myInfo->_id << " @ " << node << "." << cpu
                        << "; requested " << myInfo->_node << "." <<  myInfo->_core << "\n";
                pthread_mutex_unlock (&sched->_prLock);
            }
        }

        IF_LOGGING( LogWorkerStart(myInfo->_wrld, myInfo->_id+1); )

        while (true) {
          // wait until all workers and the coordinator are ready
            sched->_gate.worker_wait (IF_LOGGING( myInfo->_wrld, myInfo->_id ));
          // check for termination
            if (sched->_done) {
                IF_LOGGING( LogWorkerExit(myInfo->_wrld, myInfo->_id+1); )
                pthread_exit (nullptr);
            }
          // run the task
            assert (sched->_task != nullptr);
            sched->_task (myInfo->_data);
        }

    } // worker_main

    scheduler::scheduler ()
        : _numHWNodes(0), _numHWCores(0), _numHWThreads(0), _numCoresPerNode(0),
          _numThdsPerCore(0), _numWorkers(0), _workers(nullptr), _info(nullptr),
          _task(nullptr), _workSize(0), _done(false)
    {
        pthread_mutex_init (&this->_prLock, nullptr);
      // put the controller on node 0/cpu 0
        this->set_affinity(0, 0);
    }

    scheduler::~scheduler ()
    {
        pthread_mutex_destroy (&this->_prLock);
        delete[] this->_workers;
        delete[] this->_info;
    }

    void scheduler::set_num_workers (uint32_t nw)
    {
        this->_numWorkers = (nw < 1) ? 1 : ((this->_numHWThreads < nw) ? this->_numHWThreads : nw);
    }

    bool scheduler::create_workers (world_base *wrld)
    {
        if (this->_info != nullptr) {
            wrld->error ("attempt to create workers when workers already exist\n");
            return true;
        }

        IF_LOGGING( LogSchedulerStart(wrld, 0); )

      // initialize the barriers
        this->_gate.init (this->_numWorkers);
        this->_bspBar.init (this->_numWorkers);

      // allocate space for thread IDs and thread info
        this->_workers = new pthread_t[this->_numWorkers];
        this->_info = new worker_info[this->_numWorkers];

#if defined(HAVE_SCHED_SETAFFINITY) || defined(HAVE_LIBNUMA)
      // set the desired worker locations
        int workersPerNode = (this->_numWorkers + this->_numHWNodes - 1) / this->_numHWNodes;
        uint32_t wid = 0;
        for (uint32_t i = 0;  i < this->_numHWNodes;  i++) {
            for (uint32_t j = 0;  j < workersPerNode;  j++) {
                if (wid < this->_numWorkers) {
                    this->_info[wid]._node = i;
                    this->_info[wid]._core = j % this->_numCoresPerNode;
                    this->_info[wid]._thd = 0;
                    wid++;
                }
                else {
                    break;
                }
            }
        }
#endif

      // start the worker threads
        for (uint32_t i = 0;  i < this->_numWorkers;  i++) {
            this->_info[i]._id = i;
            this->_info[i]._wrld = wrld;
            this->_info[i]._sched = this;
#if !(defined(HAVE_SCHED_SETAFFINITY) || defined(HAVE_LIBNUMA))
          // no affinity support
            this->_info[i]._node = 0;
            this->_info[i]._core = 0;
            this->_info[i]._thd = 0;
#endif
            int sts = pthread_create (
                &this->_workers[i],
                nullptr,
                worker_main,
                &this->_info[i]);
            if (sts != 0) {
                wrld->error ("unable to create worker thread; err = %d\n", sts);
                return true;
            }
        }

      // wait for workers to be ready to go
        this->_gate.controller_wait (IF_LOGGING( wrld ));

        return false;
    }

    void scheduler::shutdown (world_base *wrld)
    {
        this->_done = true;
      // synchronize with workers
        this->_gate.release_workers (IF_LOGGING( wrld ));
      // wait for them to terminate
        void *dummy;
        for (uint32_t i = 0;  i < this->_numWorkers;  i++) {
            int sts = pthread_join (this->_workers[i], &dummy);
            if (sts != 0) {
                std::cerr << "error attempting to join with worker " << i
                    << " (id: " << this->_workers[i] << "); error code = "
                    << sts << std::endl;
            }
        }
        this->_numWorkers = 0;
        IF_LOGGING( LogSchedulerShutdown(wrld, 0); )
    }

  // compute the size of scheduler blocks base on the number of workers and the
  // initial number of strands.
  // Note that the result is 16 bits!!
    uint16_t sched_block_size (uint32_t nWorkers, uint32_t nStrands)
    {
        assert (nWorkers > 0);
        uint32_t strandsPerWorker = nStrands / nWorkers;
      // compute minimum power of two less than strandsPerWorker and greater than
      // the minimum block size (64)
        uint32_t blkSz = 64;
        while (blkSz < strandsPerWorker) { blkSz += blkSz; }
      // we would like there to be a reasonable number of blocks per worker (16), so
      // shrink the blk size while keeping it above the minimum
        for (int i = 0;  (64 < blkSz) && (i < 4);  i++) {
            blkSz >>= 1;
        }
      // lastly, we don't want blocks larger than the maximum block size
        if (blkSz > 4096) {
            blkSz = 4096;
        }
        return blkSz;
    }

} // namepace diderot
