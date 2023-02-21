/*! \file diderot.h
 *
 * This header file pulls in all the various headers used in the Diderot library code.
 * It is meant to be included in the generated code produced by the Diderot compiler.
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_DIDEROT_H_
#define _DIDEROT_DIDEROT_H_

#include "base.h"
#include "util.h"
#include "options.h"
#include "tensor.h"
#include "image.h"
#include "dynseq.h"
#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
#  ifdef DIDEROT_NO_SPACE_PARTITION
#    include "brute-force-query.h"
#  else
#    include "kdtree.h"
#  endif // DIDEROT_NO_SPACE_PARTITION
#endif // DIDEROT_HAS_STRAND_COMMUNICATION
#include "world.h"
#include "eigen.h"

#ifdef DIDEROT_TARGET_PARALLEL
#include "parallel.h"
#endif

#ifdef DIDEROT_TARGET_CUDA
#include "gpu-select.cuh"

/* FIXME: should be named "CUDA_ERR" or "CUDA_CHECK", since it is a macro */
#define cuda_err(cuda_fn, str)                                                  \
    do {                                                                        \
        cudaError_t cudaErr = cuda_fn;                                          \
        if (cudaErr) {                                                          \
            std::cerr << (str) << ". Error " << cudaGetErrorName(cudaErr)       \
                << ": \"" << cudaGetErrorString(cudaErr) <<  "\" at "           \
                << __FILE__ << ":" << __LINE__  << std::endl;                   \
            return 1;                                                           \
        }                                                                       \
    } while (0)

#define cuda_err_passthrough(cuda_fn, str)                                      \
    do {                                                                        \
        cudaError_t cudaErr = cuda_fn;                                          \
        if (cudaErr) {                                                          \
            std::cerr << str << ". Error "<< cudaGetErrorName(cudaErr)          \
                << ": " << cudaGetErrorString(cudaErr) << std::endl;            \
            return cudaErr;                                                     \
        }                                                                       \
    } while (0)

#endif // DIDEROT_TARGET_CUDA

#endif // !_DIDEROT_DIDEROT_H_
