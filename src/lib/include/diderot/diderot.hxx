/*! \file diderot.hxx
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

#ifndef _DIDEROT_DIDEROT_HXX_
#define _DIDEROT_DIDEROT_HXX_

#include "base.hxx"
#include "util.hxx"
#include "options.hxx"
#include "tensor.hxx"
#include "image.hxx"
#include "dynseq.hxx"
#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
#  ifdef DIDEROT_NO_SPACE_PARTITION
#    include "brute-force-query.hxx"
#  else
#    include "kdtree.hxx"
#  endif // DIDEROT_NO_SPACE_PARTITION
#endif // DIDEROT_HAS_STRAND_COMMUNICATION
#include "world.hxx"
#include "eigen.hxx"

#ifdef DIDEROT_TARGET_PARALLEL
#include "parallel.hxx"
#endif

#endif // !_DIDEROT_DIDEROT_HXX_
