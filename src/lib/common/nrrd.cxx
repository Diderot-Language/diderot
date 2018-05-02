/*! \file nrrd.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include "diderot/base.hxx"

namespace diderot {

    namespace __details {

        struct nrrd_type_info nrrd_type_info[nrrdTypeLast] = {
                [nrrdTypeDefault] = {false, 0, "unknown"},      /*  0: signifies "determine output type for me" */
                [nrrdTypeChar] = {false, 1, "char"},            /*  1:   signed 1-byte integer */
                [nrrdTypeUChar] = {false, 1, "uchar"},          /*  2: unsigned 1-byte integer */
                [nrrdTypeShort] = {false, 2, "short"},          /*  3:   signed 2-byte integer */
                [nrrdTypeUShort] = {false, 2, "ushort"},        /*  4: unsigned 2-byte integer */
                [nrrdTypeInt] = {false, 4, "int"},              /*  5:   signed 4-byte integer */
                [nrrdTypeUInt] = {false, 4, "uint"},            /*  6: unsigned 4-byte integer */
                [nrrdTypeLLong] = {false, 8, "long"},           /*  7:   signed 8-byte integer */
                [nrrdTypeULLong] = {false, 8, "ulong"},         /*  8: unsigned 8-byte integer */
                [nrrdTypeFloat] = {true, 4, "float"},           /*  9:          4-byte floating point */
                [nrrdTypeDouble] = {true, 8, "double"},         /* 10:          8-byte floating point */
                [nrrdTypeBlock] = {false, 0, "block"},          /* 11: size user defined at run time; MUST BE LAST */
            };

    } // namespace __details

} // namespace diderot
