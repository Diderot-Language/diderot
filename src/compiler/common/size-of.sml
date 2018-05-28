(* size-of.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * src/compiler/common/size-of.sml.  Generated from size-of_sml.in by configure.
 *
 * Sizes of various C types that the compiler needs to know about.
 *)

structure SizeOf =
  struct

  (* C representation of booleans; we include this definition in this module because
   * it defined by the configuration.
   *)
    val c_bool = "bool"

  (* Sizes of standard C/C++ types *)
    val c_int : Word.word = 0w4
    val c_long : Word.word = 0w8
    val c_float : Word.word = 0w4
    val c_double : Word.word = 0w8

  (* Sizes of host-side OpenCL types; these will be 0 if OpenCL is not supported *)
    val cl_int : Word.word = 0w0
    val cl_long : Word.word = 0w0
    val cl_float : Word.word = 0w0
    val cl_double : Word.word = 0w0

  end
