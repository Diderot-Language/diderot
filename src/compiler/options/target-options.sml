(* target-options.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Utility support for target options that are specified on the command line
 * (e.g., precision of the real type, target selection, etc.)
 *)

structure TargetOptions =
  struct

    datatype platform
      = SEQUENTIAL      (* sequential code for the CPU *)
      | PARALLEL        (* parallel code for the CPU *)
      | OPENCL          (* OpenCL *)
      | CUDA            (* CUDA *)
      | DEBUGGER        (* sequential library that can be run under the Diderot debugger *)

  (* the target descriptor collects together the information specified on the Diderot
   * compiler's command-line.
   *)
    type t = {
        srcFile : string,               (* source filename *)
        outDir : string,                (* directory to put output *)
        outBase : string,               (* basename for output or name of executable *)
        exec : bool,                    (* generate standalone executable? *)
        staticLib : bool,               (* true if exec is false and the generated library *)
                                        (* should be statically linked *)
        jsonAPI : bool,                 (* generate a JSON description of library API? *)
        snapshot : bool,                (* generate functions to get snapshot *)
        platform : platform,            (* target platform *)
        namespace : string,             (* optional namespace prefix *)
        double : bool,                  (* true for double-precision candidates *)
        longint : bool,                 (* true for 64-bit integers *)
        scalar : bool,                  (* true if scalar code (not SSE) should be generated *)
        runtimeLog : bool,              (* true if runtime event logging is enabled *)
        debug : bool,                   (* true if debugging of the target is enabled *)
        bsp : bool,                     (* true if BSP style execution should always be used *)
        kdtree : bool                   (* true if a kdtree is used to accelerate spatial queries *)
      }

    fun platformToString SEQUENTIAL = "SEQUENTIAL"
      | platformToString PARALLEL = "PARALLEL"
      | platformToString OPENCL = "OPENCL"
      | platformToString CUDA = "CUDA"
      | platformToString DEBUGGER = "DEBUGGER"

  end
