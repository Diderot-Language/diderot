(* targets.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure Targets : sig

    val get : TargetOptions.t -> {
            info : TreeIR.target_info,
            generate : CmdLineConstants.t * TreeIR.program -> unit
          }

  end = struct

    fun get (tgt : TargetOptions.t) = (case #platform tgt
           of TargetOptions.SEQUENTIAL => TargetCPU.target tgt
            | TargetOptions.PARALLEL => TargetCPU.target tgt
            | TargetOptions.DEBUGGER => TargetCPU.target tgt
            | TargetOptions.OPENCL => raise Fail "OpenCL not supported yet"
            | TargetOptions.CUDA => raise Fail "CUDA not supported yet"
          (* end case *))

  end
