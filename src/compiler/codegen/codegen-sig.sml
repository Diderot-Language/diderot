(* codegen-sig.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2023 The University of Chicago
 * All rights reserved.
 *)

signature CODEGEN =
  sig

   (* Generate standalone *)
   val exec : TargetSpec.t * CmdLineConstants.t * TreeIR.program -> unit

   (* Generate library functions *)
   val library : TargetSpec.t * CmdLineConstants.t * TreeIR.program -> unit

  end
