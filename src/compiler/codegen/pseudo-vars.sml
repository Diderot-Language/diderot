(* pseudo-vars.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PseudoVars =
  struct

  (* TreeIR "variables" that are used to get the names needed to access the
   * global and strand state variables.  These are just used as keys to lookup
   * the C names in the environment, so their type is irrelevant.
   *)
    local
      fun new name = TreeVar.new (name, TreeTypes.IntTy)
    in
    val selfLocal = new "$selfLocal"    (* access for invariant and non-shared state variables *)
    val selfIn = new "$selfSharedIn"    (* access for shared input state variables *)
    val selfOut = new "$selfSharedOut"  (* access for shared output state variables *)
    val global = new "$global"
    val world = new "$world"
    end (* local *)

  end
