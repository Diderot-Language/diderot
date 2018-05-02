(* strand-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Utility definitions for strands.
 *)

structure StrandUtil =
  struct

  (* method names *)
    datatype method_name
      = Start
      | Update
      | Stabilize

    fun nameToString Start = "Start"
      | nameToString Update = "Update"
      | nameToString Stabilize = "Stabilize"

  end
