(* strand-sets.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure StrandSets : sig

    datatype t = ALL | ACTIVE | STABLE

  (* least-upper-bound *)
    val join : t * t -> t

    val toString : t -> string
    val same : t * t -> bool
    val hash : t -> word

  end = struct

    datatype t = ALL | ACTIVE | STABLE

    fun join (ACTIVE, ACTIVE) = ACTIVE
      | join (STABLE, STABLE) = STABLE
      | join _ = ALL

    fun toString ALL = "all"
      | toString ACTIVE = "active"
      | toString STABLE = "stable"

    fun same (ALL, ALL) = true
      | same (ACTIVE, ACTIVE) = true
      | same (STABLE, STABLE) = true
      | same _ = false

    fun hash ALL = 0w13
      | hash ACTIVE = 0w17
      | hash STABLE = 0w23

  end
