(* domain-sig.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Abstract interface for data-flow analysis domains.
 *)

signature DOMAIN =
  sig

    structure IR : SSA
    type t

    val bottom : t
    val join : t list -> t
    val transfer : t * IR.node -> t

    val same : t * t -> bool

    val toString : t -> string

  end
