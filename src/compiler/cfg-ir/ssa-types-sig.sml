(* ssa-types-sig.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

signature SSA_TYPES =
  sig

    type ty

    val same : ty * ty -> bool
    val hash : ty -> word
    val toString : ty -> string

  (* common types *)
    val BoolTy : ty
    val StringTy : ty
    val intTy : ty
    val realTy : ty

  end

