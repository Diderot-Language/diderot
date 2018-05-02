(* operators-sig.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Abstract interface used to parameterize the intermediate representation
 * over the allowed operators.
 *)

signature OPERATORS =
  sig

    type ty

    type rator

    val resultArity : rator -> int              (* arity of results (usually 1) *)
    val arity : rator -> int                    (* operator arity *)
    val isPure : rator -> bool                  (* false for operations that have effects *)
    val same : rator * rator -> bool            (* equality test *)
    val hash : rator -> word                    (* hash key *)

    val toString : rator -> string

  end
