(* operators.sml
 *
 * Names used for the various operators.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Operators =
  struct

  (* binary operators (in precedence order) *)
    val op_lt = Atom.atom "<"
    val op_lte = Atom.atom "<="
    val op_equ = Atom.atom "=="
    val op_neq = Atom.atom "!="
    val op_gte = Atom.atom ">="
    val op_gt = Atom.atom ">"
    val op_add = Atom.atom "+"
    val op_sub = Atom.atom "-"
    val op_mul = Atom.atom "*"
    val op_dot = Atom.atom "•"                  (* u2022 *)
    val op_cross = Atom.atom "×"                (* u00d7 *)
    val op_convolve = Atom.atom "⊛"             (* u229b *)
    val op_outer = Atom.atom "⊗"                (* u2297 *)
    val op_colon = Atom.atom ":"
    val op_div = Atom.atom "/"
    val op_mod = Atom.atom "%"
    val op_pow = Atom.atom "^"
    val op_at = Atom.atom "@"
    val op_compose = Atom.atom "∘"              (* u2218 *)

  (* assignment operators *)
    val asgn_add = Atom.atom "+="
    val asgn_sub = Atom.atom "-="
    val asgn_mul = Atom.atom "*="
    val asgn_div = Atom.atom "/="
    val asgn_mod = Atom.atom "%="
(* QUESTION: do we want ^= ? *)

  (* unary operators *)
    val op_neg = Atom.atom "unary -"
    val op_not = Atom.atom "!"
    val op_D = Atom.atom "∇"                    (* u2207 *)
    val op_Ddot = Atom.atom "∇•"                (* u2207, u2022 *)
    val op_Dotimes = Atom.atom "∇⊗"             (* u2207, u2297 *)
    val op_curl = Atom.atom "∇×"                (* u2207, u00d7 *)
    val op_norm = Atom.atom "|()|"

  end
