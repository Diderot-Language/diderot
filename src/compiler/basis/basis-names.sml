(* basis-names.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Builtin names used for Basis functions.
 *)

structure BasisNames =
  struct

  (* import the operator names from parse-tree/operators.sml *)
    open Operators

  (* reduction operators *)
    val fn_all = Atom.atom "all"
    val fn_exists = Atom.atom "exists"
    val fn_max = Atom.atom "max"                (* also used as a regular function *)
    val fn_mean = Atom.atom "mean"
    val fn_min = Atom.atom "min"                (* also used as a regular function *)
    val fn_product = Atom.atom "product"
    val fn_sum = Atom.atom "sum"
    val fn_variance = Atom.atom "variance"

  (* function names *)
    val fn_border = Atom.atom "border"
    val fn_clamp = Atom.atom "clamp"
    val fn_det = Atom.atom "det"
    val fn_evecs = Atom.atom "evecs"
    val fn_evals = Atom.atom "evals"
    val fn_inside = Atom.atom "inside"
    val fn_inv = Atom.atom "inv"
    val fn_length = Atom.atom "length"
    val fn_lerp = Atom.atom "lerp"
    val fn_clerp = Atom.atom "clerp"
    val fn_max = Atom.atom "max"
    val fn_min = Atom.atom "min"
    val fn_mirror = Atom.atom "mirror"
    val fn_modulate = Atom.atom "modulate"
    val fn_normalize = Atom.atom "normalize"
    val fn_numActive = Atom.atom "numActive"
    val fn_numStable = Atom.atom "numStable"
    val fn_numStrands = Atom.atom "numStrands"
    val fn_size = Atom.atom "size"
    val fn_trace = Atom.atom "trace"
    val fn_transpose = Atom.atom "transpose"
    val fn_wrap = Atom.atom "wrap"
    val fn_concat = Atom.atom "concat"

  (* standard math functions *)
    val fn_acos = Atom.atom "acos"
    val fn_asin = Atom.atom "asin"
    val fn_atan = Atom.atom "atan"
    val fn_atan2 = Atom.atom "atan2"
    val fn_ceil = Atom.atom "ceil"
    val fn_cos = Atom.atom "cos"
    val fn_erf = Atom.atom "erf"
    val fn_erfc = Atom.atom "erfc"
    val fn_exp = Atom.atom "exp"
    val fn_floor = Atom.atom "floor"
    val fn_fmod = Atom.atom "fmod"
    val fn_log = Atom.atom "log"
    val fn_log10 = Atom.atom "log10"
    val fn_log2 = Atom.atom "log2"
    val fn_pow = Atom.atom "pow"
    val fn_round = Atom.atom "round"
    val fn_sin = Atom.atom "sin"
    val fn_sqrt = Atom.atom "sqrt"
    val fn_tan = Atom.atom "tan"
    val fn_trunc = Atom.atom "trunc"

  (* Query function names *)
    val fn_sphere = Atom.atom "sphere"

  (* Sets of strands *)
    val set_active = Atom.atom "active"
    val set_all    = Atom.atom "all"
    val set_stable = Atom.atom "stable"

  (* kernel names *)
    val kn_bspln3 = Atom.atom "bspln3"
    val kn_bspln5 = Atom.atom "bspln5"
    val kn_c4hexic = Atom.atom "c4hexic"
    val kn_ctmr = Atom.atom "ctmr"
    val kn_tent = Atom.atom "tent"
    val kn_c1tent = Atom.atom "c1tent" (* for backwards compatibility to vis12 *)
    val kn_c2tent = Atom.atom "c2tent" (* for backwards compatibility to vis12 *)
    val kn_c2ctmr = Atom.atom "c2ctmr" (* for backwards compatibility to vis12 *)

  end
