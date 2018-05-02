(* reductions.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure Reductions : sig

  (* the reduction operators supported in map-reduce computations.  Note that these
   * do not include the synthetic reductions, such as "mean"
   *)
    datatype t
      = ALL             (* boolean and; unit is true *)
      | EXISTS          (* boolean or; unit is false *)
      | IMAX            (* integer max; unit is DIDEROT_INT_MIN *)
      | RMAX            (* real max; unit is -∞ *)
      | IMIN            (* integer min; unit is DIDEROT_INT_MAX *)
      | RMIN            (* real min; unit is +∞ *)
      | IPRODUCT        (* integer product; unit is 1 *)
      | RPRODUCT        (* real product; unit is 1 *)
      | ISUM            (* integer sum; unit is 0 *)
      | RSUM            (* real sum; unit is 0 *)

    val toString : t -> string
    val same : t * t -> bool
    val hash : t -> word

  (* return the identity value for a reduction operator *)
    val identity : t -> Literal.t

  end = struct

  (* the reduction operators supported in map-reduce computations *)
    datatype t
      = ALL | EXISTS
      | IMAX | RMAX
      | IMIN | RMIN
      | IPRODUCT | RPRODUCT
      | ISUM | RSUM

    fun toString ALL = "all"
      | toString EXISTS = "exists"
      | toString IMAX = "max<int>"
      | toString RMAX = "max<real>"
      | toString IMIN = "min<int>"
      | toString RMIN = "min<real>"
      | toString IPRODUCT = "product<int>"
      | toString RPRODUCT = "product<real>"
      | toString ISUM = "sum<int>"
      | toString RSUM = "sum<real>"

    fun same (ALL, ALL) = true
      | same (EXISTS, EXISTS) = true
      | same (IMAX, IMAX) = true
      | same (RMAX, RMAX) = true
      | same (IMIN, IMIN) = true
      | same (RMIN, RMIN) = true
      | same (IPRODUCT, IPRODUCT) = true
      | same (RPRODUCT, RPRODUCT) = true
      | same (ISUM, ISUM) = true
      | same (RSUM, RSUM) = true
      | same _ = false

    fun hash ALL = 0w13
      | hash EXISTS = 0w17
      | hash IMAX = 0w23
      | hash RMAX = 0w31
      | hash IMIN = 0w37
      | hash RMIN = 0w41
      | hash IPRODUCT = 0w43
      | hash RPRODUCT = 0w47
      | hash ISUM = 0w53
      | hash RSUM = 0w57

    fun identity ALL = Literal.Bool true
      | identity EXISTS = Literal.Bool false
      | identity IMAX = Literal.Int (IntLit.minInt())
      | identity RMAX = Literal.Real RealLit.negInf
      | identity IMIN = Literal.Int (IntLit.maxInt())
      | identity RMIN = Literal.Real RealLit.posInf
      | identity IPRODUCT = Literal.Int 1
      | identity RPRODUCT = Literal.Real RealLit.one
      | identity ISUM = Literal.Int 0
      | identity RSUM = Literal.Real (RealLit.zero false)

  end
