(* rational.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Support for rational numbers.
 *)

signature RATIONAL =
  sig

    eqtype t

    val zero : t

    val explode : t -> {sign : int, num : IntInf.int, denom : IntInf.int}

    val ~ : t -> t
    val + : t * t -> t
    val - : t * t -> t
    val * : t * t -> t
    val div : t * t -> t

(*
    val min : t * t -> t
    val max : t * t -> t
*)
    val abs : t -> t

    val sign : t -> int
    val sameSign : t * t -> bool

(*
    val > : t * t -> bool
    val >= : t * t -> bool
    val < : t * t -> bool
    val <= : t * t -> bool
*)

    val isZero : t -> bool

    val same : t * t -> bool
    val compare : t * t -> order

    val / : LargeInt.int * LargeInt.int -> t

    val fromInt : int -> t
    val fromLargeInt : LargeInt.int -> t

    val toString : t -> string

    val toReal : t -> real

  end

structure Rational :> RATIONAL =
  struct

    structure II = IntInf

  (* invariants:
   *   denom > 0
   *   gcd(num, denom) = 1
   *)
    datatype t = R of {num : II.int, denom : II.int}

    fun explode (R{num, denom}) =
          if (num < 0)
            then {sign = ~1, num = ~num, denom = denom}
          else if (num = 0)
            then {sign = 0, num = 0, denom = 0}
            else {sign = 1, num = num, denom = denom}

    val zero = R{num=0, denom=1}

    fun isZero (R{num, ...}) = (num = 0)

    fun gcd (a : II.int, 0) = a
      | gcd (a, b) = if (a > b)
          then gcd(a-b, b)
          else gcd(a, b-a)

    fun mkRat (0, _) = zero
      | mkRat (n, 1) = R{num=n, denom=1}
      | mkRat (num, denom) = let
          val d = gcd(II.abs num, denom)
          in
            R{num = num div d, denom = denom div d}
          end

    fun neg (R{num, denom}) = R{num = ~num, denom = denom}

    fun add (R{num=n1, denom=d1}, R{num=n2, denom=d2}) = let
          val d = gcd(d1, d2)
          val a1 = d2 div d
          val a2 = d1 div d
          val lcm = a1 * d1
          in
            mkRat (a1*n1 + a2*n2, lcm)
          end

    fun sub (R{num=n1, denom=d1}, R{num=n2, denom=d2}) = let
          val d = gcd(d1, d2)
          val a1 = d2 div d
          val a2 = d1 div d
          val lcm = a1 * d1
          in
            mkRat (a1*n1 - a2*n2, lcm)
          end

    fun mul (R{num=n1, denom=d1}, R{num=n2, denom=d2}) =
          mkRat (n1*n2, d1*d2)

    fun divide (_, R{num=0, ...}) = raise Div
      | divide (R{num=n1, denom=d1}, R{num=n2, denom=d2}) = let
          val n = n1 * d2
          val d = n2 * d1
          in
            if (d < 0) then mkRat(~n, ~d) else mkRat(n, d)
          end

    fun abs (R{num, denom}) = R{num = II.abs num, denom = denom}

    fun sign (R{num, ...}) = II.sign num

    fun sameSign (R{num=n1, ...}, R{num=n2, ...}) = II.sameSign(n1, n2)

    fun same (R{num=n1, denom=d1}, R{num=n2, denom=d2}) = (n1 = n2) andalso (d1 = d2)

    fun compare (R{num=n1, denom=d1}, R{num=n2, denom=d2}) = (
          case Int.compare(II.sign n1, II.sign n2)
           of EQUAL =>
                if (d1 = d2)
                  then II.compare(n1, n2)
                  else let
                    val d = gcd(d1, d2)
                    val a1 = d2 div d
                    val a2 = d1 div d
                    in
                      II.compare(a1*n1, a2*n2)
                    end
            | order => order
          (* end case *))

(*
    val > : t * t -> bool
    val >= : t * t -> bool
    val < : t * t -> bool
    val <= : t * t -> bool

    val min : t * t -> t
    val max : t * t -> t
*)

    fun fromInt n = mkRat(II.fromInt n, 1)
    fun fromLargeInt n = mkRat(n, 1)

    fun toString (R{num, denom = 1}) = II.toString num
      | toString (R{num, denom}) =
          String.concat[II.toString num, "/", II.toString denom]

    fun toReal (R{num, denom = 1}) = Real.fromLargeInt num
      | toReal (R{num, denom}) = Real.fromLargeInt num / Real.fromLargeInt denom

    fun op / (_, 0) = raise Div
      | op / (a, b) = if (b < 0)
          then mkRat(~a, ~b)
          else mkRat(a, b)

  (* bind operators *)
    val ~ : t -> t = neg
    val op + : t * t -> t = add
    val op - : t * t -> t = sub
    val op * : t * t -> t = mul
    val op div : t * t -> t = divide
(*
    val > : t * t -> bool
    val >= : t * t -> bool
    val < : t * t -> bool
    val <= : t * t -> bool
*)

  end
