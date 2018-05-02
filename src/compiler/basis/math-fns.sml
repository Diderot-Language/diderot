(* math-fns.sml
 *
 * Math functions that have not been lifted to work on fields.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure MathFns =
  struct

    datatype t
      = ATAN2		(* `atan2(y, x)` computes the principal value of the arc tangent of `y/x`,
			 * using the signs of both arguments to determine the quadrant of the
			 * return value. *)
      | CEIL		(* `ceil(x)` returns the smallest integral value greater than or equal to x. *)
      | ERF		(* `erf(x)` calculates the error function of `x` *)
      | ERFC		(* `erfc(x)` calculates the complementary error function of `x` *)
      | FLOOR		(* `floor(x)` returns the largest integral value less than or equal to x. *)
      | FMOD		(* `fmod(x, y)` computes the floating-point remainder of `x/y`. *)
      | LOG		(* computes the value of the natural logarithm of `x`. *)
      | LOG10		(* computes the value of the logarithm of `x` to base 10. *)
      | LOG2		(* computes the value of the logarithm of `x` to base 2. *)
      | POW		(* `pow(x,y)` returns `x` raised to the `y` power *)
      | ROUND		(* `round(x)` returns the integral value nearest to `x`; the treatment
                         * of 0.5 fractions is implementation dependent *)
      | TRUNC		(* `trunc(x)` returns integral value nearest to but no larger in
                         * magnitude than `x` *)

    fun toString f = (case f
           of ATAN2 => "atan2"
            | CEIL  => "ceil"
            | ERF   => "erf"
            | ERFC  => "erfc"
            | FLOOR => "floor"
            | FMOD  => "fmod"
            | LOG   => "log"
            | LOG10 => "log10"
            | LOG2  => "log2"
            | POW   => "pow"
            | ROUND => "round"
            | TRUNC => "trunc"
          (* end case *))

    fun hash f = (case f
           of ATAN2 => 0w101
            | CEIL  => 0w103
            | ERF   => 0w107
            | ERFC  => 0w109
            | FLOOR => 0w113
            | FMOD  => 0w137
            | LOG   => 0w139
            | LOG10 => 0w149
            | LOG2  => 0w151
            | POW   => 0w157
            | ROUND => 0w163
            | TRUNC => 0w167
          (* end case *))

    fun same (ATAN2, ATAN2) = true
      | same (CEIL, CEIL) = true
      | same (ERF, ERF) = true
      | same (ERFC, ERFC) = true
      | same (FLOOR, FLOOR) = true
      | same (FMOD, FMOD) = true
      | same (LOG, LOG) = true
      | same (LOG10, LOG10) = true
      | same (LOG2, LOG2) = true
      | same (POW, POW) = true
      | same (ROUND, ROUND) = true
      | same (TRUNC, TRUNC) = true
      | same _ = false

    fun arity f = (case f
           of ATAN2 => 2
            | CEIL => 1
            | ERF => 1
            | ERFC => 1
            | FLOOR => 1
            | FMOD => 2
            | LOG => 1
            | LOG10 => 1
            | LOG2 => 1
            | POW => 2
            | ROUND => 1
            | TRUNC => 1
          (* end case *))

  (* return a type signature for a math function (based on the representation of ints and
   * reals).
   *)
    fun sigOf (realTy, f) = (case f
           of ATAN2 => (realTy, [realTy, realTy])
            | FMOD => (realTy, [realTy, realTy])
            | POW => (realTy, [realTy, realTy])
            | _ => (realTy, [realTy])
          (* end case *))

  end
