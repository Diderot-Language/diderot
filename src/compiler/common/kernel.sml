(* kernel.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure Kernel : sig

    type t

    type coefficient = Rational.t

  (* polynomial represented as list of coefficients, where ith element is
   * coefficient for x^i.
   *)
    type polynomial = coefficient list

  (* kernel name *)
    val name : t -> string
    val toString : t -> string

  (* are two kernels the same *)
    val same : t * t -> bool

  (* hash value *)
    val hash : t -> word

  (* how many levels of differentiation does a kernel provide? *)
    val continuity : t -> int

  (* kernel support *)
    val support : t -> int

  (* representation of i'th derivative of the kernel *)
    val curve : t * int -> {
            isCont : bool,
            segs : polynomial list      (* piece-wise polynomial that defines *)
                                        (* the curve over the whole support *)
          }

    val evaluate : polynomial * int -> Rational.t

  (* some standard kernels *)
    val tent : t                (* linear interpolation *)
    val ctmr : t                (* Catmull-Rom interpolation *)
    val bspln3 : t              (* cubic bspline reconstruction, doesn't interpolate *)
    val bspln5 : t              (* quintic bspline reconstruction, doesn't interpolate *)
    val c4hexic : t             (* C4 hexic kernel; doesn't interpolate *)
    val c1tent : t              (* a "C1" version of tent, for pedagogy *)
    val c2tent : t              (* a "C2" version of tent, for pedagogy *)
    val c2ctmr : t              (* a "C2" version of ctmr, for pedagogy *)

  end = struct

    structure R = Rational
    structure A = Array

    val maxDiffLevels = 15              (* support upto 15 levels of differentiation *)

    type coefficient = R.t

    val zero = R.zero
    val one = R.fromInt 1

  (* polynomial represented as list of coefficients, where ith element is
   * coefficient for x^i.
   *)
    type polynomial = coefficient list

    fun differentiate [] = raise Fail "invalid polynomial"
      | differentiate [_] = [zero]
      | differentiate (_::coeffs) = let
          fun lp (_, []) = []
            | lp (i, c::r) = R.*(R.fromInt i, c) :: lp(i+1, r)
          in
            lp (1, coeffs)
          end

  (* evaluate a polynomial at an integer coordinate (used to test continuity) *)
    fun evaluate (poly, x) = let
          val x = R.fromInt x
          fun eval (sum, [], xn) = sum
            | eval (sum, c::r, xn) = eval(R.+(sum, R.*(c, xn)), r, R.*(x, xn))
          in
            eval (zero, poly, one)
          end

    type curve = {
        isCont : bool,
        segs : polynomial list  (* piece-wise polynomial that defines *)
                                (* the curve over the whole support *)
      }

    datatype t = K of {
        name : string,
        id : Stamp.t,           (* unique ID *)
        support : int,          (* number of samples to left/right *)
        continuity : int,       (* number of levels of continuity *)
        curves : curve option array (* cache of curves indexed by differentiation level *)
      }

  (* determine if a list of polynomials represents a continuous piece-wise polynomial *)
    fun isContinuous polys = let
          val s = List.length polys div 2
          fun chk (i, y_i, []) = R.isZero y_i
            | chk (i, y_i, f_i::r) = let
                val y = evaluate(f_i, i)
                in
                  R.same(y_i, y) andalso chk(i+1, evaluate(f_i, i+1), r)
                end
          in
            chk (~s, R.zero, polys)
          end

  (* kernel name *)
    fun name (K{name, ...}) = name
    val toString = name

  (* kernel support *)
    fun support (K{support, ...}) = support

  (* how many levels of differentiation does a kernel provide? *)
    fun continuity (K{continuity, ...}) = continuity

    fun hash (K{id, ...}) = Stamp.hash id
    fun same (K{id=a, ...}, K{id=b, ...}) = Stamp.same (a, b)

  (* representation of i'th derivative of the kernel *)
    fun curve (K{curves, ...}, k) = (case A.sub(curves, k)
           of SOME curve => curve
            | NONE => let
              (* compute the (k+1)'th derivative, given the k'th *)
                fun diff (k, {isCont, segs}) = let
                      val segs' = List.map differentiate segs
                      val isCont' = isCont andalso isContinuous segs'
                      in {
                        isCont = isCont',
                        segs = segs'
                      } end
                fun lp (j, curve) = if (j < k)
                      then (case A.sub(curves, j+1)
                         of NONE => let
                              val curve' = diff(j+1, curve)
                              in
                                A.update(curves, j+1, SOME curve');
                                lp (j+1, curve')
                              end
                          | SOME curve' => lp(j+1, curve')
                        (* end case *))
                      else curve
                in
                  lp (0, valOf(A.sub(curves, 0)))
                end
          (* end case *))

  (* some standard kernels *)
    local
      val op / = R./
      fun r i = R.fromInt i
(* FIXME: we should really get the continuity info from the kernels themselves *)
      fun mkKernel {name, support, continuity, segs} = let
            val curves = Array.array(maxDiffLevels+1, NONE)
            val curve0 = {
                    isCont = isContinuous segs,
                    segs = segs
                  }
            in
              A.update (curves, 0, SOME curve0);
              K{name=name, id=Stamp.new(), support=support, continuity=continuity, curves=curves}
            end
    in
    val tent = mkKernel{                (* linear interpolation *)
            name = "tent",
            support = 1,
            continuity = 0,
            segs = [
                [r 1, r 1],             (* -1 .. 0 *)
                [r 1, r ~1]             (*  0 .. 1 *)
              ]
          }
    val ctmr = mkKernel{                (* Catmull-Rom interpolation *)
            name = "ctmr",
            support = 2,
            continuity = 1,
            segs = [
                [r 2, r 4,  5/2,  1/2], (* -2 .. -1 *)
                [r 1, r 0, ~5/2, ~3/2], (* -1 .. 0 *)
                [r 1, r 0, ~5/2,  3/2], (*  0 .. 1 *)
                [r 2, r ~4, 5/2, ~1/2]  (*  1 .. 2 *)
              ]
          }
    val bspln3 = mkKernel{              (* cubic bspline reconstruction; doesn't interpolate *)
            name = "bspln3",
            support = 2,
            continuity = 2,
            segs = [
                [ 4/3, r 2,  r 1,   1/6 ],      (* -2 .. -1 *)
                [ 2/3, r 0,  r ~1, ~1/2 ],      (* -1 .. 0 *)
                [ 2/3, r 0,  r ~1,  1/2 ],      (*  0 .. 1 *)
                [ 4/3, r ~2, r 1,  ~1/6 ]       (*  1 .. 2 *)
              ]
          }
    val bspln5 = mkKernel{              (* quintic bspline reconstruction; doesn't interpolate *)
            name = "bspln5",
            support = 3,
            continuity = 4,
            segs = [
                [ 81/40, 27/8, 9/4,  3/4,  1/8, 1/120 ],        (* -3 .. -2 *)
                [ 17/40, ~5/8, ~7/4, ~5/4, ~3/8, ~1/24 ],       (* -2 .. -1 *)
                [ 11/20, r 0,  ~1/2, r 0,  1/4, 1/12 ],         (* -1 .. 0 *)
                [ 11/20, r 0,  ~1/2, r 0,  1/4, ~1/12 ],        (*  0 .. 1 *)
                [ 17/40, 5/8,  ~7/4, 5/4, ~3/8, 1/24 ],         (*  1 .. 2 *)
                [ 81/40, ~27/8, 9/4, ~3/4, 1/8, ~1/120 ]        (*  2 .. 3 *)
              ]
          }
    val c4hexic = mkKernel{             (* C4 hexic kernel; doesn't interpolate *)
            name = "c4hexic",
            support = 3,
            continuity = 4,
            segs = [
                [ 1539/160, 189/8,  747/32,  r 12,  109/32,  61/120,  1/32 ],   (* -3 .. -2 *)
                [ 3/160,    ~35/8,  ~341/32, r ~10, ~147/32, ~25/24,  ~3/32 ],  (* -2 .. -1 *)
                [ 69/80,    r 0,    ~23/16,  r 0,   19/16,   7/12,    1/16 ],   (* -1 .. 0 *)
                [ 69/80,    r 0,    ~23/16,  r 0,   19/16,   ~7/12,   1/16 ],   (*  0 .. 1 *)
                [ 3/160,    35/8,   ~341/32, r 10,  ~147/32, 25/24,   ~3/32 ],  (*  1 .. 2 *)
                [ 1539/160, ~189/8, 747/32,  r ~12, 109/32,  ~61/120, 1/32 ]    (*  2 .. 3 *)
              ]
          }
    val c1tent = mkKernel{
            name = "c1tent",
            support = 1,
            continuity = 1, (* otherwise copy-paste of tent *)
            segs = [
                [r 1, r 1],             (* -1 .. 0 *)
                [r 1, r ~1]             (*  0 .. 1 *)
              ]
          }
    val c2tent = mkKernel{
            name = "c2tent",
            support = 1,
            continuity = 2, (* otherwise copy-paste of tent *)
            segs = [
                [r 1, r 1],             (* -1 .. 0 *)
                [r 1, r ~1]             (*  0 .. 1 *)
              ]
          }
    val c2ctmr = mkKernel{
            name = "c2ctmr",
            support = 2,
            continuity = 2, (* otherwise copy-paste of ctmr *)
            segs = [
                [r 2, r 4,  5/2,  1/2], (* -2 .. -1 *)
                [r 1, r 0, ~5/2, ~3/2], (* -1 .. 0 *)
                [r 1, r 0, ~5/2,  3/2], (*  0 .. 1 *)
                [r 2, r ~4, 5/2, ~1/2]  (*  1 .. 2 *)
              ]
          }
    end

  end
