(* eval-kern.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure EvalKern : sig

  (* `expand (result, d, h, k, [x])`
   *
   * expands the EvalKernel operations into vector operations.  The parameters
   * are
   *    result  -- the lhs variable to store the result
   *    d       -- the vector width of the operation, which should be equal
   *               to twice the support of the kernel
   *    h       -- the kernel
   *    k       -- the derivative of the kernel to evaluate
   *    x       -- the d-wide vector that specifies the values at which the
   *               kernel is being evaluated.
   *
   * The generated code is computing
   *
   *    result = a_0 + x*(a_1 + x*(a_2 + ... x*a_n) ... )
   *
   * as a d-wide vector operation, where n is the degree of the kth derivative
   * of h and the a_i are d-wide coefficient vectors that have an element for
   * each piece of h.  The computation is implemented as follows
   *
   *    m_n     = x * a_n
   *    s_{n-1} = a_{n-1} + m_n
   *    m_{n-1} = x * s_{n-1}
   *    s_{n-2} = a_{n-2} + m_{n-1}
   *    m_{n-2} = x * s_{n-2}
   *    ...
   *    s_1     = a_1 + m_2
   *    m_1     = x * s_1
   *    result  = a_0 + m_1
   *
   * Note that the coeffient vectors are flipped.
   *)
    val expand : LowIR.var * int * Kernel.t * int * LowIR.var list
          -> (LowIR.var * LowIR.rhs) list

  end = struct

    structure IR = LowIR
    structure Ty = LowTypes
    structure Op = LowOps

  (* convert a rational to a RealLit.t value.  We do this by long division
   * with a cutoff when we get to 12 digits.
   *)
    fun ratToFloat r = (case Rational.explode r
           of {sign=0, ...} => RealLit.zero false
            | {sign, num, denom=1} => RealLit.fromInt(IntInf.fromInt sign * num)
            | {sign, num, denom} => let
              (* normalize so that num <= denom *)
                val (denom, exp) = let
                      fun lp (n, denom) = if (denom < num)
                            then lp(n+1, denom*10)
                            else (denom, n)
                      in
                        lp (1, denom)
                      end
              (* normalize so that num <= denom < 10*num *)
                val (num, exp) = let
                      fun lp (n, num) = if (10*num < denom)
                            then lp(n-1, 10*num)
                            else (num, n)
                      in
                        lp (exp, num)
                      end
              (* divide num/denom, computing the resulting digits *)
                fun divLp (n, a) = let
                      val (q, r) = IntInf.divMod(a, denom)
                      in
                        if (r = 0) then (q, [])
                        else if (n < 12) then let
                          val (d, dd) = divLp(n+1, 10*r)
                          in
                            if (d < 10)
                              then (q, (IntInf.toInt d)::dd)
                              else (q+1, 0::dd)
                          end
                        else if (IntInf.div(10*r, denom) < 5)
                          then (q, [])
                          else (q+1, []) (* round up *)
                      end
                val digits = let
                      val (d, dd) = divLp (0, num)
                      in
                        (IntInf.toInt d)::dd
                      end
                in
                  RealLit.fromDigits{isNeg=(sign < 0), digits=digits, exp=exp}
                end
          (* end case *))

    fun expand (result, d, h, k, [x]) = let
          val {isCont, segs} = Kernel.curve (h, k)
        (* degree of polynomial *)
          val deg = List.length(hd segs) - 1
        (* convert to a vector of vectors to give fast access *)
          val segs = Vector.fromList (List.rev (List.map Vector.fromList segs))
        (* get the kernel coefficient value for the d'th term of the i'th
         * segment.
         *)
          fun coefficient d i =
                Literal.Real(ratToFloat (Vector.sub (Vector.sub(segs, i), d)))
          val ty = Ty.vecTy d
          val coeffs = List.tabulate (deg+1, fn i => IR.Var.new("a"^Int.toString i, ty))
        (* code to define the coefficient vectors *)
          val coeffVecs = let
                fun mk (x, (i, code)) = let
                      val lits = List.tabulate(d, coefficient i)
                      val vars = List.tabulate(d, fn _ => IR.Var.new("_f", Ty.realTy))
                      val code =
                            ListPair.map (fn (x, lit) => (x, IR.LIT lit)) (vars, lits) @
                              (x, IR.CONS(vars, IR.Var.ty x)) :: code
                      in
                        (i-1, code)
                      end
                in
                  #2 (List.foldr mk (deg, []) coeffs)
                end
        (* build the evaluation of the polynomials in reverse order *)
          fun pTmp i = IR.Var.new("prod" ^ Int.toString i, ty)
          fun sTmp i = IR.Var.new("sum" ^ Int.toString i, ty)
          fun eval (i, [coeff]) = let
                val m = pTmp i
                in
                  (m, [(m, IR.OP(Op.VMul d, [x, coeff]))])
                end
            | eval (i, coeff::r) = let
                val (m, stms) = eval(i+1, r)
                val s = sTmp i
                val m' = pTmp i
                val stms =
                      (m', IR.OP(Op.VMul d, [x, s])) ::
                      (s, IR.OP(Op.VAdd d, [coeff, m])) ::
                      stms
                in
                  (m', stms)
                end
          val evalCode = (case coeffs
                 of [a0] => (* constant function *)
                      [(result, IR.VAR a0)]
                  | a0::r => let
                      val (m, stms) = eval (1, r)
                      in
                        List.rev ((result, IR.OP(Op.VAdd d, [a0, m]))::stms)
                      end
                (* end case *))
          in
            coeffVecs @ evalCode
          end

  end
