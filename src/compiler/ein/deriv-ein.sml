(* derivative.sml
 *
 * Takes derivative of EIN terms including polynomials
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 *)

structure DerivativeEin : sig

    val differentiate: Ein.mu list * Ein.ein_exp -> Ein.ein_exp

  end  = struct

    structure E = Ein

    fun err str = raise Fail (String.concat["Ill-formed EIN Operator: ", str])

    fun mkAdd exps = E.Opn(E.Add, exps)
    fun mkSub (e1, e2) = E.Op2(E.Sub, e1, e2)
    fun mkProd exps = E.Opn(E.Prod, exps)
    fun mkDiv (e1, e2) = E.Op2(E.Div, e1, e2)
    fun mkNeg e = E.Op1(E.Neg, e)
    fun mkAbs e = E.Op1(E.Abs, e)

    fun filterProd args = (case EinFilter.mkProd args
           of SOME e => e
            | NONE => mkProd args
          (* end case *))

    fun rewriteProd [a] = a
      | rewriteProd exps = E.Opn(E.Prod, exps)

    fun iterPP es = let
        fun iterP([], [r]) = r
        | iterP ([], rest) = rewriteProd rest
        | iterP (E.Const 0::es, rest) = E.Const(0)
        | iterP (E.Const 1::es, rest) = iterP(es, rest)
        | iterP (E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::es, rest) =
            (* variable can't be 0 and 1 '*)
            if(c1=c2)
            then iterP (es, E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::rest)
            else E.Const(0)
        | iterP(E.Opn(E.Prod, ys)::es, rest) = iterP(ys@es, rest)
        | iterP (e1::es, rest)   = iterP(es, e1::rest)
        in iterP(es, []) end

    fun iterAA(es) = let
        fun iterA([], []) = E.Const 0
        | iterA([], [r]) = r
        | iterA ([], rest) = E.Opn(E.Add, rest)
        | iterA (E.Const 0::es, rest) = iterA(es, rest)
        | iterA (E.Opn(E.Add, ys)::es, rest) = iterA(ys@es, rest)
        | iterA (e1::es, rest)   = iterA(es, e1::rest)
        in iterA(es, []) end


  (* chain rule *)
    fun prodAppPartial ([], _) = err "Empty App Partial"
      | prodAppPartial ([e1], p0) = E.Apply(p0, e1)
      | prodAppPartial (e::es, p0) = let
          val l = prodAppPartial (es, p0)
          val e2' = filterProd [e, l]
          val e1' = filterProd (es @ [E.Apply(p0, e)])
          in
            mkAdd[e1', e2']
          end

(*---------------------------------------------------------------------------------------------------------*)
    (*single derivative of a unary operator
    del: apply derivative to e1
    *)
    fun applyOp1Single (op1, e1, del) = let
          val one = E.Const 1
          val half = mkDiv (E.Const 1, E.Const 2)
          val square = mkProd [e1, e1]
          val e2 = mkDiv(one, E.Op1(E.Sqrt, mkSub(one, square)))
          val ee = case op1
             of E.Neg       => mkNeg del
              | E.Exp       => mkProd [del, E.Op1(E.Exp, e1)]
              | E.Sqrt      => let
                  val e3 = mkDiv (del, E.Op1(op1, e1))
                  in mkProd [half, e3]
                  end
              | E.Cosine    => mkProd [mkNeg (E.Op1(E.Sine, e1)), del]
              | E.ArcCosine =>  mkProd [mkNeg e2, del]
              | E.Sine      =>  mkProd [E.Op1(E.Cosine, e1), del]
              | E.ArcSine   =>  mkProd [e2, del]
              | E.Tangent   =>
                  mkProd [mkDiv(one, mkProd[E.Op1(E.Cosine, e1), E.Op1(E.Cosine, e1)]), del]
              | E.ArcTangent=>
                  mkProd [mkDiv(one, mkAdd[one, square]), del]
              | E.PowInt n  => mkProd [E.Const n, E.Op1(E.PowInt(n-1), e1), del]
              | E.Abs       => mkProd [del,  E.Op1(E.Sgn, e1)]
              | E.Sgn       => raise Fail "unhandled case: ask charisee"
          (* end case *)
          in ee
        end
    (*apply derivative with apply expression*)
    fun applyOp1 (op1, e1, dx) = let
            val d0::dn = dx
            val del = E.Apply(E.Partial[d0], e1)
            fun iterDn e = if null dn then e else E.Apply(E.Partial dn, e)
            val single = applyOp1Single (op1, e1, del)
            (* end case *)
            in iterDn single
            end
(*---------------------------------------------------------------------------------------------------------*)
    fun applyOp2Single (op2, e1, dele1, e2, dele2) = (case op2
        of E.Sub    => E.Op2(E.Sub, dele1, dele2)
         | E.Div    => let
            val num = E.Op2(E.Sub, iterPP([dele1,e2]),iterPP ([e1, dele2]))
            in
                E.Op2(E.Div, num, iterPP([e2, e2]))
            end
        (* end case*))
    fun applyOp2 (op2, e1, e2, dx) = let
        val d0::dn = dx
        val dele1 = E.Apply(E.Partial[d0], e1)
        val dele2 = E.Apply(E.Partial[d0], e2)
        fun iterDn e = if null dn then e else E.Apply(E.Partial dn, e)
        val single = applyOp2Single (op2, e1, dele1, e2, dele2)
        (* end case *)
        in iterDn single
    end
(*---------------------------------------------------------------------------------------------------------*)
    (* differentiate *)
    (*note would need to keep track of change*)
    fun differentiate (px, body) =
        (case body
            of E.Const _            => E.Const 0
            | E.ConstR _            => E.Const 0
            | E.Zero _              => E.Const 0
            | E.Delta _             => E.Const 0
            | E.Epsilon _           => E.Const 0
            | E.Eps2 _              => E.Const 0
            | E.Field _             => body
            | E.Tensor _            => E.Const 0
            | E.Poly (e1, n, dx)  => E.Poly(e1, n, dx@px)
            | E.Lift(e1)            => E.Lift(differentiate(px, e1))
            | E.Sum(op1, e1)        => let
                val e2 = differentiate(px, e1)
                in (case e2
                    of E.Opn(E.Add, ps) => iterAA(List.map (fn e1=>E.Sum(op1, e1)) ps)
                    | _                 => E.Sum(op1, e2)
                (*end case*))
                end
            | E.Op1(op1, e1) =>     (* applyOp1 (op1, e1, px)*)
                applyOp1Single(op1, e1, differentiate(px, e1))
            | E.Op2(op2, e1, e2) =>  (*applyOp2 (op2, e1, e2, px)*)
                applyOp2Single (op2, e1, differentiate(px, e1), e2,differentiate(px, e2))
            | E.Opn(E.Prod, [e1])        => raise Fail(EinPP.expToString(e1))
            | E.Opn(E.Prod, e1::es)        =>  let
                val (d0::dn) = px
                val e1' = differentiate ([d0], e1)
                val es' = differentiate ([d0], iterPP(es))
                val A = iterPP([e1,es'])
                val B = iterPP(e1'::es)
                val e = iterAA([A,B])
                fun iterDn e2 = if null dn then e2 else E.Apply(E.Partial dn, e2)
                in iterDn e  end
            | E.Opn(opn, es)        =>             let
                val xx = List.map (fn e1=> differentiate (px, e1)) es
                in iterAA(xx) end
            | _    => raise Fail(EinPP.expToString(body))
        (* end case*))
        end

   end
