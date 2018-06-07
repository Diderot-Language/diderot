(* ein-sums.sml
 *
 * Transformations to push summations down in the expression tree.  Note that normalization
 * does not rewrite summations (CHECK THIS CLAIM!)
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure EinSums : sig

    val clean :  Ein.ein -> Ein.ein

    val distribute : Ein.ein -> Ein.ein

    val transform : Ein.ein -> Ein.ein

  end = struct

    structure E = Ein

    fun mkProd exps = E.Opn(E.Prod, exps)
    fun mkDiv  (e1, e2) = E.Op2(E.Div, e1, e2)

    fun rewriteProd [a] = a
      | rewriteProd exps = mkProd exps

    fun rewriteSum (c, p) = E.Sum(c, rewriteProd p)

    fun rewriteProdSum (pre, _, []) = rewriteProd pre
      | rewriteProdSum (pre, outer, post) = rewriteProd (pre@[rewriteSum(outer, post)])

    fun findIndex (v, searchspace) = List.find (fn x => x = E.V v) searchspace

  (* findSx : sum_indexid list*ein_exp -> index_id option
   * Is c in e?. Simple lookup in the searchspace
   *)
    fun findSx (c, b) = let
          fun sort [] = NONE
            | sort (e1::es) = (case findSx(c, e1)
                 of NONE => sort(es)
                  | someS => someS
                (* end case *))
          in
            case b
             of E.Const _                 => NONE
              | E.ConstR _                => NONE
              | E.Tensor(_, [])           => NONE
              | E.Tensor(_, shape)        => findIndex (c, shape)
              | E.Zero(shape)             => findIndex (c, shape)
              | E.Delta(i, j)             => findIndex (c, [i, j])
              | E.Epsilon(i, j, k)        => findIndex (c, [i, j, k])
              | E.Eps2(i, j)              => findIndex (c, [i, j])
              | E.Field(_, shape)         => findIndex (c, shape)
              | E.Lift e1                 => findSx (c, e1)
              | E.Conv(v,[],h,[])         => NONE
              | E.Conv(_ , alpha, _ , dx) => findIndex (c, alpha@dx)
              | E.Partial (shape)         => findIndex (c, shape)
              | E.Apply(e1, e2)           => sort [e1, e2]
              | E.Comp(e1, _)             => findSx(c, e1)
              | E.Probe(E.Conv(_, [], _, []), E.Tensor(_, []))
                                          => NONE
              | E.Probe(e1, e2)           => sort [e1, e2]
              | E.Value _                 => NONE
              | E.Img _                   => raise Fail "Img used pre expansion"
              | E.Krn _                   => raise Fail "Krn used pre expansion"
              | E.OField(_, e1, _)        => findSx(c,e1)
              | E.Poly(_, _,shape)       => raise Fail "Poly used pre expansion"
              | E.Sum(_, e1)              => findSx (c, e1)
              | E.Op1(_, e1)              => findSx (c, e1)
              | E.Op2(_, e1, e2)          => sort [e1, e2]
              | E.Op3(_, e1, e2, e3)      => sort [e1, e2, e3]
              | E.Opn(_, es)              => sort es
            (* end case *)
          end

  (* splitSum: sum_index_id * ein_exp list -> ein_exp list * ein_exp list
   * filters ein_exp list by which ones have sum_index_id c
   *)
    fun splitSum ((v, lb, ub), p) = let
          fun filter (s, keep, []) = (List.rev s, List.rev keep)
            | filter (s, keep, e::es) = (case e
                 of E.Opn(E.Prod, p) => filter(s, keep, p@es)
                  | _ => (case findSx(v, e)
                       of NONE => filter (e::s, keep, es)
                        | SOME _ => filter (s, e::keep, es)
                      (* end case *))
                (* end case *))
          in
            filter ([], [], p)
          end

  (* splitMultipleSum:sum_index_id list *sum_index_id list *ein_exp list *ein_exp
   * Two summation indices sorts what the binding.
   * Check Tex file for clarity
   * Sum([c1,c2],pre*post)
   * pre  Σ_c2 post
   * return pre, outer Sum, post
   *)
    fun splitMultipleSum (c1, c2, pre, post) = (case (pre, post)
           of (_, []) => let
                val (pre, post) = splitSum(c1, pre)
              (* pre * Σ_c1(post) *)
                in
                  (pre, [c1], post)
                end
            | ([], _) => (case splitSum(c1, post)
                 of ([], D) => ([], [c1]@c2, D)        (* Σ_(c1,c2) D *)
                  | (C, []) => ([], c2, C)             (* Σ_(c2) D *)
                  | (C, D) => ([], c2, C@[rewriteSum([c1], D)]) (* Σ_(c2) C * Σ_(c1,c2) D  *)
               (* end case *))
            | _ => (case (splitSum(c1, pre), splitSum(c1, post))
                of ((C, []), (E, [])) => (C, c2, E)
                 | ((C, D), (E, [])) => (C@[rewriteSum([c1], D)], c2, E)
                 | ((C, []), (E, F)) => (C, c2, E@[rewriteSum([c1],F)])
                 | ((C, D), _) => (C, [c1], D@[rewriteSum(c2, post)])
               (* end case *))
          (* end case *))

  (* shiftSum : sum_index_e * ein_exp -> ein_exp
   * rewrites embedded summation
   *)
    fun shiftSum (sx, e) = let
        val c2 :: rest = List.rev sx
        val (A, B) = splitSum(c2, e)
        fun double ([], outer, pre, post) = rewriteProdSum(pre,outer,post)
          | double (c1::cs, outer, pre, post) = let
              val (pre', outer', post') = splitMultipleSum(c1, outer, pre, post)
              in
                double (cs, outer', pre', post')
              end
        val out = double(rest, [c2], A, B)
        in
          out
        end

    fun merge e = let
          fun merge2 (E.Sum(sx0, E.Sum(sx1, e))) = E.Sum(sx0@sx1, merge2 e)
            | merge2 (E.Opn(E.Prod, es)) = E.Opn(E.Prod, (List.map merge2 es))
            | merge2 (E.Sum(sx0, e)) = E.Sum(sx0, merge2 e)
            | merge2 e = e
          in
            merge2 e
          end

  (* clean : EIN -> EIN
   * Rewrites body by moving summation indices around
   *)
    fun clean (Ein.EIN{params, index, body}) = let
          fun rewriteBody body = (case body
                 of E.Lift e1             => E.Lift(rewriteBody e1)
                  | E.Apply(e1, e2)       => E.Apply(rewriteBody e1, rewriteBody e2)
                  | E.Probe(e1, e2)       => E.Probe(e1, rewriteBody e2)
                  | E.Value _             => raise Fail"Value before Expand"
                  | E.Img _               => raise Fail"Img before Expand"
                  | E.Krn _               => raise Fail"Krn before Expand"
                  | E.Comp(e1, es)        => E.Comp(rewriteBody e1, es)
                  | E.OField(ofld, e1, ix) => E.OField(ofld, rewriteBody e1, ix)
                  | E.Sum(sx, E.Opn(E.Prod, [e1])) =>
                      merge (shiftSum(sx, [ rewriteBody e1]))
                  | E.Sum(sx, E.Opn(E.Prod, [e1,e2])) =>
                      merge (shiftSum(sx, [rewriteBody e1, rewriteBody e2]))
                  | E.Sum(sx, E.Opn(E.Prod, e)) => merge (shiftSum(sx, e))
                  | E.Sum(sx, e1)         => merge (shiftSum(sx, [rewriteBody e1]))
                  | E.Op1(op1, e1)        => E.Op1(op1, rewriteBody e1)
                  | E.Op2(op2, e1, e2)    => E.Op2(op2, rewriteBody e1, rewriteBody e2)
                  | E.Op3(op3, e1, e2, e3)=>
                      E.Op3(op3, rewriteBody e1, rewriteBody e2, rewriteBody e3)
                  | E.Opn(opn, es)        => E.Opn(opn, List.map rewriteBody es)
                  | _                     => body
                (* end case *))
          in
            Ein.EIN{params=params, index=index, body=rewriteBody body}
          end

  (* Distribute summation if needed *)
    fun distribute (Ein.EIN{params, index, body}) = let
          val changed = ref false
          fun constant () = raise Fail "sum of constant"
          fun rewrite b = (case b
                 of E.Probe (e1, e2) => E.Probe(rewrite e1, rewrite e2)
                  | E.Sum(sx, E.Lift e1) => (changed := true; E.Lift(E.Sum(sx, e1)))
                  | E.Sum(sx1, E.Sum(sx2, e1)) => (changed := true; E.Sum (sx1@sx2, e1))
                  | E.Sum(sx, E.Op1(op1, e1)) => (changed := true; E.Op1(op1, E.Sum(sx, e1)))
                  | E.Sum(sx, E.Op2(E.Div, e1, e2)) => (
                      changed := true;
                      case e1
                       of E.Const _ => mkDiv (e1, E.Sum(sx, e2))
                        | E.ConstR _ => mkDiv (e1, E.Sum(sx, e2))
                        | _ => E.Sum(sx, mkProd [e1, mkDiv(E.Const 1, rewrite e2)])
                      (* end case *))
                  | E.Sum(sx, E.Op2(op2, e1, e2)) => (
                      changed := true; E.Op2(op2, E.Sum(sx, e1), E.Sum(sx, e2)))
                  | E.Sum(sx, E.Op3(op3, e1, e2, e3)) => (
                      changed := true; E.Op3(op3, E.Sum(sx, e1), E.Sum(sx, e2), E.Sum(sx, e3)))
                 | E.Sum(sx, E.Opn(E.Prod, es)) => let
                      val p' = List.map rewrite es
                      val (c, e) = EinFilter.filterSca(sx, p')
                      in
                        if c then changed := true else (); e
                      end
                  | E.Sum(sx, E.Opn(opn, es)) => (
                      changed := true; E.Opn(opn, List.map (fn e1 => E.Sum(sx, e1)) es))
(* QUESTION: should we rewrite the body of the Sum here? *)
                  | E.Sum(sx, _) => b
                  | E.Op1(op1, e1) => E.Op1(op1, rewrite e1)
(* QUESTION: doesn't this optimization get done elsewhere? *)
                  | E.Op2(E.Sub, e1, E.Const 0) => (changed := true; rewrite e1)
                  | E.Op2(op2, e1, e2) => E.Op2(op2, rewrite e1, rewrite e2)
                  | E.Opn(E.Prod, es) => (case es
                       of [e1, E.Opn(E.Prod, es)] => (changed := true; mkProd(e1::es))
                        | [
                            E.Tensor(id0,[]), E.Tensor(id1, [i1]),
                            E.Sum([v], E.Opn(E.Prod, [E.Tensor(id2, [ix2]), E.Tensor(id3, [ix3])]))
                          ] => let
                            val e1 = E.Sum([v],mkProd[E.Tensor(id2,[ix2]),E.Tensor(id3,[ix3])])
                            val e2 = mkProd[E.Tensor(id0,[]),e1]
                            val e3 = mkProd[e2, E.Tensor(id1,[i1])]
                            in
                              changed := true; e3
                            end
                        | _ => E.Opn(E.Prod, List.map rewrite es)
                      (* end case *))
                  | E.Opn(opn, es) => E.Opn(opn, List.map rewrite es)
                  | _ => b
                (* end case *))
          fun loop body  = let
                val body' = rewrite body
                in
                  if !changed then  (changed := false ;loop body') else  body'
                end
          val b = loop body
          in
            Ein.EIN{params=params, index=index, body=b}
          end

  (* distribute and clean summation *)
    fun transform ein = let
          val ein = distribute ein
          val ein = clean ein
          val ein = distribute ein
          val ein = clean ein
          in
            ein
          end

  end
