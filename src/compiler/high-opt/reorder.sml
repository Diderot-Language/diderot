(* reorder.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Reorder : sig

  (* Reorders the subexpressions in an Ein Function after substitution *)
    val transform : Ein.ein -> Ein.ein

  end = struct

    structure E = Ein

  (* rewriteProd : ein_exp list ->ein_exp*)
    fun rewriteProd [e] = e
      | rewriteProd exps = E.Opn(E.Prod, exps)

  (* rewriteProd : ein_exp * ein_exp list ->ein_exp*)
    fun rewriteApply (c, p) = E.Apply(c, rewriteProd p)

  (* pushApply:ein_exp * ein_exp list ->int* ein_exp
   * Moves non-field expression outside of apply
   *)
    fun pushApply (del, p) = (case EinFilter.partitionField p
           of ([], []) => raise Fail "No Field in Apply expression"
            | ([], post) => (false, rewriteApply(del, post))
            | (pre, post) => (true, rewriteProd(pre@[rewriteApply(del, post)]))
          (* end case *))

    (* Does some simple ordering
       Scalars*Epsilons*deltas*else
    *)
    fun transform (Ein.EIN{params, index, body}) = let
          val changed = ref false
          fun order b = (case b
                 of E.Lift e => E.Lift(order e)
                  | E.Apply(e1, E.Opn(E.Prod, e2)) => let
                      val (change, e') = pushApply(e1, e2)
                      in
                        if change then changed := true else ();
                        e'
                      end
                  | E.Probe(e1, e2) => E.Probe(order e1, order e2)
                  | E.Sum(c1,E.Sum(c2, e)) => (changed:=true;E.Sum(c1@c2,order e))
                  | E.Sum(c, e1) => E.Sum(c, order e1)
                  | E.Op1(op1, e1) => E.Op1(op1,order e1)
                  | E.Op2(op2, e1,e2) => E.Op2(op2,order e1,order e2)
                  | E.Op3(op3, e1, e2, e3) => E.Op3(op3, order e1, order e2, order e3)
                  | E.Opn(E.Prod, es) => let
                      val (s, eps, dels, p) = EinFilter.partitionGreek es
                      val p' = List.map order p
                      in
                        E.Opn(E.Prod, s@eps@dels@p')
                      end
                  | E.Opn(opn, es) => E.Opn(opn, List.map order es)
                  | _ => b
                (* end case *))
      (* iterate to a fixed point *)
        fun loop body = let
              val body' = order body
              in
                if !changed
                  then (changed := false; loop body')
                  else body'
              end
        val b = loop body
(* QUESTION: I don't think that the following does anything, since we are at a fixed point!
        val b' = order b
*)
        in
          Ein.EIN{params=params, index=index, body=b}
        end

  end 
