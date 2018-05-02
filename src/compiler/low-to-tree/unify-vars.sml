(* unify-vars.sml
 *
 * Determine the equivalence classes of local variables.  Given two local
 * variables x and y, if the CFG has a JOIN or FOREACH node with
 *
 *      "x = phi(..., y, ...)",
 *
 * then x and y are in the same equivalence class.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure UnifyVars : sig

  (* analyze a CFG to identify equivalence classes for variables.  We also introduce
   * copy nodes just prior to the join edges for the phi nodes in the join.
   *) 
    val analyze : LowIR.cfg -> unit

  (* if a local variable is in an equivalence class with other variables, then return
   * the representative member of the class.
   *)
    val eqClassOf : LowIR.var -> LowIR.var option

  end = struct

    structure IR = LowIR

    val {peekFn, setFn, getFn, ...} = IR.Var.newProp URef.uRef

  (* handle a JOIN node *)
    fun doJoin (joinNd, preds, phis) = let
          fun doPhi (x, ys) = let
              val xRep = getFn x
            (* for each predecessor, add a copy stm and unify variables; we return
             * a new rhs list of variables for the phi node.
             *)
              fun doPreds ([], []) = []
                | doPreds (_::preds, NONE::ys) = NONE :: doPreds (preds, ys)
                | doPreds (pred::preds, SOME y :: ys) = let
                    val y' = IR.Var.copy y
                    val copyNd = IR.Node.mkASSIGN(y', IR.VAR y)
                    in
                      ignore (URef.union(xRep, getFn y'));
                      IR.Node.replaceOutEdge {src = pred, oldDst = joinNd, dst = copyNd};
                      IR.Node.replaceInEdge {oldSrc = pred, src = copyNd, dst = joinNd};
                      LowCensus.inc y';
                      SOME y' :: doPreds (preds, ys)
                    end
              in
              (* NOTE: since preds is changed by this transformation, we need to dereference
               * if anew for each phi node.
               *)
                (x, doPreds (!preds, ys))
              end
          in
            List.map doPhi phis
          end

  (* handle a FOREACH node *)
    fun doForeach (forNd, predNd, nextNd, phis) = let
          val IR.NEXT{pred = nextPred, ...} = IR.Node.kind nextNd
          fun doPhi (x, [SOME y1, SOME y2]) = let
              val xRep = getFn x
            (* push a copy for y across the pred->nd edge *)
              fun doPred (pred, nd, y) = let
                    val y' = IR.Var.copy y
                    val copyNd = IR.Node.mkASSIGN(y', IR.VAR y)
                    in
                      ignore (URef.union(xRep, getFn y'));
                      IR.Node.replaceOutEdge {src = pred, oldDst = nd, dst = copyNd};
                      IR.Node.replaceInEdge {oldSrc = pred, src = copyNd, dst = nd};
                      LowCensus.inc y';
                      SOME y'
                    end
              in
              (* note that we put the copies between the NEXT node at the end of the
               * loop body and its predecessor, since the last node of the loop must
               * be a NEXT node.
               *)
                (x, [doPred(!predNd, forNd, y1), doPred(!nextPred, nextNd, y2)])
              end
          in
            List.map doPhi phis
          end

    fun analyze cfg = let
          fun doNode nd = (case IR.Node.kind nd
                 of IR.JOIN{preds, mask, phis, ...} => phis := doJoin (nd, preds, !phis)
                  | IR.FOREACH{pred, bodyExit, phis, ...} =>
                      phis := doForeach (nd, pred, !bodyExit, !phis)
                  | _ => ()
                (* end case *))
          in
            IR.CFG.apply doNode cfg
          end

    fun eqClassOf x = (case peekFn x
           of NONE => NONE
            | SOME u => SOME(URef.!! u)
          (* end case *))

  end
