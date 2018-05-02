(* mid-contract.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Contraction phase for MidIR.
 *)

structure MidContract : sig

    val transform : MidIR.program -> MidIR.program

  end = struct

    structure IR = MidIR
    structure Op = MidOps
    structure Ty = MidTypes
    structure V = IR.Var
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntAddNeg               = ST.newCounter "mid-opt:add-neg"
    val cntAddZero              = ST.newCounter "mid-opt:add-zero"
    val cntSubNeg               = ST.newCounter "mid-opt:sub-neg"
    val cntSubZero              = ST.newCounter "mid-opt:syb-zero"
    val cntSubSame              = ST.newCounter "mid-opt:sub-same"
    val cntNegNeg               = ST.newCounter "mid-opt:neg-neg"
    val cntEigenVals            = ST.newCounter "mid-opt:eigen-vals"
    val cntIntToReal            = ST.newCounter "mid-opt:int-to-real"
    val cntUnused               = ST.newCounter "mid-opt:unused"
    val firstCounter            = cntAddNeg
    val lastCounter             = cntUnused

    structure UnusedElim = UnusedElimFn (
        structure IR = IR
        val cntUnused = cntUnused)

    fun useCount (IR.V{useCnt, ...}) = !useCnt

  (* adjust a variable's use count *)
    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun decUse (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)
    fun use x = (incUse x; x)

    fun getRHSOpt x = (case V.getDef x
           of IR.OP arg => SOME arg
            | _ => NONE
          (* end case *))

    fun doAssign (lhs, IR.OP rhs) = (case rhs
           of (Op.IAdd, [a, b]) => (case V.getDef b
                 of IR.OP(Op.INeg, [c]) => (
                    (* rewrite to "a-c" *)
                      ST.tick cntAddNeg;
                      decUse b;
                      SOME[(lhs, IR.OP(Op.ISub, [a, use c]))])
                  | IR.LIT(Literal.Int 0) => (
                    (* rewrite to "a" *)
                      ST.tick cntAddZero;
                      decUse b;
                      SOME[(lhs, IR.VAR a)])
                  | _ => NONE
                 (* end case *))
            | (Op.ISub, [a, b]) => if IR.Var.same(a, b)
                then ( (* rewrite to 0 *)
                  ST.tick cntSubSame;
                  decUse a; decUse b;
                  SOME[(lhs, IR.LIT(Literal.Int 0))])
                else (case V.getDef b
                   of IR.OP(Op.INeg, [c]) => (
                      (* rewrite to "a+c" *)
                        ST.tick cntSubNeg;
                        decUse b;
                        SOME[(lhs, IR.OP(Op.IAdd, [a, use c]))])
                    | IR.LIT(Literal.Int 0) => (
                      (* rewrite to "a" *)
                        ST.tick cntSubZero;
                        decUse b;
                        SOME[(lhs, IR.VAR a)])
                    | _ => NONE
                  (* end case *))
            | (Op.INeg, [a]) => (case getRHSOpt a
                 of SOME(Op.INeg, [b]) => (
                    (* rewrite to "b" *)
                      ST.tick cntNegNeg;
                      decUse a;
                      SOME[(lhs, IR.VAR(use b))])
                  | _ => NONE
                (* end case *))
            | (Op.IntToReal, [a]) => (case V.getDef a
                 of IR.LIT(Literal.Int n) => (
                      (* rerite to a real literal *)
                        ST.tick cntIntToReal;
                        decUse a;
                        SOME[(lhs, IR.LIT(Literal.Real(RealLit.fromInt n)))])
                  | _ => NONE
                (* end case *))
            | _ => NONE
          (* end case *))
      | doAssign _ = NONE

    fun doMAssign (ys, rhs) = (case (ys, rhs)
           of ([vals, vecs], IR.OP(Op.EigenVecs2x2, xs)) =>
                if (useCount vecs = 0)
                  then (
                    ST.tick cntEigenVals;
                    SOME[(vals, IR.OP(Op.EigenVals2x2, xs))])
                  else NONE
            | ([vals, vecs], IR.OP(Op.EigenVecs3x3, xs)) =>
                if (useCount vecs = 0)
                  then (
                    ST.tick cntEigenVals;
                    SOME[(vals, IR.OP(Op.EigenVals3x3, xs))])
                  else NONE
            | _ => NONE
          (* end case *))

    structure Rewrite = RewriteFn (
      struct
        structure IR = IR
        val doAssign = doAssign
        val doMAssign = doMAssign
        val elimUnusedVars = UnusedElim.reduce
      end)

    val transform = Rewrite.transform

  end
