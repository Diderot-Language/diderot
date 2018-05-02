(* low-contract.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Contraction phase for LowIR.
 *)

structure LowContract : sig

    val transform : LowIR.program -> LowIR.program

  end = struct

    structure IR = LowIR
    structure Op = LowOps
    structure Ty = LowTypes
    structure V = IR.Var
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntAddNeg               = ST.newCounter "low-contract:add-neg"
    val cntAddConst             = ST.newCounter "low-contract:add-const"
    val cntSubNeg               = ST.newCounter "low-contract:sub-neg"
    val cntSubSame              = ST.newCounter "low-contract:sub-same"
    val cntNegNeg               = ST.newCounter "low-contract:neg-neg"
    val cntIntToReal            = ST.newCounter "low-contract:int-to-real"
    val cntTensorIndex          = ST.newCounter "low-contract:tensor-index"
    val cntProjectLast          = ST.newCounter "low-contract:project-last"
    val cntSubscript            = ST.newCounter "low-contract:subscript"
    val cntUnused               = ST.newCounter "low-contract:unused"
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

  (* get the local definition of a variable.  Unlike getDef, this function does
   * not chase through global definitions, which means that we do not have to
   * worry about lifting local variables in the globalInit to global scope.
   *)
    fun getLocalDef (x as IR.V{bind, ...}) = (case !bind
           of IR.VB_RHS rhs => (case rhs
                 of IR.VAR x => getLocalDef x
                  | _ => rhs
                (* end case *))
            | _ => IR.VAR x
          (* end case *))

(* TODO: tensor selection operations *)
    fun doAssign (lhs, IR.OP rhs) = (case rhs
           of (Op.IAdd, [a, b]) => (case (V.getDef a, V.getDef b)
                 of (IR.LIT(Literal.Int a'), IR.LIT(Literal.Int b')) => (
                    (* rewrite to sum of a' and b' *)
                      ST.tick cntAddConst;
                      decUse a; decUse b;
                      SOME[(lhs, IR.LIT(Literal.Int(a' + b')))])
                  | (IR.LIT(Literal.Int 0), _) => (
                    (* rewrite to b *)
                      ST.tick cntAddConst;
                      decUse a;
                      SOME[(lhs, IR.VAR b)])
                  | (_, IR.LIT(Literal.Int 0)) => (
                    (* rewrite to a *)
                      ST.tick cntAddConst;
                      decUse b;
                      SOME[(lhs, IR.VAR a)])
                  | (_, IR.OP(Op.INeg, [c])) => (
                    (* rewrite to "a-c" *)
                      ST.tick cntAddNeg;
                      decUse b;
                      SOME[(lhs, IR.OP(Op.ISub, [a, use c]))])
                  | _ => NONE
                 (* end case *))
            | (Op.ISub, [a, b]) => if IR.Var.same(a, b)
                then ( (* rewrite to 0 *)
                  ST.tick cntSubSame;
                  decUse a; decUse b;
                  SOME[(lhs, IR.LIT(Literal.Int 0))])
                else (case getRHSOpt b
                   of SOME(Op.INeg, [c]) => (
                      (* rewrite to "a+c" *)
                        ST.tick cntSubNeg;
                        decUse b;
                        SOME[(lhs, IR.OP(Op.IAdd, [a, use c]))])
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
            | (Op.TensorIndex(Ty.TensorTy dims, idxs), [t]) => let
                fun get ([], [], x) = (
                      SOME[(lhs, IR.VAR(use x))])
                  | get (ix::ixs, d::ds, x) = (case getLocalDef x
                       of IR.CONS(ys, _) => get(ixs, ds, List.nth(ys, ix))
                        | _ => let
                            val rator = if List.null ds
                                  then Op.VIndex(d, ix)
                                  else Op.TensorIndex(Ty.tensorTy(d::ds), ix::ixs)
                            in
                              SOME[(lhs, IR.OP(rator, [use x]))]
                            end
                      (* end case *))
                  | get _ = raise Fail "malformed TensorIndex"
                in
                  case getLocalDef t
                   of IR.CONS _ => (ST.tick cntTensorIndex; decUse t; get(idxs, dims, t))
                    | _ => NONE
                  (* end case *)
                end
            | (Op.ProjectLast(Ty.TensorTy dims, idxs), [t]) => let
                fun get ([], [_], x) = (
                      SOME[(lhs, IR.VAR(use x))])
                  | get (ix::ixs, d::ds, x) = (case getLocalDef x
                       of IR.CONS(ys, _) => get(ixs, ds, List.nth(ys, ix))
                        | _ => SOME[
                              (lhs, IR.OP(Op.ProjectLast(Ty.tensorTy(d::ds), ix::ixs), [use x]))
                            ]
                      (* end case *))
                  | get _ = raise Fail "malformed ProjectLast"
                in
                  case getLocalDef t
                   of IR.CONS _ => (ST.tick cntProjectLast; decUse t; get(idxs, dims, t))
                    | _ => NONE
                  (* end case *)
                end
            | (Op.Subscript ty, [seq, idx]) => (case (getLocalDef seq, V.getDef idx)
                 of (IR.SEQ(vs, _), IR.LIT(Literal.Int i)) => (
                      ST.tick cntSubscript; decUse seq; decUse idx;
                      SOME[(lhs, IR.VAR(use (List.nth(vs, Int.fromLarge i))))])
                  | _ => NONE
                (* end case *))
            | _ => NONE
          (* end case *))
      | doAssign _ = NONE

    fun doMAssign _ = NONE

    structure Rewrite = RewriteFn (
      struct
        structure IR = IR
        val doAssign = doAssign
        val doMAssign = doMAssign
        val elimUnusedVars = UnusedElim.reduce
      end)

    val transform = Rewrite.transform

  end
