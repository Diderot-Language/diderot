(* check-const.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckConst : sig

  (* evaluate a constant expression; this returns NONE if the expression is not a valid
   * constant expression and will also emit an error message into the error stream.
   * The bool should be true if the constant is the default value for an input variable,
   * since we then allow "load" and "image".
   *)
    val eval : ((Error.err_stream * Error.span) * bool * AST.expr) -> ConstExpr.t option

  end = struct

    structure L = Literal
    structure BV = BasisVars
    structure Ty = Types
    structure C = ConstExpr

    datatype token = datatype TypeError.token

  (* an exception to raise when the arguments to an operator are not concrete values
   * of the right type.
   *)
    exception Arg

    val unaryOpTbl : (C.t -> C.t) Var.Tbl.hash_table = let
          val tbl = Var.Tbl.mkTable (16, Fail "unOpTbl")
          val ins = Var.Tbl.insert tbl
          fun tensorNeg (C.Real r) = C.Real(RealLit.negate r)
            | tensorNeg (C.Tensor(vs, ty)) = C.Tensor(List.map tensorNeg vs, ty)
            | tensorNeg (C.Expr _) = raise Arg
            | tensorNeg _ = raise Fail "impossible"
          in
            List.app ins [
                (BV.op_not, fn (C.Bool b) => C.Bool(not b) | _ => raise Arg),
                (BV.neg_i, fn (C.Int a) => C.Int(IntLit.neg a) | _ => raise Arg),
                (BV.neg_t, tensorNeg)
              ];
            tbl
          end

    val binOpTbl : (C.t * C.t -> C.t) Var.Tbl.hash_table = let
          val tbl = Var.Tbl.mkTable (64, Fail "binOpTbl")
          val ins = Var.Tbl.insert tbl
          in
            List.app ins [
                (BV.equ_bb, fn (C.Bool a, C.Bool b) => C.Bool(a = b) | _ => raise Arg),
                (BV.neq_bb, fn (C.Bool a, C.Bool b) => C.Bool(a <> b) | _ => raise Arg),
                (BV.add_ii, fn (C.Int a, C.Int b) => C.Int(IntLit.add(a, b)) | _ => raise Arg),
                (BV.sub_ii, fn (C.Int a, C.Int b) => C.Int(IntLit.sub(a, b)) | _ => raise Arg),
                (BV.mul_ii, fn (C.Int a, C.Int b) => C.Int(IntLit.mul(a, b)) | _ => raise Arg),
                (BV.div_ii, fn (C.Int a, C.Int b) => C.Int(IntLit.divide(a, b)) | _ => raise Arg),
                (BV.op_mod, fn (C.Int a, C.Int b) => C.Int(IntLit.modulo(a, b)) | _ => raise Arg),
                (BV.lt_ii, fn (C.Int a, C.Int b) => C.Bool(a < b) | _ => raise Arg),
                (BV.lte_ii, fn (C.Int a, C.Int b) => C.Bool(a <= b) | _ => raise Arg),
                (BV.gt_ii, fn (C.Int a, C.Int b) => C.Bool(a > b) | _ => raise Arg),
                (BV.gte_ii, fn (C.Int a, C.Int b) => C.Bool(a >= b) | _ => raise Arg),
                (BV.equ_ii, fn (C.Int a, C.Int b) => C.Bool(a = b) | _ => raise Arg),
                (BV.neq_ii, fn (C.Int a, C.Int b) => C.Bool(a <> b) | _ => raise Arg)
              ];
            tbl
          end

    fun eval (cxt, true, e as AST.E_LoadNrrd _) = SOME(C.Expr e) (* top-level load is okay for input *)
      | eval (cxt, isInput, constExp) = let
          exception EVAL
          fun err msg = (TypeError.error (cxt, msg); raise EVAL)
          fun mkPrim (f, mvs, args, ty) =
                if Basis.allowedInConstExp f
                  then C.Expr(AST.E_Prim(f, mvs, List.map C.valueToExpr args, ty))
                  else err [S "invalid use of ", V f, S " in constant expression"]
          val findBinOp = Var.Tbl.find binOpTbl
          val findUnaryOp = Var.Tbl.find unaryOpTbl
          fun eval' e = (case e
                 of AST.E_Var(x, span) => (case C.valueOf x
                       of SOME v => v
                        | NONE => err [
                              S "reference to non-constant variable ", V x,
                              S " in constant expression"
                            ]
                      (* end case *))
                  | AST.E_Lit(L.String s) => C.String s
                  | AST.E_Lit(L.Bool b) => C.Bool b
                  | AST.E_Lit(L.Int i) => C.Int i
                  | AST.E_Lit(L.Real r) => C.Real r
                  | AST.E_Prim(f, mvs, [e], ty) => (case findUnaryOp f
                       of SOME rator => let
                            val e' = eval' e
                            in
                              rator e'
                                handle Arg => mkPrim (f, mvs, [e'], ty)
                            end
                        | NONE => err[S "invalid constant expression"]
                      (* end case *))
                  | AST.E_Prim(f, mvs, [e1, e2], ty) => (case findBinOp f
                       of SOME rator => let
                            val e1' = eval' e1
                            val e2' = eval' e2
                            in
                              rator (e1', e2')
                                handle Arg => mkPrim (f, mvs, [e1', e2'], ty)
                            end
                        | NONE => err[S "invalid constant expression"]
                      (* end case *))
                  | AST.E_Prim(f, mvs, args, ty) =>
                      mkPrim (f, mvs, List.map eval' args, ty)
                  | AST.E_Tensor(es, ty) => C.Tensor(List.map eval' es, ty)
                  | AST.E_Seq(es, ty) => C.Seq(List.map eval' es, ty)
                  | AST.E_Slice(e, indices, _) => (case (eval' e, indices)
                       of (C.Tensor(vs, _), _) => raise Fail "FIXME"
                        | (C.Seq(vs, _), [SOME idx]) => (case eval' idx
                             of C.Int i => (List.nth(vs, Int.fromLarge i)
                                  handle _ => err [S "out-of-bounds sequence access in constant expression"])
                              | C.Expr _ => C.Expr e
                              | _ => raise Fail "impossible"
                            (* end case *))
                        | (C.Expr _, _) => C.Expr e
                        | _ => raise Fail "impossible"
                      (* end case *))
                  | AST.E_LoadNrrd _ => if isInput
                      then err [S "invalid input initialization"]
                      else err [S "invalid constant expression"]
                  | AST.E_Coerce{srcTy=Ty.T_Int, dstTy as Ty.T_Tensor(Ty.Shape[]), e} => (
                      case eval' e
                       of C.Int i => C.Real(RealLit.fromInt i)
                        | C.Expr e' =>
                            C.Expr(AST.E_Coerce{srcTy=Ty.T_Int, dstTy=dstTy, e=e'})
                        | _ => raise Fail "impossible"
                      (* end case *))
                  | _ => err [S "invalid constant expression"]
                (* end case *))
          in
            SOME(eval' constExp) handle EVAL => NONE
          end

  end
