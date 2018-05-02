(* check-type.sml
 *
 * The typechecker for type expressions.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckType : sig

  (* check the well-formedness of a type and translate it to an AST type *)
    val check : Env.t * Env.context * ParseTree.ty -> Types.ty

  end = struct

    structure PT = ParseTree
    structure Ty = Types
    structure TU = TypeUtil

    fun err arg = (TypeError.error arg; Ty.T_Error)

    datatype token = datatype TypeError.token

  (* check a differentiation level, which must be >= 0 *)
    fun checkDiff (cxt, k) =
          if (k < 0)
            then (TypeError.error (cxt, [S "differentiation must be >= 0"]); Ty.DiffConst 0)
            else Ty.DiffConst(IntInf.toInt k)

  (* check a sequence dimension, which must be > 0 *)
    fun checkSeqDim (env, cxt, dim) = (case CheckExpr.checkDim (env, cxt, dim)
           of SOME d => if (d < 0)
                then (
                  TypeError.error (cxt, [S "invalid sequence dimension; must be non-negative"]);
                  Ty.DimConst 0)
                else Ty.DimConst(IntInf.toInt d)
            | NONE => Ty.DimConst 0
          (* end case *))

  (* check a dimension, which must be 1, 2 or 3 *)
    fun checkDim (env, cxt, dim) = (case CheckExpr.checkDim (env, cxt, dim)
           of SOME d => if (d < 1) orelse (3 < d)
                then (
                  TypeError.error (cxt, [S "invalid dimension; must be 1, 2, or 3"]);
                  Ty.DimConst 0)
                else Ty.DimConst(IntInf.toInt d)
            | NONE => Ty.DimConst 0
          (* end case *))

  (* check the well-formedness of a type and translate it to an AST type *)
    fun check (env, cxt, ty) = (case ty
           of PT.T_Mark m => check (Env.withEnvAndContext(env, cxt, m))
            | PT.T_Bool => Ty.T_Bool
            | PT.T_Int => Ty.T_Int
            | PT.T_Real => Ty.realTy
            | PT.T_Id strand => (case Env.findStrand(env, strand)
                 of SOME _ => Ty.T_Strand strand
                  | NONE => (
                      err(cxt, [S "unknown type ", A strand]);
                      Ty.T_Error)
                (* end case *))
            | PT.T_String => Ty.T_String
            | PT.T_Kernel k => Ty.T_Kernel(checkDiff(cxt, k))
            | PT.T_Field{diff, dim, shape} => Ty.T_Field{
                  diff = checkDiff (cxt, diff),
                  dim = checkDim (env, cxt, dim),
                  shape = CheckExpr.checkShape (env, cxt, shape)
                }
            | PT.T_Tensor shape => Ty.T_Tensor(CheckExpr.checkShape(env, cxt, shape))
            | PT.T_Image{dim, shape} => Ty.T_Image{
                  dim = checkDim (env, cxt, dim),
                  shape = CheckExpr.checkShape (env, cxt, shape)
                }
            | PT.T_Seq(ty, dim) => let
                val ty = check(env, cxt, ty)
                in
                  if TU.isValueOrStrandType ty
                    then Ty.T_Sequence(ty, SOME(checkSeqDim (env, cxt, dim)))
                    else err(cxt, [S "invalid element type for sequence: ", TY ty])
                end
            | PT.T_DynSeq ty => let
                val ty = check(env, cxt, ty)
                in
                  if TU.isValueOrStrandType ty
                    then Ty.T_Sequence(ty, NONE)
                    else err(cxt, [S "invalid element type for sequence: ", TY ty])
                end
          (* end case *))

  end
