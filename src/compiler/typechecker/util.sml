(* util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Util : sig

  (* `coerceType (dstTy, (e, srcTy))` checks to see if `srcTy` can be coerced to `dstTy`.
   * If so, it returns the coerced expression.  Otherwise it returns `NONE`.
   *)
    val coerceType : Types.ty * (AST.expr * Types.ty) -> AST.expr option

  (* `coerceType ((e1, ty1), (e2, ty2))` checks to see if either 1ty1` can be coerced
   * to `ty2` or vice versa.  If so, it returns the coerced expressions and resulting
   * type, otherwise it returns `NONE`.
   *)
    val coerceType2 : (AST.expr * Types.ty) * (AST.expr * Types.ty)
          -> (AST.expr * AST.expr * Types.ty) option

  (* given a non-empty list of types, find a common type that all of the types can
   * be coerced to (we assume coercion is transitive).  Return NONE if not such type
   * is found.
   *)
    val coerceTypes : Types.ty list -> Types.ty option

  end = struct

    structure Ty = Types

  (* build a coercion expression; for literal values, we can coerce directly *)
    fun coerceExp (Ty.T_Tensor(Ty.Shape[]), Ty.T_Int, AST.E_Lit(Literal.Int n)) =
          AST.E_Lit(Literal.Real(RealLit.fromInt n))
      | coerceExp (ty1, ty2, e) = AST.E_Coerce{srcTy=ty2, dstTy=ty1, e=e}

    fun coerceType (dstTy, (e, srcTy)) = (case Unify.matchType(dstTy, srcTy)
           of Unify.EQ => SOME e
            | Unify.COERCE => SOME(coerceExp (dstTy, srcTy, e))
            | Unify.FAIL => NONE
          (* end case *))

    fun coerceType2 ((e1, ty1), (e2, ty2)) = (
        (* first try to coerce ty2 to ty1 *)
          case Unify.matchType (ty1, ty2)
           of Unify.EQ => SOME(e1, e2, ty1)
            | Unify.COERCE => SOME(e1, coerceExp (ty1, ty2, e2), ty1)
            | Unify.FAIL => (
              (* try to coerce ty1 to ty2 *)
                case Unify.matchType (ty2, ty1)
                 of Unify.EQ => raise Fail "impossible"
                  | Unify.COERCE => SOME(coerceExp (ty2, ty1, e1), e2, ty2)
                  | Unify.FAIL => NONE
                (* end case *))
          (* end case *))

    fun coerceTypes [] = NONE
      | coerceTypes [ty] = SOME ty
      | coerceTypes (ty::tys) = let
          fun lp (commonTy, []) = SOME commonTy
            | lp (commonTy, ty::tys) = (case Unify.matchType (commonTy, ty)
                 of Unify.FAIL => (case Unify.matchType (ty, commonTy)
                       of Unify.COERCE => lp (ty, tys) (* can coerce commonTy to ty *)
                        | _ => NONE
                      (* end case *))
                  | _ => lp (commonTy, tys) (* can coerce ty to commonTy *)
                (* end case *))
          in
            lp (ty, tys)
          end

  end
