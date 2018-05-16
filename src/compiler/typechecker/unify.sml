(* unify.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Unify : sig

  (* when matching two types (ty1 and ty2), there are three possible outcomes:
   *   EQ       -- types are equal
   *   COERCE   -- ty2 can be coerced to match ty1 (e.g., int -> float, fixed seq -> dynamic seq)
   *   FAIL     -- types do not match
   *)
    datatype match = EQ | COERCE | FAIL

    val matchType : Types.ty * Types.ty -> match

    val tryMatchType : Types.ty * Types.ty -> match

  (* attempt to match a list of parameter types with a list of typed arguments.  Return
   * the arguments with any required coercions, or NONE on failure.
   *)
    val matchArgs : Types.ty list * AST.expr list * Types.ty list -> AST.expr list option
    val tryMatchArgs : Types.ty list * AST.expr list * Types.ty list -> AST.expr list option

    val equalType : Types.ty * Types.ty -> bool
    val equalTypes : Types.ty list * Types.ty list -> bool

    val tryEqualType : Types.ty * Types.ty -> bool
    val tryEqualTypes : Types.ty list * Types.ty list -> bool

    val equalDim : Types.dim * Types.dim -> bool

  end = struct

    structure Ty = Types
    structure MV = MetaVar
    structure TU = TypeUtil

    datatype match = EQ | COERCE | FAIL

  (* a patch list tracks the meta variables that have been updated so that we can undo
   * the effects of unification when just testing for a possible type match.
   *)

    fun bindTyVar (pl, tv as Ty.TV{bind as ref NONE, ...}, ty) = (
          bind := SOME ty;
          pl := Ty.TYPE tv :: !pl)
      | bindTyVar _ = raise Fail "rebinding type variable"

    fun bindDiffVar (pl, dv as Ty.DfV{bind as ref NONE, ...}, diff) = (
          bind := SOME diff;
          pl := Ty.DIFF dv :: !pl)
      | bindDiffVar _ = raise Fail "rebinding differentiation variable"

    fun bindShapeVar (pl, sv as Ty.SV{bind as ref NONE, ...}, shape) = (
          bind := SOME shape;
          pl := Ty.SHAPE sv :: !pl)
      | bindShapeVar _ = raise Fail "rebinding shape variable"

    fun bindDimVar (pl, dv as Ty.DV{bind as ref NONE, ...}, dim) = (
          bind := SOME dim;
          pl := Ty.DIM dv :: !pl)
      | bindDimVar _ = raise Fail "rebinding dimension variable"

    fun undo pl = let
          fun undo1 (Ty.TYPE(Ty.TV{bind, ...})) = bind := NONE
            | undo1 (Ty.DIFF(Ty.DfV{bind, ...})) = bind := NONE
            | undo1 (Ty.SHAPE(Ty.SV{bind, ...})) = bind := NONE
            | undo1 (Ty.DIM(Ty.DV{bind, ...})) = bind := NONE
          in
            List.map undo1 (!pl)
          end

(* FIXME: what about the bounds? *)
    fun equalDiff (pl, diff1, diff2) = (case (TU.pruneDiff diff1, TU.pruneDiff diff2)
           of (Ty.DiffConst NONE, Ty.DiffConst NONE) => true
            | (Ty.DiffConst(SOME k1), Ty.DiffConst(SOME k2)) => (k1 = k2)
            | (Ty.DiffConst(SOME k), Ty.DiffVar(dv, i)) => let
                val k' = k+i
                in
                  if k' < 0 then false
                  else (bindDiffVar(pl, dv, Ty.DiffConst(SOME k')); true)
                end
            | (Ty.DiffVar(dv, i), Ty.DiffConst(SOME k)) => let
                val k' = k+i
                in
                  if k' < 0 then false
                  else (bindDiffVar(pl, dv, Ty.DiffConst(SOME k')); true)
                end
            | (Ty.DiffVar(dv1, 0), diff2 as Ty.DiffVar(dv2, i2)) => (
                bindDiffVar(pl, dv1, diff2); true)
            | (diff1 as Ty.DiffVar(dv1, i1), Ty.DiffVar(dv2, 0)) => (
                bindDiffVar(pl, dv2, diff1); true)
            | (Ty.DiffVar(dv1, i1), Ty.DiffVar(dv2, i2)) =>
                raise Fail(concat[
                    "equalDiff(", TU.diffToString diff1, ", ",
                    TU.diffToString diff2, ") unimplemented"
                  ])
            | _ => false
          (* end case *))

  (* match two differentiation constants where the first is allowed to be less than the second *)
    fun matchDiff (pl, diff1, diff2) = (case (TU.pruneDiff diff1, TU.pruneDiff diff2)
           of (Ty.DiffConst _, Ty.DiffConst NONE) => true
            | (Ty.DiffConst NONE, Ty.DiffConst _) => false
            | (Ty.DiffConst(SOME k1), Ty.DiffConst(SOME k2)) => (k1 <= k2)
            | (Ty.DiffVar(dv1, k1), Ty.DiffVar(dv2, k2)) =>
                MV.sameDiffVar(dv1, dv2) andalso (k1 <= k2)
            | _ => equalDiff (pl, diff1, diff2)  (* force equality *)
          (* end case *))

    fun equalDim (pl, dim1, dim2) = (case (TU.pruneDim dim1, TU.pruneDim dim2)
           of (Ty.DimConst d1, Ty.DimConst d2) => (d1 = d2)
            | (Ty.DimVar dv, dim2) => (bindDimVar(pl, dv, dim2); true)
            | (dim1, Ty.DimVar dv) => (bindDimVar(pl, dv, dim1); true)
          (* end case *))

    fun equalShape (pl, shape1, shape2) = (case (TU.pruneShape shape1, TU.pruneShape shape2)
           of (Ty.Shape[], Ty.Shape[Ty.DimVar dv]) => ( (* tensor[] == tensor[1] *)
                bindDimVar(pl, dv, Ty.DimConst 1); true)
(*
            | (Ty.Shape[], Ty.ShapeExt(Ty.ShapeVar sv, Ty.DimVar dv)) => (
                bindShapeVar (pl, sv, Ty.Shape[]); bindDimVar(pl, dv, Ty.DimConst 1); true)
*)
            | (Ty.Shape[Ty.DimVar dv], Ty.Shape[]) => (
                bindDimVar(pl, dv, Ty.DimConst 1); true)
(*
            | (Ty.ShapeExt(Ty.ShapeVar sv, Ty.DimVar dv), Ty.Shape[]) => (
                bindShapeVar (pl, sv, Ty.Shape[]); bindDimVar(pl, dv, Ty.DimConst 1); true)
*)
            | (Ty.Shape dd1, Ty.Shape dd2) => let
                fun chk ([], []) = true
                  | chk (d1::dd1, d2::dd2) = equalDim(pl, d1, d2) andalso chk (dd1, dd2)
                  | chk _ = false
                in
                  chk (dd1, dd2)
                end
            | (Ty.Shape dd, Ty.ShapeExt(shape, d2)) => let
                fun chk ([], _) = false
                  | chk ([d], revDD) =
                      equalDim(pl, d, d2) andalso equalShape(pl, Ty.Shape(List.rev revDD), shape)
                  | chk (d::dd, revDD) = chk(dd, d::revDD)
                in
                  chk (dd, [])
                end
            | (Ty.ShapeVar sv, shape) => (bindShapeVar (pl, sv, shape); true)
            | (Ty.ShapeExt(shape1, d1), Ty.ShapeExt(shape2, d2)) =>
                equalDim(pl, d1, d2) andalso equalShape(pl, shape1, shape2)
            | (shape1, shape2) => equalShape(pl, shape2, shape1)
        (* end case *))

(* QUESTION: do we need an occurs check? *)
    fun unifyType (pl, ty1, ty2) = let
          fun matchVar (tv1 as Ty.TV{id=id1, bind=b1}, tv2 as Ty.TV{id=id2, bind=b2}) =
                if Stamp.same(id1, id2)
                  then true
                  else (case (!b1, !b2)
                     of (SOME ty1, SOME ty2) => match(ty1, ty2)
                      | (SOME ty1, NONE) => (bindTyVar (pl, tv2, ty1); true)
                      | (NONE, SOME ty2) => (bindTyVar (pl, tv1, ty2); true)
                      | (NONE, NONE) => (bindTyVar (pl, tv1, Ty.T_Var tv2); true)
                    (* end case *))
          and matchVarTy (tv as Ty.TV{bind, ...}, ty) = (case !bind
                 of NONE => (bindTyVar(pl, tv, ty); true)
                  | SOME ty' => match(ty', ty)
                (* end case *))
          and match (Ty.T_Var tv1, Ty.T_Var tv2) = matchVar(tv1, tv2)
            | match (Ty.T_Var tv1, ty2) = matchVarTy(tv1, ty2)
            | match (ty1, Ty.T_Var tv2) = matchVarTy(tv2, ty1)
            | match (Ty.T_Bool, Ty.T_Bool) = true
            | match (Ty.T_Int, Ty.T_Int) = true
            | match (Ty.T_String, Ty.T_String) = true
            | match (Ty.T_Sequence(ty1, NONE), Ty.T_Sequence(ty2, NONE)) = match(ty1, ty2)
            | match (Ty.T_Sequence(ty1, SOME d1), Ty.T_Sequence(ty2, SOME d2)) =
                equalDim(pl, d1, d2) andalso match(ty1, ty2)
            | match (Ty.T_Strand s1, Ty.T_Strand s2) = Atom.same(s1, s2)
            | match (Ty.T_Kernel k1, Ty.T_Kernel k2) = equalDiff (pl, k1, k2)
            | match (Ty.T_Tensor s1, Ty.T_Tensor s2) = equalShape (pl, s1, s2)
            | match (Ty.T_Image{dim=d1, shape=s1}, Ty.T_Image{dim=d2, shape=s2}) =
                equalDim (pl, d1, d2) andalso equalShape(pl, s1, s2)
            | match (Ty.T_Field{diff=k1, dim=d1, shape=s1}, Ty.T_Field{diff=k2, dim=d2, shape=s2}) =
                equalDiff (pl, k1, k2) andalso equalDim (pl, d1, d2) andalso equalShape(pl, s1, s2)
            | match (Ty.T_Fun(tys11, ty12), Ty.T_Fun(tys21, ty22)) =
                ListPair.allEq match (tys11, tys21) andalso match (ty12, ty22)
            | match (Ty.T_Error, _) = true
            | match (_, Ty.T_Error) = true
            | match _ = false
          in
            match (TU.pruneHead ty1, TU.pruneHead ty2)
          end

  (* try to unify ty1 and ty2, where we allow ty2 to be coerced to ty1 *)
    fun unifyTypeWithCoercion (pl, ty1, ty2) = (case (TU.pruneHead ty1, TU.pruneHead ty2)
           of (Ty.T_Tensor shp, Ty.T_Int) =>
                if equalShape (pl, Ty.Shape[], shp) then COERCE else FAIL
            | (Ty.T_Sequence(ty1, SOME d1), Ty.T_Sequence(ty2, SOME d2)) =>
                if equalDim(pl, d1, d2)
                  then unifyTypeWithCoercion(pl, ty1, ty2)
                  else FAIL
            | (Ty.T_Sequence(ty1, NONE), Ty.T_Sequence(ty2, SOME _)) => (
                case unifyTypeWithCoercion(pl, ty1, ty2)
                 of EQ => COERCE
                  | result => result
                (* end case *))
            | (Ty.T_Field{diff=k1, dim=d1, shape=s1}, Ty.T_Field{diff=k2, dim=d2, shape=s2}) =>
                if unifyType(pl, ty1, ty2)
                  then EQ
                else if matchDiff (pl, k1, k2) andalso equalDim(pl, d1, d2)
                andalso equalShape(pl, s1, s2)
                  then COERCE
                  else FAIL
            | (ty1, ty2) => if unifyType(pl, ty1, ty2) then EQ else FAIL
          (* end case *))
(* +DEBUG *
val unifyTypeWithCoercion = fn (pl, ty1, ty2) => let
  val res = unifyTypeWithCoercion (pl, ty1, ty2)
  val res' = (case res of EQ => "EQ" | COERCE => "COERCE" | FAIL => "FAIL")
  in
    print(concat["unifyTypeWithCoercion (_, ", TU.toString ty1, ", ", TU.toString ty2, ") = ", res', "\n"]);
    res
  end
* -DEBUG *)

    fun equalTypes (tys1, tys2) = let
          val pl = ref[]
          in
            ListPair.allEq (fn (ty1, ty2) => unifyType(pl, ty1, ty2)) (tys1, tys2)
          end

    fun equalType (ty1, ty2) = unifyType (ref[], ty1, ty2)

  (* try to match types; if we fail, all meta-variable bindings are undone *)
    fun tryEqualType (ty1, ty2) = let
          val pl = ref[]
          in
            unifyType(pl, ty1, ty2) orelse (undo pl; false)
          end

  (* try to unify two types to equality; if we fail, all meta-variable bindings are undone *)
    fun tryEqualTypes (tys1, tys2) = let
          val pl = ref[]
          in
            ListPair.allEq (fn (ty1, ty2) => unifyType(pl, ty1, ty2)) (tys1, tys2)
            orelse (undo pl; false)
          end

    fun matchType (ty1, ty2) = unifyTypeWithCoercion (ref[], ty1, ty2)

  (* try to unify two type lists to equality; if we fail, all meta-variable bindings are undone *)
    fun tryMatchType (ty1, ty2) = let
          val pl = ref[]
          in
            case unifyTypeWithCoercion (pl, ty1, ty2)
             of FAIL => (undo pl; FAIL)
              | result => result
            (* end case *)
          end

  (* attempt to match a list of parameter types with a list of typed arguments.  Return
   * the arguments with any required coercions, or NONE on failure.
   *)
    local
      fun matchArgs' (pl, paramTys, args, argTys) = let
            fun matchArgTys ([], [], [], args') = SOME(List.rev args')
              | matchArgTys (ty1::tys1, arg::args, ty2::tys2, args') = (
                  case unifyTypeWithCoercion (pl, ty1, ty2)
                   of EQ => matchArgTys (tys1, args, tys2, arg::args')
                    | COERCE => matchArgTys (tys1, args, tys2, AST.E_Coerce{srcTy=ty2, dstTy=ty1, e=arg}::args')
                    | _ => (undo pl; NONE)
                  (* end case *))
                | matchArgTys _ = NONE
            in
              matchArgTys (paramTys, args, argTys, [])
            end
    in
    fun matchArgs (paramTys, args, argTys) = matchArgs' (ref[], paramTys, args, argTys)
    fun tryMatchArgs (paramTys, args, argTys) = let
          val pl = ref[]
          in
            case matchArgs' (ref[], paramTys, args, argTys)
             of NONE => (undo pl; NONE)
              | someResult => someResult
            (* end case *)
          end
    end

  (* rebind equalDim without patch-list argument *)
    val equalDim = fn (d1, d2) => equalDim(ref [], d1, d2)

  end
