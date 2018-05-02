(* type-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure TypeUtil : sig

  (* constructor for building a tensor type of known order, but unknown
   * dimensions.
   *)
    val mkTensorTy : int -> Types.ty

  (* constructor for building a fixed-size sequence type of unknown size *)
    val mkSequenceTy : Types.ty -> Types.ty

  (* function to compute the slice of a tensor type based on a boolean
   * mask.  The value true in the mask means that the corresponding
   * dimension is being indexed, while false means that it is being
   * copied.
   *)
    val slice : Types.ty * bool list -> Types.ty

(* FIXME: make terminology be consistent between documentation and implementation w.r.t.
 * the kinds of types (e.g., concrete type, value type, ...)
 *)
  (* returns true if the type is a value type; i.e., a basic value type (bool, int,
   * string, or tensor), or a sequence of values.
   *)
    val isValueType : Types.ty -> bool

  (* returns true if the type is a value type, or a strand type, or a sequence of such types *)
    val isValueOrStrandType : Types.ty -> bool

  (* return true if the type is an image type *)
    val isImageType : Types.ty -> bool

  (* return true if the type is T_Error *)
    val isErrorType : Types.ty -> bool

  (* return the range (return type) of a function type *)
    val rngOf : Types.ty -> Types.ty

  (* prune out instantiated meta variables from a type.  We also normalize
   * tensor shapes (i.e., remove 1s).
   *)
    val prune : Types.ty -> Types.ty
    val pruneDiff : Types.diff -> Types.diff
    val pruneShape : Types.shape -> Types.shape
    val pruneDim : Types.dim -> Types.dim

  (* prune the head of a type *)
    val pruneHead : Types.ty -> Types.ty

  (* resolve meta variables to their instantiations (or else variable) *)
    val resolve : Types.ty_var -> Types.ty
    val resolveDiff : Types.diff_var -> Types.diff
    val resolveShape : Types.shape_var -> Types.shape
    val resolveDim : Types.dim_var -> Types.dim
    val resolveVar : Types.meta_var -> Types.var_bind

  (* equality testing *)
    val sameDim : Types.dim * Types.dim -> bool

  (* string representations of types, etc *)
    val toString : Types.ty -> string
    val diffToString : Types.diff -> string
    val shapeToString : Types.shape -> string
    val dimToString : Types.dim -> string

  (* convert to fully resolved monomorphic forms *)
    val monoDim : Types.dim -> int
    val monoShape : Types.shape -> int list
    val monoDiff : Types.diff -> int

  (* instantiate a type scheme, returning the argument meta variables and the resulting type.
   * Note that we assume that the scheme is closed.
   *)
    val instantiate : Types.scheme -> (Types.meta_var list * Types.ty)

  end = struct

    structure Ty = Types
    structure MV = MetaVar

  (* constructor for building a tensor type of known order, but unknown
   * dimensions.
   *)
    fun mkTensorTy order =
          Ty.T_Tensor(
            Ty.Shape(List.tabulate(order, fn _ => Ty.DimVar(MetaVar.newDimVar()))))

    fun mkSequenceTy ty = Ty.T_Sequence(ty, SOME(Ty.DimVar(MetaVar.newDimVar())))

  (* prune out instantiated meta variables from a type.  We also normalize
   * tensor dimensions (i.e., remove 1s).
   *)
    fun prune ty = (case ty
           of (ty as Ty.T_Var(Ty.TV{bind, ...})) => (case !bind
                 of NONE => ty
                  | SOME ty => prune ty
                (* end case *))
            | Ty.T_Sequence(ty, NONE) => Ty.T_Sequence(prune ty, NONE)
            | Ty.T_Sequence(ty, SOME dim) => Ty.T_Sequence(prune ty, SOME(pruneDim dim))
            | (Ty.T_Kernel diff) => Ty.T_Kernel(pruneDiff diff)
            | (Ty.T_Tensor shape) => Ty.T_Tensor(pruneShape shape)
            | (Ty.T_Image{dim, shape}) => Ty.T_Image{
                  dim = pruneDim dim,
                  shape = pruneShape shape
                }
            | (Ty.T_Field{diff, dim, shape}) => Ty.T_Field{
                  diff = pruneDiff diff,
                  dim = pruneDim dim,
                  shape = pruneShape shape
                }
            | (Ty.T_Fun(tys1, ty2)) => Ty.T_Fun(List.map prune tys1, prune ty2)
            | ty => ty
          (* end case *))

    and pruneDiff (Ty.DiffVar(Ty.DfV{bind=ref(SOME diff), ...}, i)) = (
          case pruneDiff diff
           of Ty.DiffVar(dv, i') => Ty.DiffVar(dv, i+i')
            | Ty.DiffConst i' => Ty.DiffConst(i+i')
          (* end case *))
      | pruneDiff diff = diff

    and pruneDim dim = (case dim
           of Ty.DimVar(Ty.DV{bind=ref(SOME dim), ...}) => pruneDim dim
            | dim => dim
          (* end case *))

    and filterDim dim = (case pruneDim dim
           of Ty.DimConst 1 => NONE
            | dim => SOME dim
          (* end case *))

    and pruneShape shape = (case shape
           of Ty.Shape dd => Ty.Shape(List.mapPartial filterDim dd)
            | Ty.ShapeVar(Ty.SV{bind=ref(SOME shape), ...}) => pruneShape shape
            | Ty.ShapeExt(shape, dim) => (case filterDim dim
                 of SOME dim => Ty.shapeExt(pruneShape shape, dim)
                  | NONE => pruneShape shape
                (* end case *))
            | _ => shape
          (* end case *))

  (* resolve meta variables to their instantiations (or else variable) *)
    fun resolve (tv as Ty.TV{bind, ...}) = (case !bind
           of NONE => Ty.T_Var tv
            | SOME ty => prune ty
          (* end case *))

    fun resolveDiff (dv as Ty.DfV{bind, ...}) = (case !bind
           of NONE => Ty.DiffVar(dv, 0)
            | SOME diff => pruneDiff diff
          (* end case *))

    fun resolveShape (sv as Ty.SV{bind, ...}) = (case !bind
           of NONE => Ty.ShapeVar sv
            | SOME shape => pruneShape shape
          (* end case *))

    fun resolveDim (dv as Ty.DV{bind, ...}) = (case !bind
           of NONE => Ty.DimVar dv
            | SOME dim => pruneDim dim
          (* end case *))

    fun resolveVar (Ty.TYPE tv) = Ty.TYPE(resolve tv)
      | resolveVar (Ty.DIFF dv) = Ty.DIFF(resolveDiff dv)
      | resolveVar (Ty.SHAPE sv) = Ty.SHAPE(resolveShape sv)
      | resolveVar (Ty.DIM d) = Ty.DIM(resolveDim d)

  (* prune the head of a type *)
    fun pruneHead ty = let
          fun prune' (ty as Ty.T_Var(Ty.TV{bind, ...})) = (case !bind
                 of NONE => ty
                  | SOME ty => prune' ty
                (* end case *))
            | prune' (Ty.T_Sequence(ty, NONE)) = Ty.T_Sequence(ty, NONE)
            | prune' (Ty.T_Sequence(ty, SOME dim)) = Ty.T_Sequence(ty, SOME(pruneDim dim))
            | prune' (Ty.T_Kernel diff) = Ty.T_Kernel(pruneDiff diff)
            | prune' (Ty.T_Tensor shape) = Ty.T_Tensor(pruneShape shape)
            | prune' (Ty.T_Image{dim, shape}) = Ty.T_Image{
                  dim = pruneDim dim,
                  shape = pruneShape shape
                }
            | prune' (Ty.T_Field{diff, dim, shape}) = Ty.T_Field{
                  diff = pruneDiff diff,
                  dim = pruneDim dim,
                  shape = pruneShape shape
                }
            | prune' ty = ty
          in
            prune' ty
          end

  (* helper function for isValueType and isValueOrStrandType; checks for fixed-size types
   * inside a dynamic sequence (i.e., no nested dynamic sequences).
   *)
    fun isFixedSize (allowStrand, ty) = (case ty
           of Ty.T_Bool => true
            | Ty.T_Int => true
            | Ty.T_String => true
            | Ty.T_Sequence(ty, SOME _) => isFixedSize (allowStrand, ty)
            | Ty.T_Strand _ => allowStrand
            | Ty.T_Tensor _ => true
            | Ty.T_Error => true
            | _ => false
          (* end case *))

  (* returns true if the type is a value type; i.e., a basic value type (bool, int,
   * string, or tensor), or a sequence of values.
   *)
    fun isValueType ty = (case prune ty
           of Ty.T_Bool => true
            | Ty.T_Int => true
            | Ty.T_String => true
            | Ty.T_Sequence(ty, SOME _) => isValueType ty
            | Ty.T_Sequence(ty, NONE) => isFixedSize (false, ty)
            | Ty.T_Tensor _ => true
            | Ty.T_Error => true
            | _ => false
          (* end case *))

  (* returns true if the type is a value type, or a strand type, or a sequence of such types *)
    fun isValueOrStrandType ty = (case prune ty
           of Ty.T_Bool => true
            | Ty.T_Int => true
            | Ty.T_String => true
            | Ty.T_Sequence(ty, SOME _) => isValueOrStrandType ty
            | Ty.T_Sequence(ty, NONE) => isFixedSize (true, ty)
            | Ty.T_Strand _ => true
            | Ty.T_Tensor _ => true
            | Ty.T_Error => true
            | _ => false
          (* end case *))

  (* returns true if the type is an ImageTy *)
    fun isImageType ty = (case prune ty
           of Ty.T_Image _ => true
            | Ty.T_Error => true
            | _ => false
          (* end case *))

    fun isErrorType ty = (case prune ty
           of Ty.T_Error => true
            | _ => false
          (* end case *))

  (* equality testing *)
    fun sameDim (Ty.DimConst d1, Ty.DimConst d2) = (d1 = d2)
      | sameDim (Ty.DimVar v1, Ty.DimVar v2) = MetaVar.sameDimVar(v1, v2)
      | sameDim _ = false

    fun listToString fmt sep items = String.concatWith sep (List.map fmt items)

    fun diffToString diff = (case pruneDiff diff
           of Ty.DiffConst n => Int.toString n
            | Ty.DiffVar(dv, 0) => MV.diffVarToString dv
            | Ty.DiffVar(dv, i) => if i < 0
                then String.concat["(", MV.diffVarToString dv, "-", Int.toString(~i), ")"]
                else String.concat["(", MV.diffVarToString dv, "+", Int.toString i, ")"]
          (* end case *))

    fun shapeToString shape = (case pruneShape shape
           of Ty.Shape shape => concat["[", listToString dimToString "," shape, "]"]
            | Ty.ShapeVar sv => concat["[", MV.shapeVarToString sv, "]"]
            | Ty.ShapeExt(shape, d) => let
                fun toS (Ty.Shape shape) = (listToString dimToString "," shape) ^ ","
                  | toS (Ty.ShapeVar sv) = MV.shapeVarToString sv ^ ";"
                  | toS (Ty.ShapeExt(shape, d)) = concat[toS shape, dimToString d, ","]
                in
                  concat["[", toS shape, dimToString d, "]"]
                end
          (* end case *))

    and dimToString dim = (case pruneDim dim
           of Ty.DimConst n => Int.toString n
            | Ty.DimVar v => MV.dimVarToString v
          (* end case *))

    fun toString ty = (case pruneHead ty
           of Ty.T_Var(Ty.TV{bind=ref(SOME ty), ...}) => toString ty
            | Ty.T_Var tv => MV.tyVarToString tv
            | Ty.T_Bool => "bool"
            | Ty.T_Int => "int"
            | Ty.T_String => "string"
            | Ty.T_Sequence(ty, NONE) => concat[toString ty, "[]"]
            | Ty.T_Sequence(ty, SOME dim) => concat[toString ty, "[", dimToString dim, "]"]
            | Ty.T_Strand id => Atom.toString id
            | Ty.T_Kernel n => "kernel#" ^ diffToString n
            | Ty.T_Tensor(Ty.Shape[]) => "real"
            | Ty.T_Tensor(Ty.Shape[Ty.DimConst 2]) => "vec2"
            | Ty.T_Tensor(Ty.Shape[Ty.DimConst 3]) => "vec3"
            | Ty.T_Tensor(Ty.Shape[Ty.DimConst 4]) => "vec4"
            | Ty.T_Tensor(Ty.Shape[Ty.DimConst 2, Ty.DimConst 2]) => "mat2"
            | Ty.T_Tensor(Ty.Shape[Ty.DimConst 3, Ty.DimConst 3]) => "mat3"
            | Ty.T_Tensor(Ty.Shape[Ty.DimConst 4, Ty.DimConst 4]) => "mat4"
            | Ty.T_Tensor shape => "tensor" ^ shapeToString shape
            | Ty.T_Image{dim, shape} => concat[
                  "image(", dimToString dim, ")", shapeToString shape
                ]
            | Ty.T_Field{diff, dim, shape} => concat[
                  "field#", diffToString diff, "(", dimToString dim,
                  ")", shapeToString shape
                ]
            | Ty.T_Fun(tys1, ty2) => let
                fun tysToString [] = "()"
                  | tysToString [ty] = toString ty
                  | tysToString tys = String.concat[
                        "(", listToString toString " * " tys, ")"
                      ]
                in
                  String.concat[tysToString tys1, " -> ", toString ty2]
                end
            | Ty.T_Error => "<error-type>"
          (* end case *))

  (* return the range (return type) of a function type *)
    fun rngOf (Ty.T_Fun(_, ty)) = ty
      | rngOf ty = raise Fail(concat["TypeUtil.rngOf(", toString ty, ")"])

    fun slice (Ty.T_Tensor(Ty.Shape l), mask) = let
          fun f (d, true, dd) = dd
            | f (d, false, dd) = d::dd
          in
            Ty.T_Tensor(Ty.Shape(ListPair.foldr f [] (l, mask)))
          end
      | slice (Ty.T_Field{shape as Ty.Shape l,diff,dim}, mask) = let
          fun f (d, true, dd) = dd
            | f (d, false, dd) = d::dd
          in
            Ty.T_Field{diff=diff, dim=dim, shape= Ty.Shape (ListPair.foldr f [] (l, mask))}
          end
      | slice (ty, _) = raise Fail(concat["slice(", toString ty, ", _)"])

  (* convert to fully resolved monomorphic forms *)
    fun monoDim dim = (case pruneDim dim
           of Ty.DimConst d => d
            | dim => raise Fail(concat["dim ", dimToString dim, " is not constant"])
          (* end case *))

    fun monoShape shp = (case pruneShape shp
           of Ty.Shape shp => List.map monoDim shp
            | shp => raise Fail(concat["shape ", shapeToString shp, " is not constant"])
          (* end case *))

    fun monoDiff diff = (case pruneDiff diff
           of Ty.DiffConst k => k
            | diff => raise Fail(concat["diff ", diffToString diff, " is not constant"])
          (* end case *))

  (* instantiate a type scheme, returning the argument meta variables and the resulting type.
   * Note that we assume that the scheme is closed.
   *)
    fun instantiate ([], ty) = ([], ty)
      | instantiate (mvs, ty) = let
          fun instantiateVar (mv, (mvs, env)) = let
                val mv' = MV.copy mv
                in
                  (mv'::mvs, MV.Map.insert(env, mv, mv'))
                end
          val (mvs, env) = List.foldr instantiateVar ([], MV.Map.empty) mvs
          fun iDiff (Ty.DiffVar(k, i)) = (case MV.Map.find(env, Ty.DIFF k)
                 of SOME(Ty.DIFF k) => Ty.DiffVar(k, i)
                  | _ => raise Fail "impossible"
                (* end case *))
            | iDiff diff = diff
          fun iDim (Ty.DimVar dv) = (case MV.Map.find(env, Ty.DIM dv)
                 of SOME(Ty.DIM dv) => Ty.DimVar dv
                  | _ => raise Fail "impossible"
                (* end case *))
            | iDim dim = dim
          fun iShape (Ty.ShapeVar sv) = (case MV.Map.find(env, Ty.SHAPE sv)
                 of SOME(Ty.SHAPE sv) => Ty.ShapeVar sv
                  | _ => raise Fail "impossible"
                (* end case *))
            | iShape (Ty.ShapeExt(shape, dim)) = Ty.ShapeExt(iShape shape, iDim dim)
            | iShape (Ty.Shape dims) = Ty.Shape(List.map iDim dims)
          fun ity (Ty.T_Var tv) = (case MV.Map.find(env, Ty.TYPE tv)
                 of SOME(Ty.TYPE tv) => Ty.T_Var tv
                  | _ => raise Fail "impossible"
                (* end case *))
            | ity Ty.T_Bool = Ty.T_Bool
            | ity Ty.T_Int = Ty.T_Int
            | ity Ty.T_String = Ty.T_String
            | ity (Ty.T_Sequence(ty, NONE)) = Ty.T_Sequence(ity ty, NONE)
            | ity (Ty.T_Sequence(ty, SOME d)) = Ty.T_Sequence(ity ty, SOME(iDim d))
            | ity (ty as Ty.T_Strand _) = ty
            | ity (Ty.T_Kernel k) = Ty.T_Kernel(iDiff k)
            | ity (Ty.T_Tensor shape) = Ty.T_Tensor(iShape shape)
            | ity (Ty.T_Image{dim, shape}) = Ty.T_Image{dim=iDim dim, shape=iShape shape}
            | ity (Ty.T_Field{diff, dim, shape}) =
                Ty.T_Field{diff=iDiff diff, dim=iDim dim, shape=iShape shape}
            | ity (Ty.T_Fun(dom, rng)) = Ty.T_Fun(List.map ity dom, ity rng)
            | ity Ty.T_Error = Ty.T_Error
          in
            (mvs, ity ty)
          end

  end
