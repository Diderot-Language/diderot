(* meta-var.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *
 * The Diderot typechecker uses four kinds of meta variables:
 *
 *      type variables
 *      differentiation variables
 *      shape variables
 *      dimension variables
 *
 * This module implements creation, equality testing, and printing for these.
 *)

structure MetaVar =
  struct

    datatype ty_var = datatype Types.ty_var
    datatype diff_var = datatype Types.diff_var
    datatype shape_var = datatype Types.shape_var
    datatype dim_var = datatype Types.dim_var
    datatype kind = datatype Types.kind


  (***** Type variables ****)

    fun newTyVar () = TV{
            id = Stamp.new(),
            bind = ref NONE
          }

  (* create a type variable that is instantiated to a given type *)
    fun newFromType ty = TV{
            id = Stamp.new(),
            bind = ref(SOME ty)
          }

    fun tyVarToString (TV{id, ...}) = "'ty" ^ Stamp.toString id

    fun sameTyVar (TV{id=a, ...}, TV{id=b, ...}) = Stamp.same(a, b)


  (***** Differentiation variables ****)

    fun newDiffVar bnd = DfV{
            id = Stamp.new(),
            bound = ref bnd,
            bind = ref NONE
          }

    fun diffVarToString (DfV{id, ...}) = "'diff" ^ Stamp.toString id

    fun sameDiffVar (DfV{id=a, ...}, DfV{id=b, ...}) = Stamp.same(a, b)


  (***** Shape variables ****)

    fun newShapeVar () = SV{
            id = Stamp.new(),
            bind = ref NONE
          }

    fun shapeVarToString (SV{id, ...}) = "'shp" ^ Stamp.toString id

    fun sameShapeVar (SV{id=a, ...}, SV{id=b, ...}) = Stamp.same(a, b)


  (***** Dimension variables ****)

    fun newDimVar () = DV{
            id = Stamp.new(),
            bind = ref NONE
          }

    fun dimVarToString (DV{id, ...}) = "'dim" ^ Stamp.toString id

    fun sameDimVar (DV{id=a, ...}, DV{id=b, ...}) = Stamp.same(a, b)


  (***** Meta variables ****)

    fun metaToString (TYPE tv) = tyVarToString tv
      | metaToString (DIFF dv) = diffVarToString dv
      | metaToString (SHAPE sv) = shapeVarToString sv
      | metaToString (DIM dv) = dimVarToString dv

    fun stamp (TYPE(TV{id, ...})) = id
      | stamp (DIFF(DfV{id, ...})) = id
      | stamp (SHAPE(SV{id, ...})) = id
      | stamp (DIM(DV{id, ...})) = id

    fun copy (TYPE _) = TYPE(newTyVar())
      | copy (DIFF(k as DfV{bound, ...})) = DIFF(newDiffVar(!bound))
      | copy (SHAPE _) = SHAPE(newShapeVar())
      | copy (DIM _) = DIM(newDimVar())

    fun toType (TYPE(TV{bind, ...})) = (case !bind
           of SOME ty => ty
            | NONE => raise Fail "unbound type meta variable"
          (* end case *))
      | toType mv = raise Fail(concat["toType(", metaToString mv, ")"])

    fun toDiff (DIFF(DfV{bind, ...})) = (case !bind
           of SOME ty => ty
            | NONE => raise Fail "unbound diff meta variable"
          (* end case *))
      | toDiff mv = raise Fail(concat["toDiff(", metaToString mv, ")"])

    fun toShape (SHAPE(SV{bind, ...})) = (case !bind
           of SOME ty => ty
            | NONE => raise Fail "unbound shape meta variable"
          (* end case *))
      | toShape mv = raise Fail(concat["toShape(", metaToString mv, ")"])

    fun toDim (DIM(DV{bind, ...})) = (case !bind
           of SOME ty => ty
            | NONE => raise Fail "unbound dimension meta variable"
          (* end case *))
      | toDim mv = raise Fail(concat["toDim(", metaToString mv, ")"])


    structure Map = RedBlackMapFn (
      struct
        type ord_key = Types.meta_var
        fun compare (mv1, mv2) = Stamp.compare(stamp mv1, stamp mv2)
      end)

  end
