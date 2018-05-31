(* simple.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * A simplified AST representation of a Diderot program.  This representation has the property
 * that the arguments to ifs, operators, etc. are variables and that the rhs of assignments
 * consist of a single operation.  It is not, however, a single-assignment representation.
 *)

structure Simple =
  struct

    type var = SimpleVar.t
    type func = SimpleFunc.t

  (* types with the meta variables resolved *)
    type ty = SimpleTypes.ty

  (* resolved meta-variable arguments to basis functions *)
    datatype meta_arg = datatype SimpleTypes.meta_arg

    datatype input_init = datatype Inputs.input_init

    datatype input = datatype Inputs.input

    datatype program = Program of {
        props : Properties.t list,
        consts : var list,              (* constant variables *)
        inputs : var input list,        (* input globals *)
        constInit : block,              (* code that initializes constants and inputs *)
        globals : var list,             (* non-input globals *)
        globInit : block,               (* code to initialize global variables *)
        funcs : func_def list,          (* user-defined functions *)
        strand : strand,                (* the strand definition *)
        create : block Create.t,        (* initial strand creation *)
        start : block option,           (* global start block *)
        update : block option           (* global update block *)
      }

    and func_def = Func of {
        f : func,
        params : var list,
        body : block
      }

    and strand = Strand of {
        name : Atom.atom,
        params : var list,
        spatialDim : int option,                (* spatial dimension for queries *)
        state : var list,
        stateInit : block,
        startM : block option,
        updateM : block,
        stabilizeM : block option
      }

    and block = Block of {
        code : stmt list,                       (* the body of the block *)
        props : PropList.holder                 (* property list *)
      }

    and stmt
      = S_Var of var * exp option               (* introduce a local variable.  The uninitialized *)
                                                (* form is needed for the results of conditional *)
                                                (* expressions *)
      | S_Assign of var * exp
      | S_IfThenElse of var * block * block
      | S_Foreach of var * var * block
      | S_New of Atom.atom * var list
      | S_KillAll
      | S_StabilizeAll
      | S_Continue
      | S_Die
      | S_Stabilize
      | S_Return of var
      | S_Print of var list
      | S_MapReduce of map_reduce list          (* parallel, and possibly fused, map-reduce *)

    and map_reduce = MapReduce of {
          result : var,                         (* the result of the reduction phase *)
          reduction : Reductions.t,             (* the reduction operator *)
          mapf : func_def,                      (* a function that implements the map part *)
          args : var list,                      (* the invariant arguments to the map phase *)
          source : var,                         (* the argument that is bound to the strand *)
          domain : StrandSets.t                 (* the domain of the map phase *)
        }

    and exp
      = E_Var of var
      | E_Lit of Literal.t
      | E_Kernel of Kernel.t
      | E_Select of var * var                   (* strand-field selection *)
      | E_Apply of func * var list              (* user-defined function *)
      | E_Prim of AST.var * meta_arg list * var list * ty
                                                (* Diderot builtin operator *)
      | E_Tensor of var list * ty
      | E_Field of var list * ty
      | E_Seq of var list * ty                  (* sequence (ty is result type) *)
      | E_Tuple of var list                     (* tuple of values *)
      | E_Project of var * int                  (* project out tuple member *)
      | E_Slice of var * int option list * ty   (* tensor/field slicing (ty is result type) *)
      | E_Coerce of {srcTy : ty, dstTy : ty, x : var}
      | E_BorderCtl of var BorderCtl.t * var    (* border-control wrapper for image *)
      | E_LoadSeq of ty * string
      | E_LoadImage of ty * string * ImageInfo.t
      | E_InsideImage of var * var * int        (* inside-image test; introduced by the
                                                 * simplify-fields pass. The third argument is
                                                 * the maximum support of the image.
                                                 *)
      | E_FieldFn of func                       (* lift a differentiable field function to a
                                                 * field value.
                                                 *)

    fun typeOf (E_Var x) = SimpleVar.typeOf x
      | typeOf (E_Lit lit) = (case lit
           of (Literal.Int _) => SimpleTypes.T_Int
            | (Literal.Real _) => SimpleTypes.T_Tensor[]
            | (Literal.String s) => SimpleTypes.T_String
            | (Literal.Bool _) => SimpleTypes.T_Bool
          (* end case *))
      | typeOf (E_Kernel k) = SimpleTypes.T_Kernel
      | typeOf (E_Select(_, fld)) = SimpleVar.typeOf fld
      | typeOf (E_Apply(f, _)) = SimpleFunc.resultTypeOf f
      | typeOf (E_Prim(f, _, _, ty)) = ty
      | typeOf (E_Tensor(_, ty)) = ty
      | typeOf (E_Field(_, ty)) = ty
      | typeOf (E_Seq(_, ty)) = ty
      | typeOf (E_Tuple xs) = SimpleTypes.T_Tuple(List.map SimpleVar.typeOf xs)
      | typeOf (E_Project(x, i)) = let
          val SimpleTypes.T_Tuple tys = SimpleVar.typeOf x
          in
            List.nth(tys, i)
          end
      | typeOf (E_Slice(_, _, ty)) = ty
      | typeOf (E_Coerce{dstTy, ...}) = dstTy
      | typeOf (E_BorderCtl(_, x)) = SimpleVar.typeOf x
      | typeOf (E_LoadSeq(ty, _)) = ty
      | typeOf (E_LoadImage(ty, _, _)) = ty
      | typeOf (E_InsideImage _) = SimpleTypes.T_Bool
      | typeOf (E_FieldFn f) = let
          val (dim, shp) = (case SimpleFunc.typeOf f
                 of (SimpleTypes.T_Tensor shp, [SimpleTypes.T_Tensor[]]) => (1, shp)
                  | (SimpleTypes.T_Tensor shp, [SimpleTypes.T_Tensor[d]]) => (d, shp)
                (* end case *))
          in
            SimpleTypes.T_Field{diff = NONE, dim = dim, shape = shp}
          end

    fun newProp initFn = PropList.newProp (fn (Block{props, ...}) => props, initFn)
    fun newFlag () = PropList.newFlag (fn (Block{props, ...}) => props)

  end
