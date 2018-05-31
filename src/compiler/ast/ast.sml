(* ast.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * A typed abstract-syntax tree representation of Diderot programs.
 *)

structure AST =
  struct

    structure Ty = Types

    type var = Var.t

  (* we tag variable uses with their location for error reporting *)
    type var_use = var * Error.span

    datatype var_kind = datatype Var.kind

    datatype program = Program of {
        props : Properties.t list,
        const_dcls : var_dcl list,                      (* constant variable declarations *)
        input_dcls : (var_dcl * string option) list,    (* input variable declarations *)
        globals : global_dcl list,                      (* global variable and function declarations *)
        globInit : stmt option,                         (* additional global initializtion code (optional) *)
        strand : strand,                                (* strand definition *)
        create : create,                                (* initial strand creation *)
        start : stmt option,                            (* global start block (optional) *)
        update : stmt option                            (* global update block (optional) *)
      }

  (* global declarations *)
    and global_dcl
      = D_Var of var_dcl                                (* global-variable declaration *)
      | D_Func of var * var list * stmt                 (* user-defined function *)
      | D_DiffFunc of var * var list * expr             (* differentiable function *)

    and strand = Strand of {
        name : Atom.atom,                               (* the strand name *)
        params : var list,                              (* strand parameters *)
        spatialDim : int option,                        (* spatial dimension for queries *)
        state : var_dcl list,                           (* state-variable declarations *)
        stateInit : stmt option,                        (* additional state-variable initialization
                                                         * code (optional) *)
        startM : stmt option,                           (* optional start method *)
        updateM : stmt,                                 (* required update method *)
        stabilizeM : stmt option                        (* optional stabilize method *)
      }

    and stmt
      = S_Block of stmt list
      | S_Decl of var_dcl
      | S_IfThenElse of expr * stmt * stmt
      | S_Foreach of iter * stmt
      | S_Assign of var_use * expr
      | S_New of Atom.atom * expr list
      | S_KillAll
      | S_StabilizeAll
      | S_Continue
      | S_Die
      | S_Stabilize
      | S_Return of expr
      | S_Print of expr list

    and expr
      = E_Var of var_use
      | E_Lit of Literal.t
      | E_Kernel of Kernel.t
      | E_Select of expr * var_use                      (* strand state-variable selection *)
      | E_Prim                                          (* application of primitive operator or *)
          of var * Ty.meta_var list * expr list * Ty.ty (* function; we record the meta-variable *)
                                                        (* arguments so that we know how the the *)
                                                        (* parameters were instantiated. *)
      | E_Apply of var_use * expr list * Ty.ty          (* application of field or user function *)
      | E_Comprehension of expr * iter * Ty.ty          (* sequence comprehension *)
      | E_ParallelMap of expr * var * var * Ty.ty       (* parallel map over a global set of strands *)
      | E_Tensor of expr list * Ty.ty                   (* tensor-value construction *)
      | E_Field of expr list * Ty.ty                    (* field-value construction *)
      | E_Seq of expr list * Ty.ty                      (* sequence-value construction (ty is result type) *)
      | E_Slice of expr * expr option list * Ty.ty      (* tensor slicing (ty is result type). Note
                                                         * that we restrict the indices to be
                                                         * constant expressions. *)
      | E_Cond of expr * expr * expr * Ty.ty            (* ty is result type *)
      | E_Orelse of expr * expr                         (* non-strict '||' operator *)
      | E_Andalso of expr * expr                        (* non-struct '&&' operator *)
      | E_LoadNrrd of Ty.meta_var list * string * Ty.ty (* dynamic sequence and image loading *)
      | E_Coerce of {                                   (* coercion between types *)
            srcTy : Ty.ty,
            dstTy : Ty.ty,
            e : expr
          }

    withtype var_dcl = var * expr option
         and iter = var * expr
         and create = stmt Create.t

  end
