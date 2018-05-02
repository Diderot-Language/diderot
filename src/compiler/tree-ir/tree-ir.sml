(* tree-ir.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * This representation restores the block structure and nested expression syntax
 * of the source language.
 *)

structure TreeIR =
  struct

    structure Op = TreeOps
    structure Ty = TreeTypes

    type target_info = {
        layout : int -> TreeTypes.vec_layout,
        isInline : LowOps.rator -> bool
      }

    datatype program = Program of {
        props : Properties.t list,
        target : target_info,           (* target-specific info about representation invariants *)
        consts : global_var list,       (* large constant variables *)
        inputs : input list,            (* global input variables *)
        constInit : block,              (* code that initializes constants and inputs *)
        globals : global_var list,      (* other global variables *)
        funcs : func_def list,          (* user-defined functions *)
        globInit : block,               (* CFG to initialize other globals (if any) *)
        strand : strand,                (* the strand definition *)
        create : create,                (* initial strand creation *)
        start : block option,           (* optional global start code *)
        update : block option           (* optional global update code. *)
      }

    and func_def = Func of {
        name : func,
        params : var list,
        body : block
      }

    and strand = Strand of {
        name : Atom.atom,
        params : var list,
        spatialDim : int option,
        state : state_var list,
        stateInit : method,
        startM : method option,
        updateM : method,
        stabilizeM : method option
      }

    and method = Method of {
          needsW : bool,                (* does the method need the world (e.g., to print) *)
          hasG : bool,                  (* does the method contain references to globals? *)
          body : block                  (* the method body *)
        }

    and block = Block of {
        locals : var list ref,          (* the local variables that are used in this block *)
        body : stm list
      }

    and stm
      = S_Comment of string list
      | S_Assign of bool * var * exp    (* assignment; the boolean is true for var decls *)
      | S_MAssign of var list * exp
      | S_GAssign of global_var * exp
      | S_IfThen of exp * block
      | S_IfThenElse of exp * block * block
      | S_For of var * exp * exp * block
      | S_Foreach of var * exp * block
    (* special Diderot forms *)
      | S_MapReduce
          of map_reduce list * var
      | S_LoadNrrd                      (* load image/seq from nrrd file *)
          of var * APITypes.t * string * proxy
      | S_Input                         (* get input *)
          of global_var * string * string option * exp option
      | S_InputNrrd                     (* get image/seq input *)
          of global_var * string * string option * (string * proxy) option
      | S_New of Atom.atom * exp list   (* new strand creation *)
      | S_Save of state_var * exp  (* save strand state *)
      | S_KillAll
      | S_StabilizeAll
      | S_Print of Ty.t list * exp list
      | S_Return of exp option
    (* return functions for methods *)
      | S_Active
      | S_Stabilize
      | S_Die

    and map_reduce
      = MapReduce of (var * Reductions.t * func * exp list * StrandSets.t)

    and exp
      = E_Global of global_var
      | E_State of exp option * state_var
      | E_Var of var
      | E_Lit of Literal.t
      | E_Op of Op.rator * exp list
      | E_Apply of func * exp list              (* user-defined function application *)
      | E_Vec of int * int * exp list           (* vector value; the ints are the width and the
                                                 * padded width
                                                 *)
      | E_Cons of exp list * Ty.t               (* in-memory tensor value *)
      | E_Seq of exp list * Ty.t
      | E_Pack of Ty.vec_layout * exp list      (* pack vector pieces into composite vector value *)
      | E_VLoad of Ty.vec_layout * exp * int    (* load a piece of a composite vector value from
                                                 * a tensor.  The integer specifies the index of
                                                 * the piece.
                                                 *)

    and global_var = GV of {
        name : string,                  (* name (should be unique) *)
        ty : Ty.t,                      (* type *)
        xty : APITypes.t option,        (* external type (for inputs and outputs) *)
        input : bool,                   (* is an input variable *)
        output : bool,                  (* is the output value of the strand *)
        varying : bool                  (* varies over the execution of the program *)
      }

    and state_var = SV of {
        name : string,                  (* name (should be unique) *)
        ty : Ty.t,                      (* type *)
        xty : APITypes.t,               (* external type (for outputs, snapshots, and
                                         * debugging)
                                         *)
        output : bool,                  (* true for output variables *)
        varying : bool,                 (* varies over the lifetime of the strand *)
        shared : bool                   (* variable is accessed by other strands *)
      }

    and var = V of {
        name : string,                  (* name (should be unique) *)
        id : Stamp.t,                   (* unique ID *)
        ty : Ty.t                       (* type *)
      }

    and func = FV of {
        name : string,                  (* name (should be unique) *)
        id : Stamp.t,                   (* unique ID *)
        ty : Ty.t,                      (* return type *)
        paramTys : Ty.t list,           (* parameter types *)
        isMapFn : bool,                 (* is the function used in a parallel map? *)
        needsW : bool,                  (* does the function need the world (e.g., to print) *)
        hasG : bool                     (* does the function contain references to globals? *)
      }

    withtype input = global_var Inputs.input
         and create = block Create.t
         and proxy = ImageInfo.t option

    fun emptyBlk (Block{body=[], ...}) = true
      | emptyBlk _ = false

  end
