(* ssa-sig.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * The SSA signature is the signature of the HighIR, MidIR, and LowIR modules, which implement
 * the core intermediate representations of the Diderot compiler.  These IRs have the same program
 * and control-flow structure, but differ in their types and operators.
 *
 * Some properties:
 *
 *      1) The exit of the globalInit CFG has kind RETURN and the scope of the live variables
 *         is the whole program.
 *
 *      2) The scope of the parameters of a strand is the stateInit CFG and the methods of the
 *         strand.
 *
 *      3) Each strand has a list of strand-state variables (the "state" field).  These variables
 *         are not defined or used in the code, but serve as the "master template" for the
 *         strand's state.
 *
 *      4) The exit of the stateInit CFG in a strand has kind SINIT and the number of live
 *         variables should be the same as the number of variables in the strand's state-variable
 *         list.
 *
 *      5) Each method has a list of variables, called stateIn, which corresponds to the
 *         variables in the strand's state-variable list.
 *
 *      6) The exit node(s) of the update method should have kind ACTIVE, STABILIZE, or DIE.
 *         The live variables at a ACTIVE or STABILIZE exit correspond to the variables in
 *         the strand's state-variable list.  (Note that this property is modified after the
 *         variable-analysis phase).
 *)

signature SSA =
  sig

    val irName : string

    structure Ty : SSA_TYPES
    structure Op : OPERATORS where type ty = Ty.ty

  (***** CFG *****)

    datatype global_kind = datatype GlobalVarKind.t

    datatype cfg = CFG of {
        entry : node,   (* the entry node of a graph; not necessarily an ENTRY node *)
        exit : node     (* the exit node of a graph; not necessarily an EXIT node. *)
      }

    and node = ND of {
        id : Stamp.t,
        props : PropList.holder,
        kind : node_kind
      }

    and node_kind
      = NULL
      | ENTRY of {
            succ : node ref
          }
      | JOIN of {
            preds : node list ref,      (* includes fake control-flow edges *)
            mask : bool list ref,       (* true for incoming fake edges *)
            phis : phi list ref,        (* data-flow joins corresponding to the edges in preds *)
            succ : node ref
          }
      | COND of {
            pred : node ref,
            cond : var ref,             (* the variable being tested (we use a ref to allow rewriting) *)
            trueBranch : node ref,
            falseBranch : node ref
          }
      | FOREACH of {                    (* foreach-loop; this node combines aspects of the COND
                                         * and JOIN nodes. *)
            pred : node ref,            (* the predecessor *)
            phis : phi list ref,        (* phi nodes (as in JOIN) *)
            mask : bool list ref,       (* true for incoming fake edges *)
            var : var,                  (* the loop variable *)
            src : var ref,              (* the source of values being iterated over *)
            bodyEntry : node ref,       (* the loop body entry node *)
            bodyExit : node ref,        (* the loop body exit node (will be a "NEXT") *)
            succ : node ref             (* the loop-exit edge *)
          }
      | NEXT of {                       (* a loop-body exit node *)
            pred : node ref,            (* the predecessor *)
            succ : node ref             (* the FOREACH node *)
          }
      | COM of  {                       (* comment *)
            pred : node ref,
            text : string list,
            succ : node ref
          }
      | ASSIGN of {                     (* local-variable assignment *)
            pred : node ref,
            stm : assign,
            succ : node ref
          }
      | MASSIGN of {                    (* multi-assignment *)
            pred : node ref,
            stm : massign,
            succ : node ref
          }
      | GASSIGN of {                    (* global variable assignment *)
            pred: node ref,
            lhs : global_var,
            rhs : var,
            succ : node ref
          }
      | NEW of {                        (* create new strand instance *)
            pred : node ref,
            strand : Atom.atom,
            args : var list,
            succ : node ref
          }
      | SAVE of {                       (* save state variable *)
            pred: node ref,
            lhs : state_var,
            rhs : var,
            succ : node ref
          }
      | EXIT of {                       (* includes die and stabilize *)
            pred : node ref,
            kind : var ExitKind.kind,   (* kind of exit node *)
            succ : node option ref      (* optional fake control-flow edge for when the EXIT is *)
                                        (* not the CFG exit node *)
          }

    and rhs
      = GLOBAL of global_var            (* read global variable *)
      | STATE of var option * state_var (* read strand state variable; NONE means "self" *)
      | VAR of var
      | LIT of Literal.t
      | OP of Op.rator * var list
      | CONS of var list * Ty.ty        (* tensor-value construction *)
      | SEQ of var list * Ty.ty         (* sequence-value construction *)
      | EINAPP of Ein.ein * var list
      | APPLY of func * var list        (* function application *)
      | MAPREDUCE of (Reductions.t * func * var list) list
                                        (* parallel map-reduce *)

    and var = V of {
        name : string,                  (* name *)
        id : Stamp.t,                   (* unique ID *)
        ty : Ty.ty,                     (* type *)
        bind : var_bind ref,            (* binding *)
        useCnt : int ref,               (* count of uses *)
        props : PropList.holder
      }

    and var_bind
      = VB_NONE
      | VB_RHS of rhs                   (* defined by an assignment (includes globals and state variables) *)
      | VB_MULTIOP of int * rhs         (* n'th result of operator in multi-assignment *)
      | VB_PHI of var option list       (* defined by a phi node *)
      | VB_PARAM                        (* parameter to a strand *)
      | VB_ITER                         (* bound in a foreach loop *)

  (***** global variables *****)
    and global_var = GV of {
        id : Stamp.t,                   (* variable's unique ID *)
        name : string,                  (* variable's name *)
        ty : Ty.ty,                     (* variable's type *)
        kind : global_kind,             (* the variable kind *)
        updated : bool,                 (* true if the global variable is modified in the *)
                                        (* global-update code *)
        binding : var option ref,       (* the local variable used to define this global variable
                                         * (i.e., in a GASSIGN node).  This field will be NONE
                                         * for inputs and for mutable variables.
                                         *)
        useCnt : int ref,               (* count of uses *)
        props : PropList.holder
      }

  (***** strand state variables *****)
    and state_var = SV of {
        id : Stamp.t,                   (* variable's unique ID *)
        name : string,                  (* variable's name *)
        ty : Ty.ty,                     (* variable's type *)
        output : bool,                  (* true for output variables *)
        varying : bool,                 (* true for variables that are modified during super steps *)
        shared : bool,                  (* true for variables that are read by other strands *)
        props : PropList.holder
      }

  (***** function identifiers *****)
    and func = FV of {
        id : Stamp.t,                   (* function's unique ID *)
        name : string,                  (* function's name *)
        ty : Ty.ty,                     (* function's result type *)
        paramTys : Ty.ty list,          (* function's parameter types *)
        useCnt : int ref,               (* count of uses *)
        props : PropList.holder
      }

    withtype assign = (var * rhs)
         and massign = (var list * rhs)
         and phi = (var * var option list)      (* NONE for fake edges *)

    datatype assignment
      = ASSGN of assign
      | MASSGN of massign
      | GASSGN of (global_var * var)
      | SAV of (state_var * var)

  (***** Program representation *****)

    type input = global_var Inputs.input

    datatype program = Program of {
        props : Properties.t list,
        consts : global_var list,       (* large constant variables *)
        inputs : input list,            (* global input variables *)
        constInit : cfg,                (* code that initializes constants and inputs *)
        globals : global_var list,      (* other global variables *)
        funcs : func_def list,          (* user-defined functions *)
        globInit : cfg,                 (* CFG to initialize globals *)
        strand : strand,                (* the strand definition *)
        create : create,                (* initial strand creation *)
        start : cfg option,             (* optional global start code *)
        update : cfg option             (* optional update code. *)
      }

    and func_def = Func of {
        name : func,
        params : var list,
        body : cfg
      }

    and strand = Strand of {
        name : Atom.atom,
        params : var list,
        spatialDim : int option,
        state : state_var list,
        stateInit : cfg,
        startM : cfg option,
        updateM : cfg,
        stabilizeM : cfg option
      }

    withtype create = cfg Create.t

  (* operations on global variables *)
    structure GlobalVar : sig
      (* `new (kind, isVarying, name, ty)` creates a new global variable *)
        val new : global_kind * bool * string * Ty.ty -> global_var
        val name : global_var -> string
        val uniqueName : global_var -> string
        val ty : global_var -> Ty.ty
        val kind : global_var -> global_kind
        val isVarying : global_var -> bool
        val bindingOf : global_var -> var option
        val setBinding : global_var * var -> unit
        val isInput : global_var -> bool
        val useCount : global_var -> int
        val same : global_var * global_var -> bool
        val compare : global_var * global_var -> order
        val hash : global_var -> word
        val toString : global_var -> string
      (* properties *)
        val newProp : (global_var -> 'a) -> {
                getFn : global_var -> 'a,
                peekFn : global_var -> 'a option,
                setFn : global_var * 'a -> unit,
                clrFn : global_var -> unit
              }
        val newFlag : unit -> {
                getFn : global_var -> bool,
                setFn : global_var * bool -> unit
              }
      (* collections *)
        structure Map : ORD_MAP where type Key.ord_key = global_var
        structure Set : ORD_SET where type Key.ord_key = global_var
        structure Tbl : MONO_HASH_TABLE where type Key.hash_key = global_var
      end

  (* operations on strand-state variables *)
    structure StateVar : sig
      (* `new (isOutput, name, ty, isVarying, isShared)` creates a new state variable *)
        val new : bool * string * Ty.ty * bool * bool -> state_var
        val name : state_var -> string
        val ty : state_var -> Ty.ty
        val isOutput : state_var -> bool
        val isVarying : state_var -> bool
        val isShared : state_var -> bool
        val same : state_var * state_var -> bool
        val compare : state_var * state_var -> order
        val hash : state_var -> word
        val toString : state_var -> string
      (* properties *)
        val newProp : (state_var -> 'a) -> {
                getFn : state_var -> 'a,
                peekFn : state_var -> 'a option,
                setFn : state_var * 'a -> unit,
                clrFn : state_var -> unit
              }
        val newFlag : unit -> {
                getFn : state_var -> bool,
                setFn : state_var * bool -> unit
              }
      (* collections *)
        structure Map : ORD_MAP where type Key.ord_key = state_var
        structure Set : ORD_SET where type Key.ord_key = state_var
        structure Tbl : MONO_HASH_TABLE where type Key.hash_key = state_var
      end

  (* operations on variables *)
    structure Var : sig
        val new : string * Ty.ty -> var
        val copy : var -> var
        val name : var -> string
        val ty : var -> Ty.ty
        val binding : var -> var_bind
        val setBinding : var * var_bind -> unit
        val useCount : var -> int
        val same : var * var -> bool
        val compare : var * var -> order
        val hash : var -> word
        val toString : var -> string
      (* get the local rhs definition of a variable.  Unlike getDef, this function
       * does not chase through global variable bindings.
       *)
        val getLocalDef : var -> rhs
      (* get the rhs definition of a variable; This function will return VAR x if the
       * definition chain terminates in a variable x that is not bound to a rhs (e.g.,
       * a parameter or iteration variable).  It will return GLOBAL gv if the definition
       * chain leads to a mutable global gv.
       *)
        val getDef : var -> rhs
      (* properties *)
        val newProp : (var -> 'a) -> {
                getFn : var -> 'a,
                peekFn : var -> 'a option,
                setFn : var * 'a -> unit,
                clrFn : var -> unit
              }
        val newFlag : unit -> {
                getFn : var -> bool,
                setFn : var * bool -> unit
              }
      (* collections *)
        structure Map : ORD_MAP where type Key.ord_key = var
        structure Set : ORD_SET where type Key.ord_key = var
        structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var
      end

    structure Func : sig
        val new : string * Ty.ty * Ty.ty list -> func
        val name : func -> string
        val ty : func -> Ty.ty * Ty.ty list
        val same : func * func -> bool
        val compare : func * func -> order
        val hash : func -> word
        val toString : func -> string
      (* increment the use count *)
        val use : func -> func
      (* decrement the use count *)
        val decCnt : func -> unit
      (* return the use count *)
        val useCount : func -> int
      (* properties *)
        val newProp : (func -> 'a) -> {
                getFn : func -> 'a,
                peekFn : func -> 'a option,
                setFn : func * 'a -> unit,
                clrFn : func -> unit
              }
        val newFlag : unit -> {
                getFn : func -> bool,
                setFn : func * bool -> unit
              }
      (* collections *)
        structure Map : ORD_MAP where type Key.ord_key = func
        structure Set : ORD_SET where type Key.ord_key = func
        structure Tbl : MONO_HASH_TABLE where type Key.hash_key = func
      end

  (* operations on CFGs *)
    structure CFG : sig
      (* the empty CFG *)
        val empty : cfg
      (* is a CFG empty? *)
        val isEmpty : cfg -> bool
      (* create a basic block from a list of assignments *)
        val mkBlock : assignment list -> cfg
      (* entry/exit nodes of a CFG *)
        val entry : cfg -> node
        val exit : cfg -> node
      (* DFS sorting of the graph rooted at the entry to a statement; the resulting list will
       * be in preorder with parents before children.
       *)
        val sort : cfg -> node list
      (* apply a function to all of the nodes in the graph rooted at the entry to the CFG *)
        val apply : (node -> unit) -> cfg -> unit
      (* delete a simple node from a CFG *)
        val deleteNode : node -> unit
      (* replace a simple node in a cfg with a different simple node *)
        val replaceNode : (node * node) -> unit
      (* replace a simple node in a cfg with a subgraph *)
        val replaceNodeWithCFG : (node * cfg) -> unit
      (* concatenate two CFGs *)
        val concat : cfg * cfg -> cfg
      (* prepend a node to a CFG *)
        val prependNode : node * cfg -> cfg
      (* append a node to a CFG *)
        val appendNode : cfg * node -> cfg
      (* insert a block of assignments at the beginning of the CFG.  If the CFG has an ENTRY
       * node, then the block is inserted immediatly following the entry.
       *)
        val prependBlock : assignment list * cfg -> cfg
      (* insert a block of assignments at the end of the CFG argument  If the CFG has an EXIT
       * node, then the block is inserted immediatly following the exit.
       *)
        val appendBlock : cfg * assignment list -> cfg
      end

  (* operations on CFG nodes *)
    structure Node : sig
        val id : node -> Stamp.t
        val kind : node -> node_kind
        val same : node * node -> bool
        val compare : node * node -> order
        val hash : node -> word
        val toString : node -> string
        val isNULL : node -> bool
        val isReachable : node -> bool
      (* variable defs and uses; may include duplicates *)
        val uses : node -> var list
        val defs : node -> var list
      (* dummy node with NULL kind *)
        val dummy : node
      (* CFG edges *)
        val hasPred : node -> bool
        val preds : node -> node list
        val setPred : node * node -> unit
        val hasSucc : node -> bool
        val succs : node -> node list
        val setSucc : node * node -> unit
        val setTrueBranch : node * node -> unit     (* set trueBranch successor for COND node *)
        val setFalseBranch : node * node -> unit    (* set falseBranch successor for COND node *)
        val setBodyEntry : node * node -> unit      (* set body entry edge for FOREACH node *)
        val setBodyExit : node * node -> unit       (* set body exit edge for FOREACH node *)
        val setPhis : node * phi list -> unit       (* set phis for JOIN/FOREACH node *)
        val setEdgeMask : node * bool list -> unit  (* set incoming edge mask for JOIN/FOREACH node *)
        val addEdge : node * node -> unit
      (* replace the edge src-->oldDst by the edge src-->dst and set the predecessor edge
       * from dst to src.
       *)
        val replaceOutEdge : {src : node, oldDst : node, dst : node} -> unit
      (* replace the edge oldSrc-->dst by the edge src-->dst and set the predecessor edge
       * from dst to src.
       *)
        val replaceInEdge : {oldSrc : node, src : node, dst : node} -> unit
      (* constructors *)
        val mkENTRY : unit -> node
        val mkJOIN : phi list -> node
        val mkCOND : var -> node
        val mkFOREACH : var * var -> node
        val mkNEXT : unit -> node
        val mkCOM : string list -> node
        val mkASSIGN : assign -> node
        val mkMASSIGN : massign -> node
        val mkGASSIGN : (global_var * var) -> node
        val mkNEW : Atom.atom * var list -> node
        val mkSAVE : state_var * var -> node
        val mkEXIT : var ExitKind.kind -> node
        val mkRETURN : var option -> node
        val mkACTIVE : unit -> node
        val mkSTABILIZE : unit -> node
        val mkDIE : unit -> node
        val mkNEXTSTEP : unit -> node
        val mkUNREACHABLE : unit -> node
      (* properties *)
        val newProp : (node -> 'a) -> {
                getFn : node -> 'a,
                peekFn : node -> 'a option,
                setFn : node * 'a -> unit,
                clrFn : node -> unit
              }
        val newFlag : unit -> {
                getFn : node -> bool,
                setFn : node * bool -> unit
              }
      end

  (* operations on RHS expressions *)
    structure RHS : sig
        val toString : rhs -> string
        val vars : rhs -> var list
        val map : (var -> var) -> rhs -> rhs
        val app : (var -> unit) -> rhs -> unit
      end

  (* return a string representation of a variable binding *)
    val vbToString : var_bind -> string

  (* return a string representation of a PHI node *)
    val phiToString : phi -> string

  (* return a string representation of an assignment *)
    val assignToString : assign -> string
    val massignToString : massign -> string
    val assignmentToString : assignment -> string

  end
