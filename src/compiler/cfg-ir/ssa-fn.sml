(* ssa-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * The SSAFn functor is used to generate the High, Med, and Low IRs in the Diderot
 * compiler.  These IRs have the same program and control-flow structure, but differ
 * in their types and operators.
 *)

functor SSAFn (

    val irName : string

    structure Ty : SSA_TYPES
    structure Op : OPERATORS where type ty = Ty.ty

  ) : SSA = struct

    structure Ty = Ty
    structure Op = Op

    val irName = irName

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
            bodyExit : node ref,        (* the loop body exit node *)
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

    structure GlobalVar =
      struct
        fun new (kind, isVarying, name, ty) = GV{
                id = Stamp.new(),
                name = name,
                ty = ty,
                kind = kind,
                updated = isVarying,
                binding = ref NONE,
                useCnt = ref 0,
                props = PropList.newHolder()
              }
        fun name (GV{name, ...}) = name
        fun uniqueName (GV{id, name, ...}) = name ^ Stamp.toString id
        fun kind (GV{kind, ...}) = kind
        fun isVarying (GV{updated, ...}) = updated
        fun bindingOf (GV{binding, ...}) = !binding
        fun setBinding (GV{binding, ...}, x) = (binding := SOME x)
        fun ty (GV{ty, ...}) = ty
        fun isInput (GV{kind=InputVar, ...}) = true
          | isInput _ = false
        fun useCount (GV{useCnt, ...}) = !useCnt
        fun same (GV{id=a, ...}, GV{id=b, ...}) = Stamp.same(a, b)
        fun compare (GV{id=a, ...}, GV{id=b, ...}) = Stamp.compare(a, b)
        fun hash (GV{id, ...}) = Stamp.hash id
        fun toString (GV{id, name, ...}) = concat["globals.", name, Stamp.toString id]
      (* properties *)
        fun newProp initFn = PropList.newProp (fn (GV{props, ...}) => props, initFn)
        fun newFlag () = PropList.newFlag (fn (GV{props, ...}) => props)
      (* collections *)
        local
          structure V =
            struct
              type ord_key = global_var
              val compare = compare
            end
        in
        structure Map = RedBlackMapFn (V)
        structure Set = RedBlackSetFn (V)
        end
        structure Tbl = HashTableFn (
          struct
            type hash_key = global_var
            val hashVal = hash
            val sameKey = same
          end)
      end

    structure StateVar =
      struct
        fun new (isOut, name, ty, varying, shared) = SV{
                id = Stamp.new(),
                name = name,
                ty = ty,
                output = isOut,
                varying = varying,
                shared = shared,
                props = PropList.newHolder()
              }
        fun name (SV{name, ...}) = name
        fun ty (SV{ty, ...}) = ty
        fun isOutput (SV{output, ...}) = output
        fun isVarying (SV{varying, ...}) = varying
        fun isShared (SV{shared, ...}) = shared
        fun same (SV{id=a, ...}, SV{id=b, ...}) = Stamp.same(a, b)
        fun compare (SV{id=a, ...}, SV{id=b, ...}) = Stamp.compare(a, b)
        fun hash (SV{id, ...}) = Stamp.hash id
        fun toString (SV{name, ...}) = name
      (* properties *)
        fun newProp initFn = PropList.newProp (fn (SV{props, ...}) => props, initFn)
        fun newFlag () = PropList.newFlag (fn (SV{props, ...}) => props)
      (* collections *)
        local
          structure V =
            struct
              type ord_key = state_var
              val compare = compare
            end
        in
        structure Map = RedBlackMapFn (V)
        structure Set = RedBlackSetFn (V)
        end
        structure Tbl = HashTableFn (
          struct
            type hash_key = state_var
            val hashVal = hash
            val sameKey = same
          end)
      end

    structure Var =
      struct
        fun new (name, ty) = V{
                name = name,
                id = Stamp.new(),
                ty = ty,
                bind = ref VB_NONE,
                useCnt = ref 0,
                props = PropList.newHolder()
              }
        fun copy (V{name, ty, ...}) = new (name, ty)
        fun name (V{name, ...}) = name
        fun ty (V{ty, ...}) = ty
        fun binding (V{bind, ...}) = !bind
        fun setBinding (V{bind, ...}, vb) = bind := vb
        fun useCount (V{useCnt, ...}) = !useCnt
        fun same (V{id=a, ...}, V{id=b, ...}) = Stamp.same(a, b)
        fun compare (V{id=a, ...}, V{id=b, ...}) = Stamp.compare(a, b)
        fun hash (V{id, ...}) = Stamp.hash id
        fun toString (V{name, id, ...}) = name ^ Stamp.toString id
        fun getLocalDef x = (case binding x
               of VB_RHS(VAR x) => getLocalDef x
                | VB_RHS rhs => rhs
                | _ => VAR x
              (* end case *))
        fun getDef x = (case binding x
               of VB_RHS rhs => (case rhs
                     of GLOBAL gv => if GlobalVar.isInput gv
                          then rhs (* inputs could be different at runtime! *)
                          else (case GlobalVar.bindingOf gv
                             of SOME x => getDef x
                              | NONE => rhs
                            (* end case *))
                      | VAR x => getDef x
                      | _ => rhs
                    (* end case *))
                | _ => VAR x
              (* end case *))
      (* properties *)
        fun newProp initFn = PropList.newProp (fn (V{props, ...}) => props, initFn)
        fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)
      (* collections *)
        local
          structure V =
            struct
              type ord_key = var
              val compare = compare
            end
        in
        structure Map = RedBlackMapFn (V)
        structure Set = RedBlackSetFn (V)
        end
        structure Tbl = HashTableFn (
          struct
            type hash_key = var
            val hashVal = hash
            val sameKey = same
          end)
      end

    structure Func = struct
        fun new (name, resTy, paramTys) = FV{
                name = name,
                id = Stamp.new(),
                ty = resTy,
                paramTys = paramTys,
                useCnt = ref 0,
                props = PropList.newHolder()
              }
        fun name (FV{name, ...}) = name
        fun ty (FV{ty, paramTys, ...}) = (ty, paramTys)
        fun same (FV{id=a, ...}, FV{id=b, ...}) = Stamp.same(a, b)
        fun compare (FV{id=a, ...}, FV{id=b, ...}) = Stamp.compare(a, b)
        fun hash (FV{id, ...}) = Stamp.hash id
        fun toString (FV{name, id, ...}) = name ^ Stamp.toString id
        fun use (f as FV{useCnt, ...}) = (useCnt := !useCnt + 1; f)
        fun decCnt (FV{useCnt, ...}) = (useCnt := !useCnt - 1)
        fun useCount (FV{useCnt, ...}) = !useCnt
      (* properties *)
        fun newProp initFn = PropList.newProp (fn (FV{props, ...}) => props, initFn)
        fun newFlag () = PropList.newFlag (fn (FV{props, ...}) => props)
      (* collections *)
        local
          structure FV =
            struct
              type ord_key = func
              val compare = compare
            end
        in
        structure Map = RedBlackMapFn (FV)
        structure Set = RedBlackSetFn (FV)
        end
        structure Tbl = HashTableFn (
          struct
            type hash_key = func
            val hashVal = hash
            val sameKey = same
          end)
      end

    structure RHS =
      struct
        fun vars rhs = (case rhs
               of GLOBAL _ => []
                | STATE(SOME x, _) => [x]
                | STATE _ => []
                | VAR x => [x]
                | LIT _ => []
                | OP(rator, xs) => xs
                | CONS(xs, _) => xs
                | SEQ(xs, _) => xs
                | EINAPP(_, xs) => xs
                | APPLY(_, xs) => xs
                | MAPREDUCE mrs => List.foldr (fn ((_, _, args), xs) => args@xs) [] mrs
              (* end case *))

        fun map f = let
              fun mapf rhs = (case rhs
                     of GLOBAL _ => rhs
                      | STATE(SOME x, sv) => STATE(SOME(f x), sv)
                      | STATE _ => rhs
                      | VAR x => VAR(f x)
                      | LIT _ => rhs
                      | OP(rator, xs) => OP(rator, List.map f xs)
                      | CONS(xs, ty) => CONS(List.map f xs, ty)
                      | SEQ(xs, ty) => SEQ(List.map f xs, ty)
                      | EINAPP(ein, xs) => EINAPP(ein, List.map f xs)
                      | APPLY(g, xs) => APPLY(g, List.map f xs)
                      | MAPREDUCE mrs =>
                          MAPREDUCE(List.map (fn (r, g, xs) => (r, g, List.map f xs)) mrs)
                    (* end case *))
              in
                mapf
              end

        fun app f = let
              fun appf rhs = (case rhs
                     of GLOBAL _ => ()
                      | STATE(SOME x, _) => f x
                      | STATE _ => ()
                      | VAR x => f x
                      | LIT _ => ()
                      | OP(rator, xs) => List.app f xs
                      | CONS(xs, _) => List.app f xs
                      | SEQ(xs, _) => List.app f xs
                      | EINAPP(_, xs) => List.app f xs
                      | APPLY(_, xs) => List.app f xs
                      | MAPREDUCE mrs => List.app (fn (_, _, xs) => List.app f xs) mrs
                    (* end case *))
              in
                appf
              end

      (* return a string representation of a rhs *)
        fun toString rhs = (case rhs
               of GLOBAL x => GlobalVar.toString x
                | STATE(NONE, x) => "self." ^ StateVar.toString x
                | STATE(SOME x, y) => String.concat[Var.toString x, ".", StateVar.toString y]
                | VAR x => Var.toString x
                | LIT lit => Literal.toString lit
                | OP(rator, xs) => String.concat [
                      Op.toString rator,
                      "(", String.concatWithMap "," Var.toString xs, ")"
                    ]
                | CONS(xs, ty) => String.concat [
                      "<", Ty.toString ty, ">[",
                      String.concatWithMap "," Var.toString xs, "]"
                    ]
                | SEQ(xs, ty) => String.concat [
                      "<", Ty.toString ty, ">{",
                      String.concatWithMap "," Var.toString xs, "}"
                    ]
                | EINAPP(ein, xs) => String.concat [
                      EinPP.toString ein, " (",
                      String.concatWithMap "," Var.toString xs, ")"
                    ]
                | APPLY(f, xs) => String.concat [
                      Func.toString f, " (",
                      String.concatWithMap "," Var.toString xs, ")"
                    ]
                | MAPREDUCE[(r, f, args)] => String.concat [
                      Reductions.toString r, "(MAP ", Func.toString f, " (",
                      String.concatWithMap "," Var.toString args, "))"
                    ]
                | MAPREDUCE mrs => let
                    fun toS (r, f, args) = String.concat [
                            Reductions.toString r, "(MAP ", Func.toString f, " (",
                            String.concatWithMap "," Var.toString args, "))"
                          ]
                    in
                      String.concat ["{ ", String.concatWithMap " | " toS mrs, " }"]
                    end
              (* end case *))
      end

    structure Node =
      struct
        fun id (ND{id, ...}) = id
        fun kind (ND{kind, ...}) = kind
        fun same (ND{id=a, ...}, ND{id=b, ...}) = Stamp.same(a, b)
        fun compare (ND{id=a, ...}, ND{id=b, ...}) = Stamp.compare(a, b)
        fun hash (ND{id, ...}) = Stamp.hash id
        fun toString (ND{id, kind, ...}) = let
              val tag = (case kind
                     of NULL => "NULL"
                      | ENTRY _ => "ENTRY"
                      | JOIN _ => "JOIN"
                      | COND _ => "COND"
                      | FOREACH _ => "FOREACH"
                      | NEXT _ => "NEXT"
                      | COM _ => "COM"
                      | ASSIGN _ => "ASSIGN"
                      | MASSIGN _ => "MASSIGN"
                      | GASSIGN _ => "GASSIGN"
                      | NEW _ => "NEW"
                      | SAVE _ => "SAVE"
                      | EXIT{kind, ...} => ExitKind.toString Var.toString kind
                    (* end case *))
              in
                tag ^ Stamp.toString id
              end
        fun new kind = ND{id = Stamp.new(), props = PropList.newHolder(), kind = kind}
      (* variable defs and uses *)
        fun uses (ND{kind, ...}) = (case kind
               of JOIN{phis, ...} => let
                    fun add ([], ys) = ys
                      | add (SOME x :: xs, ys) = add(xs, x::ys)
                      | add (NONE :: xs, ys) = add(xs, ys)
                    in
                      List.foldr (fn ((_, xs), ys) => add(xs, ys)) [] (!phis)
                    end
                | COND{cond, ...} => [!cond]
                | FOREACH{src, phis, ...} => let
                    fun add ([], ys) = ys
                      | add (SOME x :: xs, ys) = add(xs, x::ys)
                      | add (NONE :: xs, ys) = add(xs, ys)
                    in
                      List.foldr (fn ((_, xs), ys) => add(xs, ys)) [!src] (!phis)
                    end
                | NEXT _ => []
                | ASSIGN{stm=(y, rhs), ...} => RHS.vars rhs
                | MASSIGN{stm=(_, rhs), ...} => RHS.vars rhs
                | GASSIGN{rhs, ...} => [rhs]
                | NEW{args, ...} => args
                | SAVE{rhs, ...} => [rhs]
                | _ => []
              (* end case *))
        fun defs (ND{kind, ...}) = (case kind
               of JOIN{phis, ...} => List.map #1 (!phis)
                | FOREACH{var, phis, ...} => var :: List.map #1 (!phis)
                | ASSIGN{stm=(y, _), ...} => [y]
                | MASSIGN{stm=(ys, _), ...} => ys
                | _ => []
              (* end case *))
        val dummy = new NULL
        fun mkENTRY () = new (ENTRY{succ = ref dummy})
        fun mkJOIN phis = new (JOIN{preds = ref [], mask = ref [], phis = ref phis, succ = ref dummy})
        fun mkCOND cond = new (COND{
                pred = ref dummy, cond = ref cond,
                trueBranch = ref dummy, falseBranch = ref dummy
              })
        fun mkFOREACH (var, src) = (
              Var.setBinding (var, VB_ITER);
              new (FOREACH{
                  pred = ref dummy,
                  phis = ref [], mask = ref [],
                  var = var, src = ref src,
                  bodyEntry = ref dummy,
                  bodyExit = ref dummy,
                  succ = ref dummy
                }))
        fun mkNEXT () = new(NEXT{pred = ref dummy, succ = ref dummy})
        fun mkCOM text = new (COM{pred = ref dummy, text = text, succ = ref dummy})
        fun mkASSIGN (lhs, rhs) = (
              Var.setBinding (lhs, VB_RHS rhs);
              new (ASSIGN{pred = ref dummy, stm = (lhs, rhs), succ = ref dummy}))
        fun mkMASSIGN (lhs, rhs) = let
              fun setB (_, []) = ()
                | setB (i, x::xs) = (
                    Var.setBinding (x, VB_MULTIOP(i, rhs));
                    setB (i+1, xs))
              in
                setB (0, lhs);
                new (MASSIGN{pred = ref dummy, stm = (lhs, rhs), succ = ref dummy})
              end
        fun mkGASSIGN (lhs, rhs) = (
              if not(GlobalVar.isVarying lhs) then GlobalVar.setBinding(lhs, rhs) else ();
              new (GASSIGN{pred = ref dummy, lhs = lhs, rhs = rhs, succ = ref dummy}))
        fun mkNEW (strand, args) = new (NEW{
                pred = ref dummy, strand = strand, args = args, succ = ref dummy
              })
        fun mkSAVE (lhs, rhs) = new (SAVE{
                pred = ref dummy, lhs = lhs, rhs = rhs, succ = ref dummy
              })
        fun mkEXIT kind = new (EXIT{kind = kind, pred = ref dummy, succ = ref NONE})
        fun mkRETURN optX = mkEXIT (ExitKind.RETURN optX)
        fun mkACTIVE () = mkEXIT ExitKind.ACTIVE
        fun mkSTABILIZE () = mkEXIT ExitKind.STABILIZE
        fun mkDIE () = mkEXIT ExitKind.DIE
        fun mkNEXTSTEP () = mkEXIT ExitKind.NEXTSTEP
        fun mkUNREACHABLE () = mkEXIT ExitKind.UNREACHABLE
        fun isNULL (ND{kind=NULL, ...}) = true
          | isNULL _ = false
      (* is a node reachable from the CFG entry; UNREACHABLE exit nodes and JOINs with no real
       * predecessors are unreachable.
       *)
        fun isReachable (ND{kind=EXIT{kind=ExitKind.UNREACHABLE, ...}, ...}) = false
          | isReachable (ND{kind=JOIN{mask, ...}, ...}) = List.exists not (!mask)
          | isReachable _ = true
      (* editing node edges *)
        fun hasPred (ND{kind, ...}) = (case kind
               of NULL => false
                | ENTRY _ => false
                | _ => true
              (* end case *))
        fun setPred (nd0 as ND{kind, ...}, nd) = (case kind
               of NULL => raise Fail("setPred on NULL node " ^ toString nd0)
                | ENTRY _ => raise Fail("setPred on ENTRY node " ^ toString nd0)
                | JOIN{preds, ...} => preds := !preds @ [nd]  (* assume preds are added in order *)
                | FOREACH{pred, ...} => pred := nd
                | NEXT{pred, ...} => pred := nd
                | COND{pred, ...} => pred := nd
                | COM{pred, ...} => pred := nd
                | ASSIGN{pred, ...} => pred := nd
                | MASSIGN{pred, ...} => pred := nd
                | GASSIGN{pred, ...} => pred := nd
                | NEW{pred, ...} => pred := nd
                | SAVE{pred, ...} => pred := nd
                | EXIT{pred, ...} => pred := nd
              (* end case *))
        fun preds (nd as ND{kind, ...}) = (case kind
               of NULL => [] (*raise Fail("preds on NULL node "^toString nd)*)
                | ENTRY _ => []
                | JOIN{preds, ...} => !preds
                | COND{pred, ...} => [!pred]
                | FOREACH{pred, bodyExit, ...} => [!pred, !bodyExit]
                | NEXT{pred, ...} => [!pred]
                | COM{pred, ...} => [!pred]
                | ASSIGN{pred, ...} => [!pred]
                | MASSIGN{pred, ...} => [!pred]
                | GASSIGN{pred, ...} => [!pred]
                | NEW{pred, ...} => [!pred]
                | SAVE{pred, ...} => [!pred]
                | EXIT{pred, ...} => [!pred]
              (* end case *))
        fun hasSucc (ND{kind, ...}) = (case kind
               of NULL => false
                | ENTRY _ => true
                | JOIN _ => true
                | COND _ => true
                | COM _ => true
                | FOREACH _ => true
                | NEXT _ => true
                | ASSIGN _ => true
                | MASSIGN _ => true
                | GASSIGN _ => true
                | NEW _ => true
                | SAVE _ => true
                | EXIT{succ=ref(SOME _), ...} => true
                | EXIT _ => false
              (* end case *))
        fun setSucc (nd0 as ND{kind, ...}, nd) = (case kind
               of NULL => raise Fail("setSucc on NULL node "^toString nd0)
                | ENTRY{succ} => succ := nd
                | JOIN{succ, ...} => succ := nd
                | COND _ => raise Fail("setSucc on COND node "^toString nd0)
                | FOREACH{succ, ...} => succ := nd
                | NEXT{succ, ...} => succ := nd
                | COM{succ, ...} => succ := nd
                | ASSIGN{succ, ...} => succ := nd
                | MASSIGN{succ, ...} => succ := nd
                | GASSIGN{succ, ...} => succ := nd
                | NEW{succ, ...} => succ := nd
                | SAVE{succ, ...} => succ := nd
                | EXIT{succ, ...} => succ := SOME nd
              (* end case *))
        fun succs (ND{kind, ...}) = (case kind
               of NULL => [] (*raise Fail("succs on NULL node "^toString nd)*)
                | ENTRY{succ} => [!succ]
                | JOIN{succ, ...} => [!succ]
                | COND{trueBranch, falseBranch, ...} => [!trueBranch, !falseBranch]
                | FOREACH{bodyEntry, succ, ...} => [!bodyEntry, !succ]
                | NEXT{succ, ...} => [!succ]
                | COM{succ, ...} => [!succ]
                | ASSIGN{succ, ...} => [!succ]
                | MASSIGN{succ, ...} => [!succ]
                | GASSIGN{succ, ...} => [!succ]
                | NEW{succ, ...} => [!succ]
                | SAVE{succ, ...} => [!succ]
                | EXIT{succ=ref(SOME nd), ...} => [nd]
                | EXIT _ => []
              (* end case *))
(* QUESTION: should these functions also do the setPred operation? *)
        fun setTrueBranch (ND{kind=COND{trueBranch, ...}, ...}, nd) = trueBranch := nd
          | setTrueBranch (nd, _) = raise Fail("setTrueBranch on " ^ toString nd)
        fun setFalseBranch (ND{kind=COND{falseBranch, ...}, ...}, nd) = falseBranch := nd
          | setFalseBranch (nd, _) = raise Fail("setFalseBranch on " ^ toString nd)
        fun setBodyEntry (ND{kind=FOREACH{bodyEntry, ...}, ...}, nd) = bodyEntry := nd
          | setBodyEntry (nd, _) = raise Fail("setBodyEntry on " ^ toString nd)
        fun setBodyExit (ND{kind=FOREACH{bodyExit, ...}, ...}, nd) = bodyExit := nd
          | setBodyExit (nd, _) = raise Fail("setBodyExit on " ^ toString nd)
        fun setPhis (ND{kind=JOIN{phis, ...}, ...}, phis') = phis := phis'
          | setPhis (ND{kind=FOREACH{phis, ...}, ...}, phis') = phis := phis'
          | setPhis (nd, _) = raise Fail("setPhis on " ^ toString nd)
        fun setEdgeMask (ND{kind=JOIN{mask, ...}, ...}, mask') = mask := mask'
          | setEdgeMask (ND{kind=FOREACH{mask, ...}, ...}, mask') = mask := mask'
          | setEdgeMask (nd, _) = raise Fail("setEdgeMask on " ^ toString nd)
        fun addEdge (nd1, nd2) = (
              if hasSucc nd1
                then (
                  setSucc (nd1, nd2);
                  setPred (nd2, nd1))
                else ())
(*DEBUG*)handle ex => (
print(concat["error in addEdge(", toString nd1, ",", toString nd2, ")\n"]);
raise ex)
      (* replace the edge src-->oldDst by the edge src-->dst *)
        fun replaceOutEdge {src, oldDst, dst} = (
            (* first set the successor of src *)
              case kind src
               of COND{trueBranch, falseBranch, ...} =>
                    if same(!trueBranch, oldDst)
                      then trueBranch := dst
                      else falseBranch := dst
                | FOREACH{bodyEntry, succ, ...} =>
                    if same(!bodyEntry, oldDst)
                      then bodyEntry := dst
                      else succ := dst
                | _ => setSucc (src, dst)
              (* end case *);
            (* then set the predecessor of dst *)
              case kind oldDst
               of FOREACH{bodyExit, pred, ...} =>
                    if same(!bodyExit, src)
                      then setBodyExit (dst, src)
                      else setPred (dst, src)
                | _ => setPred (dst, src)
              (* end case *))
(*DEBUG*)handle ex => (
print(concat["error in replaceOutEdge(", toString src, ",", toString oldDst, ",", toString dst, ")\n"]);
raise ex)
      (* replace the edge oldSrc-->dst by the edge src-->dst *)
        fun replaceInEdge {oldSrc, src, dst} = (
            (* first set the successor of src; check to see if we are replacing a conditional
             * node with a conditional node.
             *)
              case (kind oldSrc, kind src)
               of (COND{trueBranch, falseBranch, ...}, COND _) =>
                    if same(!trueBranch, dst)
                      then setTrueBranch (src, dst)
                      else setFalseBranch (src, dst)
                | (FOREACH{bodyEntry, succ, ...}, FOREACH _) =>
                    if same(!bodyEntry, dst)
                      then setBodyEntry (src, dst)
                      else setSucc (src, dst)
                | _ => setSucc (src, dst)
              (* end case *);
            (* then set the predecessor of dst *)
              case kind dst
               of JOIN{preds, ...} => let
                    fun edit [] = raise Fail "replaceInEdge: cannot find predecessor"
                      | edit (nd::nds) = if same(nd, oldSrc) then src::nds else nd::edit nds
                    in
                      preds := edit (!preds)
                    end
                | FOREACH{bodyExit, pred, ...} =>
                    if same(!bodyExit, oldSrc)
                      then bodyExit := src
                      else pred := src
                | _ => setPred (dst, src)
              (* end case *))
(*DEBUG*)handle ex => (
print(concat["error in replaceInEdge(", toString oldSrc, ",", toString src, ",", toString dst, ")\n"]);
raise ex)
      (* properties *)
        fun newProp initFn =
              PropList.newProp (fn (ND{props, ...}) => props, initFn)
        fun newFlag () =
              PropList.newFlag (fn (ND{props, ...}) => props)
      end

    structure CFG =
      struct
        val empty = CFG{entry = Node.dummy, exit = Node.dummy}

        fun isEmpty (CFG{entry, exit}) =
              Node.same(entry, exit) andalso Node.isNULL entry

      (* create a basic block from a list of assignments *)
        fun mkBlock [] = empty
          | mkBlock (stm::stms) = let
              fun mkNode (ASSGN stm) = Node.mkASSIGN stm
                | mkNode (MASSGN stm) = Node.mkMASSIGN stm
                | mkNode (GASSGN stm) = Node.mkGASSIGN stm
                | mkNode (SAV stm) = Node.mkSAVE stm
              val entry = mkNode stm
              fun f (stm, prev) = let
                    val nd = mkNode stm
                    in
                      Node.addEdge (prev, nd);
                      nd
                    end
              val exit = List.foldl f entry stms
              in
                CFG{entry = entry, exit = exit}
              end

      (* entry/exit nodes of a CFG *)
        fun entry (CFG{entry = nd, ...}) = nd
        fun exit (CFG{exit = nd, ...}) = nd

      (* DFS sorting of the graph rooted at the entry node; the resulting list will
       * be in preorder with parents before children.
       *)
        fun sort (CFG{entry, ...}) = let
              val {getFn, setFn} = PropList.newFlag (fn (ND{props, ...}) => props)
              fun dfs (nd, l) =
                    if getFn nd
                      then l
                      else (
                        setFn (nd, true);
                        nd :: List.foldl dfs l (Node.succs nd))
              val nodes = dfs (entry, [])
              in
                List.app (fn nd => setFn(nd, false)) nodes;
                nodes
              end

      (* apply a function to all of the nodes in the graph rooted at the entry to the statement *)
        fun apply (f : node -> unit) (CFG{entry, ...}) = let
              val {getFn, setFn} = Node.newFlag()
              fun dfs (nd, l) =
                    if getFn nd
                      then l
                      else (
                        f nd; (* visit *)
                        setFn (nd, true);
                        nd :: List.foldl dfs l (Node.succs nd))
              val nodes = dfs (entry, [])
              in
                List.app (fn nd => setFn(nd, false)) nodes
              end

      (* delete a simple node from a CFG *)
        fun deleteNode (nd as ND{kind, ...}) = let
              val (pred, succ) = (case kind
                     of COM{pred = ref pred, succ = ref succ, ...} => (pred, succ)
                      | ASSIGN{pred = ref pred, succ = ref succ, ...} => (pred, succ)
                      | MASSIGN{pred = ref pred, succ = ref succ, ...} => (pred, succ)
                      | GASSIGN{pred = ref pred, succ = ref succ, ...} => (pred, succ)
                      | NEW{pred = ref pred, succ = ref succ, ...} => (pred, succ)
                      | SAVE{pred = ref pred, succ = ref succ, ...} => (pred, succ)
                      | _ => raise Fail(concat["unsupported deleteNode(", Node.toString nd, ")\n"])
                    (* end case *))
              in
              (* replace the predecessor edge from succ to nd with an edge from succ to pred *)
                case Node.kind succ
                 of JOIN{preds, ...} => let
                      fun edit [] = raise Fail "deleteNode: cannot find predecessor"
                        | edit (nd'::nds) = if Node.same(nd', nd) then pred::nds else nd'::edit nds
                      in
                        preds := edit (!preds)
                      end
                  | FOREACH{pred=pred', bodyExit, ...} =>
                      if Node.same(!pred', nd)
                        then pred' := pred
                        else bodyExit := pred
                  | _ => Node.setPred (succ, pred)
                (* end case *);
              (* replace the successor edge from pred to nd with an edge from pred to succ *)
                case Node.kind pred
                 of COND{trueBranch, falseBranch, ...} => (
                     (* note that we treat each branch independently, so that we handle the
                      * situation where both branches are the same node.
                      *)
                       if Node.same(!trueBranch, nd)
                         then trueBranch := succ
                         else ();
                       if Node.same(!falseBranch, nd)
                         then falseBranch := succ
                         else ())
                  | FOREACH{bodyEntry, succ=succ', ...} =>
                      if Node.same(!bodyEntry, nd)
                        then bodyEntry := succ
                        else succ' := succ
                  | NEXT _ => raise Fail "deleteNode: pred is NEXT"
                  | _ => Node.setSucc (pred, succ)
                (* end case *)
              end
(*DEBUG*)handle ex => (
print(concat["error in deleteNode(", Node.toString nd, ")\n"]);
raise ex)

      (* replace a simple node in a cfg with a subgraph *)
        fun replaceNode (oldNd as ND{kind, ...}, node) = (case kind
               of ASSIGN{pred, succ, ...} => (
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node};
                    Node.replaceInEdge {oldSrc = oldNd, src = node, dst = !succ})
                | MASSIGN{pred, succ, ...} => (
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node};
                    Node.replaceInEdge {oldSrc = oldNd, src = node, dst = !succ})
                | GASSIGN{pred, succ, ...} => (
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node};
                    Node.replaceInEdge {oldSrc = oldNd, src = node, dst = !succ})
                | NEW{pred, succ, ...} => (
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node};
                    Node.replaceInEdge {oldSrc = oldNd, src = node, dst = !succ})
                | SAVE{pred, succ, ...} => (
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node};
                    Node.replaceInEdge {oldSrc = oldNd, src = node, dst = !succ})
                | EXIT{pred, succ=ref NONE, ...} =>
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node}
                | EXIT{pred, succ=ref(SOME succ), ...} => (
                    Node.replaceOutEdge {src = !pred, oldDst = oldNd, dst = node};
                    Node.replaceInEdge {oldSrc = oldNd, src = node, dst = succ})
                | _ => raise Fail(concat[
                      "unsupported replaceNode(", Node.toString oldNd, ", ", Node.toString node, ")"
                    ])
              (* end case *))

      (* replace a simple node in a cfg with a subgraph *)
        fun replaceNodeWithCFG (nd as ND{kind, ...}, cfg as CFG{entry, exit}) =
              if isEmpty cfg
                then deleteNode nd
                else (case kind
                   of ASSIGN{pred, succ, ...} => (
                        Node.replaceOutEdge {src = !pred, oldDst = nd, dst = entry};
                        Node.replaceInEdge {oldSrc = nd, src = exit, dst = !succ})
                    | MASSIGN{pred, succ, ...} => (
                        Node.replaceOutEdge {src = !pred, oldDst = nd, dst = entry};
                        Node.replaceInEdge {oldSrc = nd, src = exit, dst = !succ})
                    | _ => raise Fail "unsupported replaceNodeWithCFG"
                  (* end case *))

      (* concatenate two CFGs *)
        fun concat (cfg1 as CFG{entry=e1, exit=x1}, cfg2 as CFG{entry=e2, exit=x2}) =
              if isEmpty cfg1 then cfg2
              else if isEmpty cfg2 then cfg1
              else (
                Node.setSucc (x1, e2);
                Node.setPred (e2, x1);
                CFG{entry = e1, exit = x2})
(*DEBUG*)handle ex => (
print(String.concat["error in concat({", Node.toString e1, ",", Node.toString x1,
"},{", Node.toString e2, ",", Node.toString x2, "})\n"]);
raise ex)

      (* prepend a node to a CFG *)
        fun prependNode (nd, cfg as CFG{entry, exit}) =
              if isEmpty cfg
                then CFG{entry=nd, exit=nd}
                else (
                  Node.setSucc (nd, entry);
                  Node.setPred (entry, nd);
                  CFG{entry=nd, exit=exit})

      (* append a node to a CFG *)
        fun appendNode (cfg as CFG{entry, exit}, nd) =
              if isEmpty cfg
                then CFG{entry=nd, exit=nd}
                else (
                  Node.setPred (nd, exit);
                  Node.setSucc (exit, nd);
                  CFG{entry=entry, exit=nd})

      (* insert a block of assignments at the beginning of the CFG.  If the CFG has an ENTRY
       * node, then the block is inserted immediatly following the entry.
       *)
        fun prependBlock ([], cfg) = cfg
          | prependBlock (stms, cfg as CFG{entry, exit}) = (case entry
               of ND{kind=ENTRY{succ}, ...} => let
                    fun mkNode (ASSGN stm) = Node.mkASSIGN stm
                      | mkNode (MASSGN stm) = Node.mkMASSIGN stm
                      | mkNode (GASSGN stm) = Node.mkGASSIGN stm
                      | mkNode (SAV stm) = Node.mkSAVE stm
                    fun f (stm, succ) = let
                          val nd = mkNode stm
                          in
                            Node.addEdge (nd, succ);
                            nd
                          end
                    val first = List.foldr f (!succ) stms
                    in
                      succ := first;
                      Node.setPred (first, entry);
                      cfg
                    end
                | _ => concat(mkBlock stms, cfg)
              (* end case *))

      (* insert a block of assignments at the end of the CFG argument  If the CFG has an EXIT
       * node, then the block is inserted immediatly before the exit.
       *)
        fun appendBlock (cfg, []) = cfg
          | appendBlock (cfg as CFG{entry, exit}, stms) = let
              fun mkNode (ASSGN stm) = Node.mkASSIGN stm
                | mkNode (MASSGN stm) = Node.mkMASSIGN stm
                | mkNode (GASSGN stm) = Node.mkGASSIGN stm
                | mkNode (SAV stm) = Node.mkSAVE stm
              fun f (stm, prev) = let
                    val nd = mkNode stm
                    in
                      Node.addEdge (prev, nd);
                      nd
                    end
              in
                case exit
                 of ND{kind=EXIT{pred, ...}, ...} => let
                      val last = List.foldl f (!pred) stms
                      in
                        pred := last;
                        Node.setSucc(last, exit);
                        cfg
                      end
                  | ND{kind=NEXT{pred, ...}, ...} => let
                      val last = List.foldl f (!pred) stms
                      in
                        pred := last;
                        Node.setSucc(last, exit);
                        cfg
                      end
                  | _ => concat(cfg, mkBlock stms)
                (* end case *)
              end
      end

  (* return a string representation of a variable binding *)
    fun vbToString VB_NONE = "NONE"
      | vbToString (VB_RHS rhs) = concat["RHS(", RHS.toString rhs, ")"]
      | vbToString (VB_MULTIOP(i, rhs)) = concat["MULTIOP(", RHS.toString rhs, ")"]
      | vbToString (VB_PHI xs) = concat[
            "PHI(",
            String.concatWithMap "," (fn NONE => "_" | SOME x => Var.toString x) xs, ")"
          ]
      | vbToString VB_PARAM = "PARAM"
      | vbToString VB_ITER = "ITER"

  (* return a string representation of a PHI node *)
    fun phiToString (y, xs) = String.concat [
            Ty.toString(Var.ty y), " ", Var.toString y, " = PHI(",
            String.concatWithMap "," (fn NONE => "_" | SOME x => Var.toString x) xs, ")"
          ]

  (* return a string representation of an assignment *)
    fun assignToString (y, rhs) =
          String.concat [Ty.toString(Var.ty y), " ", Var.toString y, " = ", RHS.toString rhs]
    fun massignToString (ys, rhs) = String.concat [
            "(", String.concatWithMap ","
              (fn y => concat[Ty.toString(Var.ty y), " ", Var.toString y]) ys,
            " = ", RHS.toString rhs
          ]
    fun assignmentToString (ASSGN asgn) = assignToString asgn
      | assignmentToString (MASSGN masgn) = massignToString masgn
      | assignmentToString (GASSGN(lhs, rhs)) = String.concat[
            GlobalVar.toString lhs, " = ", Var.toString rhs
          ]
      | assignmentToString (SAV(lhs, rhs)) = String.concat[
            StateVar.toString lhs, " = ", Var.toString rhs
          ]

  end (* SSAFn *)
