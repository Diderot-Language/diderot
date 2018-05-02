(* env.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure Env : sig

  (* the translated bindings of LowIR variables.  If the variable has a vector type,
   * then it is bound to a vector of expression trees, otherwise it is bound to
   * either a RHS (if the expression is not inlinable) or TREE (if the expression can
   * be a subexpression).
   *)
    datatype binding
      = RHS of TreeTypes.t * TreeIR.exp                 (* non-inlinable tree expression *)
      | TREE of TreeIR.exp                              (* inlinable tree expression *)
      | VEC of TreeTypes.vec_layout * TreeIR.exp list   (* composite of vector expressions *)

    val bindingToString : binding -> string

    type t

  (* create a new environment *)
    val new : TreeIR.target_info -> t

  (* create a new environment that includes function definitions for map-reduce statments *)
    val newWithFuncs : TreeIR.target_info * LowIR.func_def list -> t

  (* return the layout of a Low IR vector of a given width as a Tree IR composite vector *)
    val layoutVec : t -> int -> TreeTypes.vec_layout

    val isInlineOp : t -> LowOps.rator -> bool

  (* get the binding for a variable *)
    val useVar : t -> LowIR.var -> binding

  (* add a binding to the environment *)
    val bindVar : t * LowIR.var * binding -> unit

  (* set the definition of a variable, where the RHS is a simple expression that can be
   * replicated without code-size or performance hit (e.g., a literal or variable).
   * If the variable has tensor type, then we add a TensorRef wrapper.  The expression will
   * be wrapped in a TREE constructor.
   *)
    val bindSimple : t * LowIR.var * TreeIR.exp -> unit

  (* at the end of a block, we need to assign any pending expressions to locals.  The
   * blkStms list and the resulting statement list are in reverse order.
   *)
    val flushPending : t * TreeIR.stm list -> TreeIR.stm list

  (* lookup a function definition *)
    val lookupFunc : t * LowIR.func -> LowIR.func_def

  end = struct

    structure VT = LowIR.Var.Tbl

    datatype binding
      = RHS of TreeTypes.t * TreeIR.exp                 (* non-inlinable tree expression *)
      | TREE of TreeIR.exp                              (* inlinable tree expression *)
      | VEC of TreeTypes.vec_layout * TreeIR.exp list   (* composite of vector expressions *)

(*DEBUG*)
    fun bindingToString (RHS(ty, e)) =
          concat["RHS(", TreeTypes.toString ty, ", ", TreePP.expToString e, ")"]
      | bindingToString (TREE e) = concat["TREE(", TreePP.expToString e, ")"]
      | bindingToString (VEC(layout, _)) = concat["VEC(", VectorLayout.toString layout, ", _)"]
(*DEBUG*)

    datatype t = E of {
        tbl : (bool * binding) VT.hash_table,
        info : TreeIR.target_info,
        funcs : LowIR.func_def list
      }

    fun decCount (LowIR.V{useCnt, ...}) = let
          val n = !useCnt - 1
          in
            useCnt := n;  (n <= 0)
          end

    fun new info = E{
            tbl = VT.mkTable (256, Fail "tbl"),
            info = info,
            funcs = []
          }

    fun newWithFuncs (info, fdefs) = E{
            tbl = VT.mkTable (256, Fail "tbl"),
            info = info,
            funcs = List.filter (fn (LowIR.Func{name, ...}) => Flatten.isMapFunc name) fdefs
          }

    fun layoutVec (E{info={layout, ...}, ...}) wid = layout wid

    fun isInlineOp (E{info={isInline, ...}, ...}) rator = isInline rator

  (* use a variable.  If this is its last use, we remove it from the table.
   * NOTE: we assume that this function is _not_ called on variables that are
   * in an equivalence class; those are handled in low-to-tree.sml.
   *)
    fun useVar (env as E{tbl, ...}) = let
          val find = VT.find tbl
          val remove = VT.remove tbl
          fun use x = let
                fun removeUnused () = if (decCount x) then ignore(remove x) else ()
                in
                  case find x
                   of SOME(true, binding) => (removeUnused(); binding)
                    | SOME(false, binding) => (removeUnused(); binding)
                    | NONE => raise Fail(concat ["useVar(", LowIR.Var.toString x, ")"])
                  (* end case *)
                end
          in
            use
          end

    fun bindVar (E{tbl, ...}, x, b) = VT.insert tbl (x, (false, b))
(* DEBUG
val bindVar = fn (env, x, b) => (
      print(concat["bindVar(_, ", LowIR.Var.toString x, ", ", bindingToString b, ")\n"]);
      bindVar(env, x, b))
*)

    fun bindSimple (E{tbl, ...}, x, b) = (case TreeTypeOf.exp b
           of TreeTypes.TensorTy(shp as _::_) =>
                VT.insert tbl (x, (true, TREE(TreeIR.E_Op(TreeOps.TensorRef shp, [b]))))
            | _ => VT.insert tbl (x, (true, TREE b))
          (* end case *))
(* DEBUG
val bindSimple = fn (env, x, b) => (
      print(concat["bindSimple(_, ", LowIR.Var.toString x, ", ", TreePP.expToString b, ")\n"]);
      bindSimple(env, x, b))
*)

    fun flushPending (E{tbl, ...}, blkStms) = let
          fun doVar (x, (false, TREE e), stms) = let
                val t = Util.newLocalVar x
                in
                  VT.insert tbl (x, (true, TREE(TreeIR.E_Var t)));
                  TreeIR.S_Assign(true, t, e)::stms
                end
            | doVar (x, (false, VEC(layout, es)), stms) = let
                val xs = Util.newVectorVars layout
                val stms = ListPair.foldlEq
                      (fn (x, e, stms) => TreeIR.S_Assign(true, x, e)::stms)
                        stms (xs, es)
                in
                  VT.insert tbl (x, (true, VEC(layout, List.map TreeIR.E_Var xs)));
                  stms
                end
            | doVar (_, _, stms) = stms
          val stms = VT.foldi doVar blkStms tbl
          in
            stms
          end

    fun lookupFunc (E{funcs, ...}, f) = (
          case List.find (fn (LowIR.Func{name, ...}) => LowIR.Func.same(f, name)) funcs
           of SOME fdef => fdef
            | NONE => raise Fail(concat["lookupFunc(", LowIR.Func.toString f, ")"])
          (* end case *))

  end
