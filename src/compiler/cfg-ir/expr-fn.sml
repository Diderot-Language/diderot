(* expr-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *
 * This functor implements hash-consing for IR expressions.  It can be used to implement
 * optimizations such as CSE and PRE.
 *)

signature EXPR =
  sig

    structure IR : SSA
    structure Op : OPERATORS where type rator = IR.Op.rator

    datatype expr_nd
      = GLOBAL of IR.global_var
      | STATE of IR.var option * IR.state_var
      | VAR of IR.var
      | LIT of Literal.t
      | OP of Op.rator * expr list
      | MULTIOP of int * expr           (* n'th result of rhs expression *)
      | CONS of expr list * IR.Ty.ty
      | SEQ of expr list * IR.Ty.ty
      | PHI of IR.node * expr list      (* note that PHI nodes depend on control-flow too! *)
      | EINAPP of Ein.ein * expr list
      | APPLY of IR.func * expr list
      | MAPREDUCE of Reductions.t * IR.func * expr list

    withtype expr = expr_nd HashCons.obj

    val same : expr * expr -> bool
    val toString : expr -> string

  (* hash-cons construction of expressions *)
    type tbl

    val new : unit -> tbl

    val mkGLOBAL    : tbl -> IR.global_var -> expr
    val mkSTATE     : tbl -> IR.var option * IR.state_var -> expr
    val mkVAR       : tbl -> IR.var -> expr
    val mkLIT       : tbl -> Literal.t -> expr
    val mkOP        : tbl -> Op.rator * expr list -> expr
    val mkMULTIOP   : tbl -> int * expr -> expr
    val mkCONS      : tbl -> expr list * IR.Ty.ty -> expr
    val mkSEQ       : tbl -> expr list * IR.Ty.ty -> expr
    val mkPHI       : tbl -> IR.node * expr list -> expr
    val mkEINAPP    : tbl -> Ein.ein * expr list -> expr
    val mkAPPLY     : tbl -> IR.func * expr list -> expr
    val mkMAPREDUCE : tbl -> Reductions.t * IR.func * expr list -> expr

  (* tables, sets, and maps *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = expr
    structure Set : ORD_SET where type Key.ord_key = expr
    structure Map : ORD_MAP where type Key.ord_key = expr

  end

functor ExprFn (IR : SSA) : EXPR =
  struct

    structure IR = IR
    structure Op = IR.Op
    structure HC = HashCons

    datatype expr_nd
      = GLOBAL of IR.global_var
      | STATE of IR.var option * IR.state_var
      | VAR of IR.var
      | LIT of Literal.t
      | OP of Op.rator * expr list
      | MULTIOP of int * expr           (* n'th result of rhs expression *)
      | CONS of expr list * IR.Ty.ty
      | SEQ of expr list * IR.Ty.ty
      | PHI of IR.node * expr list
      | EINAPP of Ein.ein * expr list
      | APPLY of IR.func * expr list
      | MAPREDUCE of Reductions.t * IR.func * expr list

    withtype expr = expr_nd HashCons.obj

    val same : expr * expr -> bool = HC.same
    val same' = ListPair.allEq same

    fun sameNd (GLOBAL x, GLOBAL y) = IR.GlobalVar.same(x, y)
      | sameNd (STATE(NONE, x1), STATE(NONE, x2)) = IR.StateVar.same(x1, x2)
      | sameNd (STATE(SOME x1, y1), STATE(SOME x2, y2)) =
          IR.Var.same(x1, x2) andalso IR.StateVar.same(y1, y2)
      | sameNd (VAR x, VAR y) = IR.Var.same(x, y)
      | sameNd (LIT a, LIT b) = Literal.same(a, b)
      | sameNd (OP(op1, args1), OP(op2, args2)) =
          Op.same(op1, op2) andalso Op.isPure op1 andalso same'(args1, args2)
      | sameNd (MULTIOP(i1, e1), MULTIOP(i2, e2)) = (i1 = i2) andalso same(e1, e2)
      | sameNd (CONS(args1, _), CONS(args2, _)) = same'(args1, args2)
      | sameNd (SEQ(args1, _), SEQ(args2, _)) = same'(args1, args2)
      | sameNd (PHI(nd1, args1), PHI(nd2, args2)) =
          IR.Node.same(nd1, nd2) andalso same'(args1, args2)
      | sameNd (EINAPP(ein1, a1), EINAPP(ein2, a2)) =
          EinUtil.same(ein1, ein2) andalso same'(a1, a2)
      | sameNd (APPLY(f1, a1), APPLY(f2, a2)) =
          IR.Func.same(f1, f2) andalso same'(a1, a2)
      | sameNd (MAPREDUCE(r1, f1, a1), MAPREDUCE(r2, f2, a2)) =
(* NOTE: until we add a pass to merge identical functions, this test is not very useful! *)
          IR.Func.same(f1, f2) andalso Reductions.same (r1, r2) andalso same'(a1, a2)
      | sameNd _ = false

    fun toString exp = let
          fun toS (e : expr, l) = (case #nd e
                 of GLOBAL x => IR.GlobalVar.toString x :: l
                  | STATE(NONE, x) => "self." :: IR.StateVar.toString x :: l
                  | STATE(SOME x, y) => IR.Var.toString x :: "." :: IR.StateVar.toString y :: l
                  | VAR x => IR.Var.toString x :: l
                  | LIT lit => Literal.toString lit :: l
                  | OP(rator, args) => Op.toString rator :: "(" :: argsToS (args, ")" :: l)
                  | MULTIOP(i, e) => "#" :: Int.toString i :: "(" :: toS(e, ")" :: l)
                  | CONS(args, _) => "[" :: argsToS (args, "]" :: l)
                  | SEQ(args, _) => "{" :: argsToS (args, "}" :: l)
                  | PHI(_, args) => "PHI(" :: argsToS (args, ")" :: l)
                  | EINAPP (ein, args) =>
                      "EIN{" :: EinPP.toString(ein) :: "}(" :: argsToS (args, ")" :: l)
                  | APPLY(f, args) => IR.Func.toString f :: "(" :: argsToS(args, ")" :: l)
                  | MAPREDUCE(r, f, args) =>
                      Reductions.toString r :: "(MAP " :: IR.Func.toString f
                        :: " (" :: argsToS(args, "))" :: l)
                (* end case *))
          and argsToS ([], l) = l
            | argsToS ([e], l) = toS(e, l)
            | argsToS (e::es, l) = toS(e, ","::argsToS(es, l))
          in
            String.concat (toS (exp, []))
          end

  (* hash-cons construction of expressions *)
    datatype tbl = Tbl of expr_nd HC.tbl

    fun new () = Tbl(HC.new{eq = sameNd})

    fun mkGLOBAL (Tbl tbl) x = HC.cons0 tbl (0w9941 + IR.GlobalVar.hash x, GLOBAL x)
    fun mkSTATE (Tbl tbl) (NONE, x) = HC.cons0 tbl (0w7477 + IR.StateVar.hash x, STATE(NONE, x))
      | mkSTATE (Tbl tbl) (SOME x, y) =
          HC.cons0 tbl (0w7477 + IR.Var.hash x + IR.StateVar.hash y, STATE(SOME x, y))
    fun mkVAR (Tbl tbl) x = HC.cons0 tbl (0w7919 + IR.Var.hash x, VAR x)
    fun mkLIT (Tbl tbl) a = HC.cons0 tbl (0w6997 + Literal.hash a, LIT a)
    fun mkOP (Tbl tbl) (rator, args) =
          HC.consList tbl (Op.hash rator, fn args => OP(rator, args)) args
    fun mkMULTIOP (Tbl tbl) (i, arg) =
          HC.cons1 tbl (0w3301 + Word.fromInt i, fn e => MULTIOP(i, e)) arg
    fun mkCONS (Tbl tbl) (args, ty) =
          HC.consList tbl (0w5987, fn args => CONS(args, ty)) args
    fun mkSEQ (Tbl tbl) (args, ty) =
          HC.consList tbl (0w6011, fn args => SEQ(args, ty)) args
    fun mkPHI (Tbl tbl) (nd, args) = HC.consList tbl
         (IR.Node.hash nd, fn args => PHI(nd, args)) args
    fun mkEINAPP (Tbl tbl) (rator, args) = (
          HC.consList tbl (EinUtil.hash rator, fn args => EINAPP(rator, args)) args)
    fun mkAPPLY (Tbl tbl) (f, args) =
          HC.consList tbl (IR.Func.hash f, fn args => APPLY(f, args)) args
    fun mkMAPREDUCE (Tbl tbl) (r, f, args) =
          HC.consList tbl
            (Reductions.hash r * IR.Func.hash f, fn args => MAPREDUCE(r, f, args))
              args

  (* hash tables *)
    structure Tbl = HashTableFn (
      struct
        type hash_key = expr
        fun hashVal (e : expr) = #tag e
        val sameKey = same
      end)

  (* sets and maps *)
    structure Ord =
      struct
        type ord_key = expr
        fun compare (e1 : expr, e2 : expr) = Word.compare(#tag e1, #tag e2)
      end
    structure Set = RedBlackSetFn (Ord)
    structure Map = RedBlackMapFn (Ord)

  end
