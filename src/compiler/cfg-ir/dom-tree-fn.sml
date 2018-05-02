(* dom-tree-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

signature DOMINANCE_TREE =
  sig

    structure IR : SSA

  (* compute the dominance-tree information for a CFG *)
    val computeTree : IR.cfg -> unit

  (* return the dominance-tree children associated with a given node.
   * For a conditional, the true child proceeds the false child (if
   * both are dominance-tree children)
   *)
    val children : IR.node -> IR.node list

  (* cleanup the node properties used to store information about the tree *)
    val clear : IR.cfg -> unit

  (* print the tree (for debugging purposes) *)
    val printTree : TextIO.outstream * IR.cfg -> unit

  end

functor DomTreeFn (IR : SSA) : DOMINANCE_TREE = struct

    structure IR = IR
    structure Nd = IR.Node

  (* a property for COND nodes that maps them to their associated JOIN node
   * (if one exists).
   *)
    val {
          peekFn = getJoin : IR.node -> IR.node option,
          setFn = setJoin, clrFn = clrJoin, ...
        } = Nd.newProp (fn _ => raise Fail "join property")

  (* an element on the stack of open COND and FOREACH nodes. *)
    datatype open_join
      = THEN_BR of {cond : IR.node, elseBr : IR.node}
      | ELSE_BR of {cond : IR.node}
      | LOOP_HEAD of IR.node

(*DEBUG*
    fun dumpStk (prefix, stk) = let
          fun joinToString (THEN_BR{cond, elseBr}) =
                concat[Nd.toString cond, "?", Nd.toString elseBr]
            | joinToString (ELSE_BR{cond}) = Nd.toString cond ^ ":"
            | joinToString (LOOP_HEAD nd) = "*" ^ Nd.toString nd
          in
            print (concat[
                "  ", prefix, "; stk = [ ", String.concatWithMap " " joinToString stk, " ]\n"
              ])
          end
*DEBUG*)

  (* compute the dominance-tree information for a CFG.  Most of the immediate
   * dominator info is already present in the graph, but there are two interesting
   * cases: JOIN nodes and FOREACH nodes.  We also need tree edges from
   * COND nodes to their matching JOIN (it it exists).  We use properties
   * to implemented these edges
   *)
    fun computeTree cfg = let
          fun walk (joinStk : open_join list, nd) = (case Nd.kind nd
                 of IR.NULL => raise Fail "unexpected NULL node"
                  | IR.ENTRY{succ} => walk (joinStk, !succ)
                  | IR.JOIN{mask, succ, ...} => (
                      case joinStk
                       of THEN_BR{cond, elseBr}::r => (
                            setJoin(cond, nd);
                            walk (ELSE_BR{cond=cond}::r, elseBr))
                        | ELSE_BR{cond}::r => (
                          (* if this is the first visit to this JOIN (because the "then" path
                           * has an exit on it), then set the join of the conditional.
                           *)
                            case getJoin cond
                             of NONE => setJoin(cond, nd)
                              | SOME _ => ()
                            (* end case *);
                            walk (r, !succ))
                        | _ => raise Fail("unmatched " ^ Nd.toString nd)
                      (* end case *))
                  | IR.COND{trueBranch, falseBranch, ...} =>
                      walk (THEN_BR{cond=nd, elseBr = !falseBranch}::joinStk, !trueBranch)
                  | IR.FOREACH{bodyEntry, succ, ...} =>
                      walk (LOOP_HEAD(!succ)::joinStk, !bodyEntry)
                  | IR.NEXT{succ, ...} => (case joinStk
                       of LOOP_HEAD exit :: r => (* finished loop body, so walk the successor *)
                            walk (r, exit)
                        | _ => raise Fail "unmatched NEXT"
                      (* end case *))
                  | IR.COM{succ, ...} => walk (joinStk, !succ)
                  | IR.ASSIGN{succ, ...} => walk (joinStk, !succ)
                  | IR.MASSIGN{succ, ...} => walk (joinStk, !succ)
                  | IR.GASSIGN{succ, ...} => walk (joinStk, !succ)
                  | IR.NEW{succ, ...} => walk (joinStk, !succ)
                  | IR.SAVE{succ, ...} => walk (joinStk, !succ)
                  | IR.EXIT _ => resume joinStk
                (* end case *))
          and resume [] = ()
            | resume (THEN_BR{cond, elseBr}::r) = walk (ELSE_BR{cond=cond}::r, elseBr)
            | resume (stk as ELSE_BR{cond}::r) = (case getJoin cond
                 of NONE => resume r (* the cond is unreachable *)
                  | SOME nd' => walk(stk, nd')
                (* end case *))
            | resume (LOOP_HEAD exit :: r) = walk(r, exit)
          in
            walk ([], IR.CFG.entry cfg)
          end

  (* get the dominance-tree children associated with a given node *)
    fun children nd = let
        (* check to see if a successor node is immediately dominated by nd. *)
          fun maybeSucc (ref nd') = (case Nd.kind nd'
                 of IR.JOIN{preds, ...} => (case !preds
                       of [_] => [nd'] (* JOIN with single predecessor *)
                        | _ => []
                      (* end case *))
                  | IR.COM{succ, ...} => maybeSucc succ (* skip comments *)
                  | _ => [nd']
                (* end case *))
          in
            case Nd.kind nd
             of IR.NULL => raise Fail "unexpected NULL node"
              | IR.ENTRY{succ} => maybeSucc succ
              | IR.JOIN{succ, ...} => maybeSucc succ
              | IR.COND{trueBranch, falseBranch, ...} => (case getJoin nd
                   of SOME nd' => maybeSucc trueBranch @ maybeSucc falseBranch @ [nd']
                    | NONE => [!trueBranch, !falseBranch]
                  (* end case *))
              | IR.FOREACH{bodyEntry, succ, ...} => !bodyEntry :: maybeSucc succ
              | IR.NEXT{succ, ...} => []
              | IR.COM{succ, ...} => maybeSucc succ
              | IR.ASSIGN{succ, ...} => maybeSucc succ
              | IR.MASSIGN{succ, ...} => maybeSucc succ
              | IR.GASSIGN{succ, ...} => maybeSucc succ
              | IR.NEW{succ, ...} => maybeSucc succ
              | IR.SAVE{succ, ...} => maybeSucc succ
              | IR.EXIT _ => []
            (* end case *)
          end

  (* cleanup the node properties used to store information about the tree *)
    fun clear cfg = IR.CFG.apply clrJoin cfg

  (* print the tree (for debugging purposes) *)
    fun printTree (outS, root) = let
          val {getFn, setFn} = Nd.newFlag()  (* for blackholing *)
          fun pr s = TextIO.output(outS, s)
          fun prIndent [] = ()
            | prIndent (s::r) = (prIndent r; pr s)
          fun prTree (indent, nd) = if getFn nd
                then pr(Nd.toString nd ^ " !!!!! LOOP IN TREE !!!!!\n")
                else (
                  pr(Nd.toString nd ^ "\n");
                  setFn (nd, true);
                  case children nd
                   of [kid] => ( (* no indentation for single immediate dominator *)
                        prIndent indent;
                        prTree (indent, kid))
                    | kids => prKids (indent, children nd)
                  (* end case *);
                  setFn (nd, false))
          and prKids (_, []) = ()
            | prKids (indent, [kid]) = (
                prIndent ("┗>" :: indent);
                prTree ("  " :: indent, kid))
            | prKids (indent, kid::kids) = (
                prIndent ("┗>" :: indent);
                prTree ("| "  :: indent, kid);
                prKids (indent, kids))
          in
            prTree ([], IR.CFG.entry root)
          end

  end
