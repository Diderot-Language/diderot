(* value-numbering-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * This file contains an implementation of the hash-based value numbering
 * algorithm described in
 *
 *      Value Numbering
 *      by Preston Briggs, Keith Cooper, and Taylor Simpson
 *      CRPC-TR94517-S
 *      November 1994
 *)

functor ValueNumberingFn (D : DOMINANCE_TREE) : sig

    structure IR : SSA

    val transform : IR.program -> IR.program

  end = struct

    structure IR = D.IR
    structure E = ExprFn (IR)
    structure ValueMap = E.Map
    structure ST = Stats

    type expr = E.expr

  (********** Counters for statistics **********)
    val cntMeaninglessPhi       = ST.newCounter (IR.irName ^ ":meaningless-phi")
    val cntRedundantPhi         = ST.newCounter (IR.irName ^ ":redundant-phi")
    val cntRedundantAssign      = ST.newCounter (IR.irName ^ ":redundant-assign")
    val cntIdentityAssign       = ST.newCounter (IR.irName ^ ":identity-assign")

  (* adjust a variable's use count *)
    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun decUse (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)

    local
    (* property for mapping variables to their value number (VN), which is represented as a
     * SSA variable.  If their VN is different from themselves, then they are redundant.
     *)
      val {getFn=getVN, setFn=setVN, clrFn=clrVN, ...} = IR.Var.newProp (fn x => x)

    (* property for mapping value numbers to hash-consed expressions. *)
      val {getFn=getExp : IR.var -> expr, setFn=setExp, clrFn=clrExp, ...} =
            IR.Var.newProp (fn x => raise Fail(concat["getExp(", IR.Var.toString x, ")"]))

      datatype env = ENV of {
          avail : IR.var ValueMap.map   (* map from expressions to their value numbers, which *)
                                        (* are represented as SSA vars.  The domain are those *)
                                        (* expressions that are available. *)
        }
    in
    val emptyEnv = ENV{avail = ValueMap.empty}
  (* map variables to their hash-consed definition *)
    val getVN = getVN
    val setVN = setVN
    fun varToExp x = getExp(getVN x)
    fun bindVarToExp (ENV{avail}, x, e) = (
(*DEBUG** Log.msg' (fn () => ["** bindVarToExp: ", IR.Var.toString x, " --> ", E.toString e, "\n"]); **)
          setVN(x, x); setExp(x, e);
          ENV{avail = ValueMap.insert(avail, e, x)})
    fun expToVN (ENV{avail}, e) = ValueMap.find(avail, e)
  (* rename a variable if it's value number is different than itself *)
    fun rename x = let
          val x' = getVN x
          in
            if IR.Var.same(x, x')
              then x
              else (
(*DEBUG** Log.msg["** rename ", IR.Var.toString x, " to ", IR.Var.toString x', "\n"]; **)
                decUse x; incUse x';
                x')
          end
  (* does a variable change; i.e., get replaced by another variable? *)
    fun changed x = not(IR.Var.same(x, getVN x))
  (* clear the properties of a variable *)
    fun clearVar x = (clrVN x; clrExp x)
  (* clear the properties from the variables of a node *)
    fun clearNode nd = List.app clearVar (IR.Node.defs nd)
    end (* local *)

  (* a flag to mark identity SAVE assignments *)
    local
      val {setFn, getFn} = IR.Node.newFlag ()
    in
    fun markIdentity nd = setFn(nd, true)
    val isIdentity = getFn
    end (* local *)
    
    fun rewriteCFG cfg = let
        (* rewrite or delete a node, if necessary. *)
          fun doNode nd = (case IR.Node.kind nd
                 of IR.COND{cond as ref x, ...} =>
                      if changed x
                        then cond := rename x
                        else ()
                  | IR.JOIN{phis, ...} => let
                      fun rewritePhi ((x, ys), phis) = if changed x
                            then (
                              (* deleting redundant phi *)
                              List.app (Option.app decUse) ys;
                              phis)
                            else (x, List.map (Option.map rename) ys) :: phis
                      in
                        phis := List.foldr rewritePhi [] (!phis)
                      end
                  | IR.FOREACH{phis, src as ref x, ...} => let
                      fun rewriteVar NONE = NONE
                        | rewriteVar (SOME y) = SOME(if changed y then rename y else y)
                      fun rewritePhi (x, ys) = (x, List.map rewriteVar ys)
                      in
                        phis := List.map rewritePhi (!phis);
                        if changed x
                          then src := rename x
                          else ()
                      end
                  | IR.ASSIGN{stm=(y, rhs), ...} =>
                      if changed y
                        then (
                        (* deleting redundant assignment *)
                          IR.RHS.app decUse rhs;
                          IR.CFG.deleteNode nd)
                      else if (List.exists changed (IR.RHS.vars rhs))
                      (* rewrite node to rename variables *)
                        then IR.CFG.replaceNode(nd, IR.Node.mkASSIGN(y, IR.RHS.map rename rhs))
                        else ()
                  | IR.MASSIGN{stm=([], rhs), ...} =>
                      if (List.exists changed (IR.RHS.vars rhs))
                      (* rewrite node to rename variables *)
                        then IR.CFG.replaceNode(nd, IR.Node.mkMASSIGN([], IR.RHS.map rename rhs))
                        else ()
                  | IR.MASSIGN{stm=(ys, IR.MAPREDUCE mrs), ...} => let
                      fun doMR ([], [], _, [], []) = IR.CFG.deleteNode nd (* all were redundant *)
                        | doMR ([], [], false, _, _) = () (* no change *)
                        | doMR ([], [], true, ys, mrs) = (* update node with changes *)
                            IR.CFG.replaceNode(nd,
                              IR.Node.mkMASSIGN(List.rev ys, IR.MAPREDUCE(List.rev mrs)))
                        | doMR (y::ys, (mr as (r, f, xs))::mrs, changed', ys', mrs') =
                            if changed y
                              then (List.app decUse xs; doMR (ys, mrs, true, ys', mrs'))
                            else if (List.exists changed xs)
                              then doMR (ys, mrs, true, y::ys', (r, f, List.map rename xs)::mrs')
                              else doMR (ys, mrs, changed', y::ys', mr::mrs')
                        | doMR _ = raise Fail "impossible"
                      in
                        doMR (ys, mrs, false, [], [])
                      end
                  | IR.MASSIGN{stm=(ys, rhs), ...} =>
                      if List.all changed ys
                        then (
                        (* deleting redundant assignment *)
                          IR.RHS.app decUse rhs;
                          IR.CFG.deleteNode nd)
                      else if (List.exists changed (IR.RHS.vars rhs))
                      (* rewrite node to rename variables *)
                        then IR.CFG.replaceNode(nd, IR.Node.mkMASSIGN(ys, IR.RHS.map rename rhs))
                        else ()
                  | IR.GASSIGN{lhs, rhs, ...} =>
                      if changed rhs
                        then IR.CFG.replaceNode(nd, IR.Node.mkGASSIGN(lhs, rename rhs))
                        else ()
                  | IR.NEW{strand, args, ...} =>
                      if List.exists changed args
                        then IR.CFG.replaceNode(nd, IR.Node.mkNEW(strand, List.map rename args))
                        else ()
                  | IR.SAVE{lhs, rhs, ...} =>
                      if isIdentity nd
                        then (IR.CFG.deleteNode nd; decUse rhs)
                      else if changed rhs
                        then IR.CFG.replaceNode(nd, IR.Node.mkSAVE(lhs, rename rhs))
                        else ()
                  | IR.EXIT{kind=ExitKind.RETURN(SOME x), ...} =>
                      if changed x
                        then IR.CFG.replaceNode(nd, IR.Node.mkRETURN(SOME(rename x)))
                        else ()
                  | _ => ()
                (* end case *))
          val _ = List.app doNode (IR.CFG.sort cfg)
          in
            IR.CFG.apply clearNode cfg;
            cfg
          end

    fun transformCFG (liveIn, cfg) = let
          val tbl = E.new()
          val mkGLOBAL = E.mkGLOBAL tbl
          val mkSTATE = E.mkSTATE tbl
          val mkVAR = E.mkVAR tbl
          val mkLIT = E.mkLIT tbl
          val mkOP = E.mkOP tbl
          val mkMULTIOP = E.mkMULTIOP tbl
          val mkCONS = E.mkCONS tbl
          val mkSEQ = E.mkSEQ tbl
          val mkPHI = E.mkPHI tbl
          val mkEINAPP = E.mkEINAPP tbl
          val mkAPPLY = E.mkAPPLY tbl
          val mkMAPREDUCE = E.mkMAPREDUCE tbl
        (* bind a variable to itself (used for liveIn and loop variables *)
          fun bindToSelf (x, env) = bindVarToExp(env, x, mkVAR x)
        (* convert a list of variables to a list of expressions *)
          fun varsToExp (env, xs) = List.map varToExp xs
        (* convert an SSA RHS into a hash-consed expression *)
          fun mkExp (env, rhs) = (case rhs
                 of IR.GLOBAL x => mkGLOBAL x
                  | IR.STATE x => mkSTATE x
                  | IR.VAR x => varToExp x
                  | IR.LIT l => mkLIT l
                  | IR.OP(rator, args) => mkOP(rator, varsToExp(env, args))
                  | IR.CONS(args, ty) => mkCONS(varsToExp(env, args), ty)
                  | IR.SEQ(args, ty) => mkSEQ(varsToExp(env, args), ty)
                  | IR.EINAPP(ein, args) => mkEINAPP(ein, varsToExp(env, args))
                  | IR.APPLY(f, args) => mkAPPLY(f, varsToExp(env, args))
                  | IR.MAPREDUCE _ => raise Fail "unexpected MAPREDUCE"
                (* end case *))
        (* walk the dominator tree computing value numbers *)
          fun vn (env, nd) = let
                val env = (case IR.Node.kind nd
                       of IR.JOIN{phis, ...} => let
                            fun doPhi nd ((y, xs), env) = let
                                  val vns = List.mapPartial (Option.map getVN) xs
                                  val exp = mkPHI(nd, List.mapPartial (Option.map varToExp) xs)
                                  in
                                    case expToVN(env, exp)
                                     of SOME vn' => ((* a redundant phi node *)
(* DEBUG ** Log.msg["** redundant phi node: ", IR.phiToString (y, xs), "\n"]; **)
                                          ST.tick cntRedundantPhi;
                                          List.app (Option.app decUse) xs;
                                          setVN(y, vn');
                                          env)
                                      | NONE => bindVarToExp(env, y, exp)
                                    (* end case *)
                                  end
                            val env = List.foldr (doPhi nd) env (!phis)
                            in
                              env
                            end
                        | IR.FOREACH{phis, var, ...} => let
                          (* for loops, we handle the cycle by binding the lhs of phi nodes to
                           * themselves.
                           *)
                            val env = List.foldl
                                  (fn ((y, _), env) => bindToSelf (y, env)) env (!phis)
                            in
                              bindToSelf (var, env)
                            end
                        | IR.ASSIGN{stm=(y, rhs), ...} => let
                            val exp = mkExp(env, rhs)
                            in
                              case expToVN(env, exp)
                               of SOME vn => ((* y is redundant, so map it to vn *)
(* DEBUG ** Log.msg["** redundant assignment: ", IR.assignToString (y, rhs), **)
(* DEBUG ** "; VN[", IR.Var.toString y, "] = ", IR.Var.toString vn, "\n"]; **)
                                    ST.tick cntRedundantAssign;
(*DEBUG*)if (ST.count cntRedundantAssign > 100000) then raise Fail "too many redundant assignments" else ();
                                    setVN(y, vn);
                                    env)
                                | NONE => bindVarToExp(env, y, exp)
                              (* end case *)
                            end
                        | IR.MASSIGN{stm=(ys, IR.MAPREDUCE mrs), ...} => let
                          (* for MAPREDUCE, we treat each individual map-reduce in the fusion as
                           * a standalone operation and then re-fuse at the end
                           *)
                            fun mkExps (env, [], []) = env
                              | mkExps (env, y::ys, (r, f, xs)::mrs) = let
                                  val exp = mkMAPREDUCE(r, f, varsToExp(env, xs))
                                  in
                                    case expToVN(env, exp)
                                     of SOME vn => (  (* y is redundant, so map it to vn *)
                                          ST.tick cntRedundantAssign;
                                          setVN(y, vn);
                                          mkExps (env, ys, mrs))
                                      | NONE => mkExps (bindVarToExp(env, y, exp), ys, mrs)
                                    (* end case *)
                                  end
                              | mkExps _ = raise Fail "inconsistent MAPREDUCE"
                            in
                              mkExps (env, ys, mrs)
                            end
                        | IR.MASSIGN{stm=(ys, rhs), ...} => let
                            val rhsExp = mkExp(env, rhs)
                            fun mkExps (env, _, []) = env
                              | mkExps (env, i, y::ys) = let
                                  val exp = mkMULTIOP(i, rhsExp)
                                  in
                                    case expToVN(env, exp)
                                     of SOME vn => ((* y is redundant, so map it to vn *)
                                          ST.tick cntRedundantAssign;
                                          setVN(y, vn);
                                          mkExps (env, i+1, ys))
                                      | NONE => mkExps (bindVarToExp(env, y, exp), i+1, ys)
                                    (* end case *)
                                  end
                            in
                              mkExps (env, 0, ys)
                            end
                        | IR.SAVE{lhs, rhs, ...} =>
                          (* check for `self.x = self.x` assignments, where `x` is not
                           * a shared state variable.
                           *)
                            if not(IR.StateVar.isShared lhs)
                              then (case HashCons.node(varToExp rhs)
                                 of E.STATE(NONE, sv) => if IR.StateVar.same(lhs, sv)
                                      then (
                                        ST.tick cntIdentityAssign;
                                        markIdentity nd;
                                        env)
                                      else env
                                  | _ => env
                                (* end case *))
                              else env
                       | _ => env
                      (* end case *))
                in
                  List.app (fn nd => vn (env, nd)) (D.children nd)
                end
        (* define the initial environment by mapping the liveIn variables to themselves *)
          val env = List.foldl bindToSelf emptyEnv liveIn
          in
            D.computeTree cfg;
          (* compute value numbers over the dominance tree *)
            vn (env, IR.CFG.entry cfg);
            D.clear cfg;
          (* delete and rewrite nodes as necessary *)
            rewriteCFG cfg before (List.app clearVar liveIn)
          end
handle ex => (D.printTree(TextIO.stdOut, cfg); raise ex)

    fun transform prog = let
          val IR.Program{
                  props, consts, inputs, constInit, globals,
                  funcs, globInit, strand, create, start, update
                } = prog
        (* transform a function *)
          fun transformFunc (IR.Func{name, params, body}) = IR.Func{
                  name = name, params = params, body = transformCFG (params, body)
                }
        (* transform a strand *)
          fun transformStrand strand = let
                val IR.Strand{
                        name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                      } = strand
                val stateInit = transformCFG (params, stateInit)
(* QUESTION: what if a state variable becomes redundant? *)
                fun transformMeth body = transformCFG ([], body)
                in
                  IR.Strand{
                      name = name,
                      params = params,
                      spatialDim = spatialDim,
                      state = state,
                      stateInit = stateInit,
                      startM = Option.map transformMeth startM,
                      updateM = transformMeth updateM,
                      stabilizeM = Option.map transformMeth stabilizeM
                    }
                end
        (* transform the initial-strand creation code *)
          val create = Create.map (fn code => transformCFG ([], code)) create
          in
            IR.Program{
                props = props,
                consts = consts,
                inputs = inputs,
                constInit = transformCFG ([], constInit),
                globals = globals,
                funcs = List.map transformFunc funcs,
                globInit = transformCFG ([], globInit),
                strand = transformStrand strand,
                create = create,
                start = Option.map (fn cfg => transformCFG ([], cfg)) start,
                update = Option.map (fn cfg => transformCFG ([], cfg)) update
              }
          end

  end
