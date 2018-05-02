(* scope-vars.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure ScopeVars : sig

  (* attach local variables to the innermost block that contains their scope *)
    val assignScopes : TreeVar.t list * TreeIR.block -> TreeIR.block

  end = struct

    structure IR = TreeIR
    structure M = TreeVar.Map
    structure S = TreeVar.Set

  (* representation of a nesting depth: a pair of the depth and a path to the root.
   * For D(d, p), we have d = length p.
   *)
    datatype depth_path = D of int * int list

  (* a rose-tree representation of the nesting of blocks *)
    datatype scope_nest = Scope of {
        depth : depth_path,             (* depth path for the block *)
        locals : IR.var list ref,       (* the block in the IR *)
        used : S.set,                   (* the set of locals mentioned in the block, but not
                                         * in sub-blocks
                                         *)
        inner : scope_nest list         (* nested sub-blocks *)
      }

    fun mergeDepth (D(d1, p1), D(d2, p2)) = let
          fun merge (_, [], []) = D(0, [])
            | merge (d, p as n1::p1, n2::p2) = if (n1 = n2)
                then D(d, p)
                else merge (d-1, p1, p2)
          in
            case Int.compare(d1, d2)
             of LESS => merge(d1, p1, List.drop(p2, d2-d1))
              | EQUAL => merge(d1, p1, p2)
              | GREATER => merge(d2, List.drop(p1, d1-d2), p2)
            (* end case *)
          end

  (* build a scope_nest tree and compute a mapping from local variables to their minimum
   * binding depth.
   *)
    fun buildScopeTree (def, blk) = let
          val id = ref 0
          fun pushScope (D(d, p)) = let
                val n = !id
                in
                  id := n+1;  D(d+1, n::p)
                end
          fun recordDepth d (x, depthMap) = (case M.find(depthMap, x)
                 of NONE => M.insert(depthMap, x, d)
                  | SOME d' => M.insert(depthMap, x, mergeDepth(d, d'))
                (* end case *))
          fun doBlock (IR.Block{locals, body}, def, depth : depth_path, depthMap) = let
                fun doStms ([], def, used, depthMap, kids) = let
                      val depthMap = S.foldl (recordDepth depth) depthMap used
                      in
                        (Scope{depth = depth, locals = locals, used = used, inner = kids}, depthMap)
                      end
                  | doStms (stm::stms, def, used, depthMap, kids) = (case stm
                       of IR.S_Assign(true, x, e) => let
                            val depthMap = recordDepth depth (x, depthMap)
                            in
                              doStms (stms, S.add(def, x), doExp def (e, used), depthMap, kids)
                            end
                        | IR.S_Assign(false, x, e) =>
                            doStms (stms, def, doExp def (e, S.add(used, x)), depthMap, kids)
                        | IR.S_MAssign(xs, e) => let
                            val depthMap = List.foldl (recordDepth depth) depthMap xs
                            in
                              doStms (stms, S.addList(def, xs),
                                doExp def (e, S.addList(used, xs)), depthMap, kids)
                            end
                        | IR.S_GAssign(_, e) =>
                            doStms (stms, def, doExp def (e, used), depthMap, kids)
                        | IR.S_IfThen(e, blk) => let
                            val (scope, depthMap) = doBlock (blk, def, pushScope depth, depthMap)
                            in
                              doStms (stms, def, doExp def (e, used), depthMap, scope::kids)
                            end
                        | IR.S_IfThenElse(e, blk1, blk2) => let
                            val (scope1, depthMap) = doBlock (blk1, def, pushScope depth, depthMap)
                            val (scope2, depthMap) = doBlock (blk2, def, pushScope depth, depthMap)
                            in
                              doStms (stms, def, doExp def (e, used), depthMap, scope2::scope1::kids)
                            end
                        | IR.S_For(x, lo, hi, blk) => let
                          (* NOTE: we handle variables bound in for loops as a special case,
                           * since they can be defined in the C++ for-loop syntax.
                           *)
                            val def' = S.add(def, x)
                            val depthMap = M.insert(depthMap, x, depth)
                            val (scope, depthMap) = doBlock (blk, def', pushScope depth, depthMap)
                            val used = let
                                  val doExp = doExp def
                                  in
                                    doExp (hi, doExp (lo, used))
                                  end
                            in
                              doStms (stms, def, used, depthMap, scope::kids)
                            end
                        | IR.S_Foreach(x, e, blk) => let
                          (* NOTE: we handle variables bound in foreach loops as a special case,
                           * since they can be defined in the C++ for-loop syntax.
                           *)
                            val def' = S.add(def, x)
                            val depthMap = M.insert(depthMap, x, depth)
                            val (scope, depthMap) = doBlock (blk, def', pushScope depth, depthMap)
                            in
                              doStms (stms, def, doExp def (e, used), depthMap, scope::kids)
                            end
                        | IR.S_MapReduce(mrs, src) => let
                            fun doMR ([], xs, used) = (xs, used)
                              | doMR (IR.MapReduce(x, _, _, es, _) :: mrr, xs, used) =
                                  doMR (mrr, x::xs, List.foldl (doExp def) used es)
                            val (xs, used) = doMR (mrs, [], used)
                            val (def, depthMap) = List.foldl
                                  (fn (x, (def, dm)) => (S.add(def, x), M.insert(dm, x, depth)))
                                    (def, depthMap) xs
                            in
                              doStms (stms, def, S.add(used, src), depthMap, kids)
                            end
                        | IR.S_LoadNrrd(x, _, _, _) =>
                            doStms (stms, def, S.add(used, x), depthMap, kids)
                        | IR.S_Input(_, _, _, SOME e) =>
                            doStms (stms, def, doExp def (e, used), depthMap, kids)
                        | IR.S_New(_, es) =>
                            doStms (stms, def, List.foldl (doExp def) used es, depthMap, kids)
                        | IR.S_Save(_, e) =>
                            doStms (stms, def, doExp def (e, used), depthMap, kids)
                        | IR.S_Print(_, es) =>
                            doStms (stms, def, List.foldl (doExp def) used es, depthMap, kids)
                        | IR.S_Return(SOME e) =>
                            doStms (stms, def, doExp def (e, used), depthMap, kids)
                        | _ => doStms (stms, def, used, depthMap, kids)
                      (* end case *))
                and doExp def (e, used) = (case e
                       of IR.E_State(SOME e, _) => doExp def (e, used)
                        | IR.E_Var x => if S.member(def, x) then used else S.add(used, x)
                        | IR.E_Op(_, es) => List.foldl (doExp def) used es
                        | IR.E_Apply(_, es) => List.foldl (doExp def) used es
                        | IR.E_Vec(_, _, es) => List.foldl (doExp def) used es
                        | IR.E_Cons(es, _) => List.foldl (doExp def) used es
                        | IR.E_Seq(es, _) => List.foldl (doExp def) used es
                        | IR.E_Pack(_, es) => List.foldl (doExp def) used es
                        | IR.E_VLoad(_, e, _) => doExp def (e, used)
                        | _ => used
                      (* end case *))
                in
                  doStms (body, def, S.empty, depthMap, [])
                end
          in
            doBlock (blk, def, D(0, []), M.empty)
          end

    fun matchDepth (D(0, _), D(0, _)) = true
      | matchDepth (D(d1, p1), D(d2, p2)) = (d1 = d2) andalso (hd p1 = hd p2)

  (* walk the scope_nest tree and assign variables to blocks based on their minimum
   * reference depth.
   *)
    fun assignLocals (scope, depthMap) = let
          fun assign (Scope{depth, locals, used, inner}) = let
              (* process a nested scope, which returns a list of the used variables that were
               * not bound in the nested scope.  We add these to the used variables in this
               * scope.
               *)
                val used = let
                      fun doKid (kid, used) = S.union(used, assign kid)
                      in
                        List.foldl doKid used inner
                      end
              (* partition the used variables into those that should be bound at this depth
               * and those that should be bound at an outer scope.
               *)
                val (locs, notBound) = let
                      fun partition (x, (locs, nBnd)) = (case M.find(depthMap, x)
                             of SOME d => if matchDepth(d, depth)
                                  then (x::locs, nBnd)
                                  else (locs, S.add(nBnd, x))
                              | NONE => raise Fail("no depth for " ^ TreeVar.toString x)
                            (* end case *))
                      in
                        S.foldl partition ([], S.empty) used
                      end
                in
                  locals := locs;
                  notBound
                end
          in
            ignore (assign scope)
          end

    fun assignScopes (params, blk) = (
          assignLocals (buildScopeTree (S.fromList params, blk));
          blk)

  end
