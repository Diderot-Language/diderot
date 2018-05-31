(* simplify-vars.sml
 *
 * This module analyses the use of variables in the Simple AST and rationalizes
 * their use in the following ways:
 *
 *   -- for any strand parameter that is used in a method, we create a shadow state
 *      variable
 *
 *   -- local variables that are bound to images may need to be promoted to global
 *      scope if the simplify-fields phase extended their scope.
 *
 *   -- strand-invariant state variables (except outputs) and expressions are
 *      lifted to global scope. (TODO)
 *
 * We assume that contraction has already been run and that unused variables have
 * been eliminated.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure SimplifyVars : sig

    val transform : Simple.program -> Simple.program

  end = struct

    structure S = Simple
    structure V = SimpleVar
    structure VSet = SimpleVar.Set
    structure VMap = SimpleVar.Map

  (* analyze the variable usage in a block *)
    fun analyzeBlock chkVar = let
          fun analyzeBlk (S.Block{code, ...}, bvs) = ignore (List.foldl analyzeStm bvs code)
          and analyzeStm (stm, bvs) = (case stm
                 of S.S_Var(x, NONE) => VSet.add(bvs, x)
                  | S.S_Var(x, SOME e) => (analyzeExp (e, bvs); VSet.add(bvs, x))
                  | S.S_Assign(x, e) => (analyzeExp (e, bvs); bvs)
                  | S.S_IfThenElse(x, b1, b2) => (
                      chkVar bvs x; analyzeBlk(b1, bvs); analyzeBlk(b2, bvs); bvs)
                  | S.S_Foreach(x, xs, blk) => (
                      chkVar bvs x; analyzeBlk(blk, VSet.add(bvs, x)); bvs)
                  | S.S_New(strnd, xs) => (List.app (chkVar bvs) xs; bvs)
                  | S.S_KillAll => bvs
                  | S.S_StabilizeAll => bvs
                  | S.S_Continue => bvs
                  | S.S_Die => bvs
                  | S.S_Stabilize => bvs
                  | S.S_Return x => (chkVar bvs x; bvs)
                  | S.S_Print xs => (List.app (chkVar bvs) xs; bvs)
                  | S.S_MapReduce mrs => let
                      fun analyzeMR (S.MapReduce{result, mapf, args, source, ...}) = let
                            val S.Func{body, ...} = mapf
                            val bvs' = VSet.add(bvs, source)
                            in
                              List.app (chkVar bvs') args;
                              analyzeBlk(body, bvs')
                            end
                      in
                        List.app analyzeMR mrs;
                        List.foldl
                          (fn (S.MapReduce{result, ...}, bvs) => VSet.add(bvs, result))
                            bvs mrs
                      end
                (* end case *))
          and analyzeExp (exp, bvs) = (case exp
                 of S.E_Var x => chkVar bvs x
                  | S.E_Lit _ => ()
                  | S.E_Kernel _ => ()
                  | S.E_Select(x, fld) => chkVar bvs x
                  | S.E_Apply(f, xs) => List.app (chkVar bvs) xs
                  | S.E_Prim(_, _, xs, _) => List.app (chkVar bvs) xs
                  | S.E_Tensor(xs, _) => List.app (chkVar bvs) xs
                  | S.E_Field(xs, _) => List.app (chkVar bvs) xs
                  | S.E_Tuple xs => List.app (chkVar bvs) xs
                  | S.E_Project(x, _) => chkVar bvs x
                  | S.E_Seq(xs, _) => List.app (chkVar bvs) xs
                  | S.E_Slice(x, indices, _) => chkVar bvs x
                  | S.E_Coerce{x, ...} => chkVar bvs x
                  | S.E_BorderCtl(BorderCtl.Default x, y) => (chkVar bvs x; chkVar bvs y)
                  | S.E_BorderCtl(_, x) => chkVar bvs x
                  | S.E_LoadSeq _ => ()
                  | S.E_LoadImage _ => ()
                  | S.E_InsideImage(pos, img, _) => (chkVar bvs pos; chkVar bvs img)
                  | S.E_FieldFn _ => ()
                (* end case *))
          in
            fn blk => analyzeBlk (blk, VSet.empty)
          end

  (* track if a strand parameter is referenced in a strand method *)
    local
      val {setFn, getFn} = V.newFlag()
    in
      fun markUsedInMethod x = setFn(x, true)
      fun isUsedInMethod x = getFn x
      fun clrUsedInMethodMark x = setFn(x, false)
    end (* local *)

  (* track if a local image variable is used outside its scope *)
    local
      val {setFn, getFn} = V.newFlag()
    in
      fun markPromote x = setFn(x, true)
      fun needToPromote x = getFn x
      fun clrPromote x = setFn(x, false)
    end (* local *)

  (* analyze the variable uses in the program; return true if there are any changes that
   * require variable renaming.
   *)
    fun analyze prog = let
          val anyChange = ref false
        (* analyze a method *)
          val analyzeMethod = let
                fun chkVar bvs x = (case V.kindOf x
                       of V.LocalVar => if VSet.member(bvs, x)
                            then ()
                            else (anyChange := true; markPromote x)
                        | V.StrandParam => (anyChange := true; markUsedInMethod x)
                        | _ => ()
                      (* end case *))
                in
                  analyzeBlock chkVar
                end
        (* analyse other code *)
          val analyzeCode = let
                fun chkVar bvs x = (case V.kindOf x
                       of V.LocalVar => if VSet.member(bvs, x)
                            then ()
                            else (anyChange := true; markPromote x)
                        | _ => ()
                      (* end case *))
                in
                  analyzeBlock chkVar
                end
          val S.Program{strand, create, start, update, ...} = prog
          val S.Strand{stateInit, startM, updateM, stabilizeM, ...} = strand
          in
            analyzeCode stateInit;
            Option.app analyzeMethod startM;
            analyzeMethod updateM;
            Option.app analyzeMethod stabilizeM;
            Create.app analyzeCode create;
            Option.app analyzeCode start;
            Option.app analyzeCode update;
            !anyChange
          end

  (* walk the global initialization code to determine which locals need to be promoted. *)
    fun promoteLocals (globals, blk) = let
          val promoteVars = ref []
          fun addVar x = if needToPromote x then (promoteVars := x :: !promoteVars) else ()
          fun doBlk (S.Block{code, ...}) = List.app doStm code
          and doStm stm = (case stm
                 of S.S_Var(x, NONE) => addVar x
                  | S.S_Var(x, SOME e) => addVar x
                  | S.S_IfThenElse(x, b1, b2) => (doBlk b1; doBlk b2)
                  | S.S_Foreach(x, xs, blk) => doBlk blk
                  | S.S_MapReduce _ => raise Fail "unexpected MapReduce"
                  | _ => ()
                (* end case *))
          val cnt = ref 0
          fun promote (x, (env, xs)) = let
                fun uniqueName name = if List.exists (fn x => V.nameOf x = name) xs
                      then let
                        val id = !cnt
                        in
                          cnt := id + 1;
                          uniqueName (name ^ Int.toString id)
                        end
                      else name
                val x' = V.new (uniqueName("promote_" ^ V.nameOf x), V.GlobalVar, V.typeOf x)
                in
                  (VMap.insert(env, x, x'), x'::xs)
                end
          in
            doBlk blk;
            List.foldl promote (VMap.empty, globals) (!promoteVars)
          end

  (* rename the free variables in a block according to the given mapping.  Variables that are
   * not in the domain of the map are unchanged.
   *)
    fun renameBlock env = let
          fun rename x = (case VMap.find(env, x)
                 of SOME x' => x'
                  | NONE => x
                (* end case *))
          val renameList = List.map rename
          fun renameBlk (S.Block{props, code}) = S.Block{props = props, code = List.map renameStm code}
          and renameStm stm = (case stm
                 of S.S_Var(x, NONE) =>
                    (* we don't rename x because globals and state vars are declared elsewere *)
                      stm
                  | S.S_Var(x, SOME e) => (case VMap.find(env, x)
                       of SOME x' => S.S_Assign(x', renameExp e)  (* decl of x' is elsewhere! *)
                        | NONE => S.S_Var(x, SOME(renameExp e))
                      (* end case *))
                  | S.S_Assign(x, e) => S.S_Assign(rename x, renameExp e)
                  | S.S_IfThenElse(x, b1, b2) =>
                      S.S_IfThenElse(rename x, renameBlk b1, renameBlk b2)
                  | S.S_Foreach(x, xs, blk) =>
                      S.S_Foreach(rename x, rename xs, renameBlk blk)
                  | S.S_New(strnd, xs) => S.S_New(strnd, renameList xs)
                  | S.S_KillAll => stm
                  | S.S_StabilizeAll => stm
                  | S.S_Continue => stm
                  | S.S_Die => stm
                  | S.S_Stabilize => stm
                  | S.S_Return x => S.S_Return(rename x)
                  | S.S_Print xs => S.S_Print(renameList xs)
                  | S.S_MapReduce mrs => let
                      fun renameMR (S.MapReduce{result, reduction, mapf, args, source, domain}) =
                            S.MapReduce{
                                result = result,
                                reduction = reduction,
                                mapf = mapf,
                                args = renameList args,
                                source = source,
                                domain = domain
                              }
                      in
                        S.S_MapReduce(List.map renameMR mrs)
                      end
                (* end case *))
          and renameExp exp = (case exp
                 of S.E_Var x => S.E_Var(rename x)
                  | S.E_Select(x, fld) => S.E_Select(rename x, fld)
                  | S.E_Lit _ => exp
                  | S.E_Kernel _ => exp
                  | S.E_Apply(f, xs) => S.E_Apply(f, renameList xs)
                  | S.E_Prim(f, tys, xs, ty) =>
                      S.E_Prim(f, tys, renameList xs, ty)
                  | S.E_Tensor(xs, ty) => S.E_Tensor(renameList xs, ty)
                  | S.E_Field(xs, ty) => S.E_Field(renameList xs, ty)
                  | S.E_Seq(xs, ty) => S.E_Seq(renameList xs, ty)
                  | S.E_Tuple xs => S.E_Tuple(List.map rename xs)
                  | S.E_Project(x, i) => S.E_Project(rename x, i)
                  | S.E_Slice(x, idxs, ty) => S.E_Slice(rename x, idxs, ty)
                  | S.E_Coerce{srcTy, dstTy, x} =>
                      S.E_Coerce{srcTy=srcTy, dstTy=dstTy, x=rename x}
                  | S.E_BorderCtl(ctl, x) => S.E_BorderCtl(BorderCtl.map rename ctl, rename x)
                  | S.E_LoadSeq _ => exp
                  | S.E_LoadImage _ => exp
                  | S.E_InsideImage(pos, img, s) => S.E_InsideImage(rename pos, rename img, s)
                  | S.E_FieldFn _ => exp
                (* end case *))
          in
            renameBlk
          end

  (* transform a strand definition by introducing shadow state variables for
   * parameters.
   *)
    fun doStrand (env0, strand) = let
          val S.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
        (* identify parameters that need to be shadowed *)
          val (shadowParams, initStms, env) = (case List.filter isUsedInMethod params
                 of [] => ([], [], env0)
                  | used => let
                      fun f (x, (xs, stms, env)) = let
                              val x' = V.copy(x, V.StrandStateVar)
                              val stm = S.S_Assign(x', S.E_Var x)
                              in
                                (x'::xs, stm::stms, VMap.insert(env, x, x'))
                              end
                      in
                        List.foldr f ([], [], env0) used
                      end
                (* end case *))
          val rename = renameBlock env
          in
            S.Strand{
                name = name, params = params, spatialDim = spatialDim,
                state = state @ shadowParams,
                stateInit = let
                  val S.Block{props, code} = renameBlock env0 stateInit
                  in
                    S.Block{props = props, code = code @ initStms}
                  end,
                startM = Option.map rename startM,
                updateM = rename updateM,
                stabilizeM = Option.map rename stabilizeM
              }
          end

    fun transform prog = if analyze prog
          then let
            val S.Program{
                    props, consts, inputs, constInit, globals, funcs,
                    globInit, strand, create, start, update
                  } = prog
          (* construct the initial renaming environment *)
            val (env, globals) = promoteLocals (globals, globInit)
            val rename = renameBlock env
            fun renameFunc (S.Func{f, params, body}) = S.Func{f=f, params=params, body=rename body}
            in
              S.Program{
                  props = props, consts = consts, inputs = inputs, constInit = constInit,
                  globals = globals,
                  funcs = List.map renameFunc funcs,
                  globInit = rename globInit,
                  strand = doStrand (env, strand),
                  create = Create.map rename create,
                  start = Option.map rename start,
                  update = Option.map rename update
                }
            end
          else prog

  end
