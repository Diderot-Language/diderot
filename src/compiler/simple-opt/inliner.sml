(* inliner.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * This pass eliminates the function definitions by inlining them.
 *)

structure Inliner : sig

    val transform : Simple.program -> Simple.program

  end = struct

    structure S = Simple
    structure V = SimpleVar
    structure F = SimpleFunc

  (* decrement a function's use count and if it is zero, then delete it by removing
   * it from the funcTbl and by decrementing the count of any functions called from
   * the deleted function's body.
   *)
    fun decCnt funcTbl = let
          val remove = F.Tbl.remove funcTbl
          fun decCnt' f = if (F.decCnt f <= 0)
                then let
                  fun deleteBlk (S.Block{code, ...}) = List.app deleteStm code
                  and deleteStm (S.S_Var(_, NONE)) = ()
                    | deleteStm (S.S_Var(_, SOME e)) = deleteExp e
                    | deleteStm (S.S_Assign(_, e)) = deleteExp e
                    | deleteStm (S.S_IfThenElse(_, b1, b2)) = (deleteBlk b1; deleteBlk b2)
                    | deleteStm (S.S_Foreach(_, _, b)) = deleteBlk b
                    | deleteStm (S.S_New _) = ()
                    | deleteStm S.S_KillAll = ()
                    | deleteStm S.S_StabilizeAll = ()
                    | deleteStm S.S_Continue = ()
                    | deleteStm S.S_Die = ()
                    | deleteStm S.S_Stabilize = ()
                    | deleteStm (S.S_Return _) = ()
                    | deleteStm (S.S_Print _) = ()
                    | deleteStm (S.S_MapReduce _) = raise Fail "unexpected MapReduce"
                  and deleteExp (S.E_Apply(f, _)) = decCnt' f
                    | deleteExp _ = ()
                  val S.Func{body, ...} = remove f
                  in
                    deleteBlk body
                  end
                else ()
          in
            decCnt'
          end

  (* info about the structure of a function.  We use CONT to represent functions that have
   * a single return point at the end of their top-level block, RET means that _all_ nested
   * blocks have returns, and MIXED means a mix of nested returns and top-level return, in
   * which case we do not inline.
   *)
    datatype cont = RET | CONT | MIXED

    local
    (* a property to cache the `shouldInline` test on a function *)
      val {getFn, setFn, ...} = F.newProp (fn _ => NONE : cont option)
    in
  (* return true if the body of a function has a Return statement nested inside
   * conditional control flow.
   *)
    fun inlineInfo (f, S.Block{code, ...}) = (case getFn f
           of SOME flg => flg
            | NONE => let
              (* for now, we check for functions that would require code duplication on
               * some paths to inline.  For example, inlining the following code
               *
               *        {  if (...) { ... return }
               *           else if (...) { ...A... }
               *           else { ...B... }
               *           ...C... return
               *        }
               *
               * would require duplicating "...C..." in the "A" and "B" branches, so
               * we would not inline this function.
               *)
                fun chkBlk (S.Block{code, ...}) = chkStms code
                and chkStms [] = CONT
                  | chkStms (stm :: stms) = (case chkStm stm
                       of CONT => chkStms stms
                        | info => info
                      (* end case *))
                and chkStm stm = (case stm
                       of S.S_IfThenElse(_, b1, b2) => (case (chkBlk b1, chkBlk b2)
                             of (RET, RET) => RET
                              | (CONT, CONT) => CONT
                              | _ => MIXED
                            (* end case *))
                        | S.S_Foreach(_, _, b) => chkBlk b
                        | S.S_Return _ => RET
                        | _ => CONT
                      (* end case *))
                fun chkTop [] = CONT
                  | chkTop [S.S_Return _] = CONT (* end-of-function return *)
                  | chkTop (stm::stms) = (case chkStm stm
                       of CONT => chkStms stms
                        | info => info
                      (* end case *))
                val result = chkTop code
                in
                  setFn (f, SOME result);
                  result
                end
          (* end case *))
    end (* local *)

  (* beta reduce the application "lhs = f(args)" by creating a fresh copy of f's body
   * while mapping the parameters to arguments.  The `isDcl` flag is true if `lhs'` is
   * being declared at this point.
   *)
    fun beta (isDcl, deepReturns, lhs', params, body, args) = let
        (* if the fucntion body has a deep return, then we need to predeclare the lhs.  Futhermore,
         * if the lhs is a global or state variable and the body has a deep return, then we need to
         * use a local temporary to hold the result.
         *)
          val (lhs, preCode, postCode) = let
                fun withTmp () = let
                      val tmp = V.new(V.nameOf lhs', V.LocalVar, V.typeOf lhs')
                      val tmpDcl = [S.S_Var(tmp, NONE)]
                      in
                        if isDcl
                          then (tmp, tmpDcl, [S.S_Var(lhs', SOME(S.E_Var tmp))])
                          else (tmp, tmpDcl, [S.S_Assign(lhs', S.E_Var tmp)])
                      end
                fun withLHS () = if isDcl
                      then (lhs', [S.S_Var(lhs', NONE)], [])
                      else (lhs', [], [])
                in
                  if deepReturns
                    then (case V.kindOf lhs'
                       of V.GlobalVar => withTmp ()
                        | V.StrandStateVar => withTmp ()
                        | V.StrandOutputVar => withTmp ()
                        | _ => withLHS ()
                      (* end case *))
                    else withLHS ()
                end
          fun rename env x = (case V.Map.find(env, x)
                 of SOME x' => x'
                  | NONE => if SimpleVar.hasGlobalScope x
                      then x
                      else raise Fail("unknown variable " ^ V.uniqueNameOf x)
                (* end case *))
          fun doBlock (env, S.Block{props, code}) = let
                fun f (stm, (env, stms)) = let
                        val (env, stm) = doStmt (env, stm)
                        in
                          (env, stm::stms)
                        end
                val (_, stms) = List.foldl f (env, []) code
                in
                  S.Block{props = props, code = List.rev stms}
                end
          and doStmt (env, stm) = (case stm
                 of S.S_Var(x, optE) => let
                      val x' = V.copy(x, V.kindOf x)
                      val optE' = Option.map (doExp env) optE
                      in
                        (V.Map.insert(env, x, x'), S.S_Var(x', optE'))
                      end
                  | S.S_Assign(x, e) => (env, S.S_Assign(rename env x, doExp env e))
                  | S.S_IfThenElse(x, b1, b2) =>
                      (env, S.S_IfThenElse(rename env x, doBlock(env, b1), doBlock(env, b2)))
                  | S.S_Foreach(x, xs, blk) =>
                      (env, S.S_Foreach(rename env x, rename env xs, doBlock(env, blk)))
                  | S.S_New(strnd, xs) => (env, S.S_New(strnd, List.map (rename env) xs))
                  | S.S_KillAll => (env, stm)
                  | S.S_StabilizeAll => (env, stm)
                  | S.S_Continue => (env, stm)
                  | S.S_Die => (env, stm)
                  | S.S_Stabilize => (env, stm)
                  | S.S_Return x => (env, S.S_Assign(lhs, S.E_Var(rename env x)))
                  | S.S_Print xs => (env, S.S_Print(List.map (rename env) xs))
                  | S.S_MapReduce _ => raise Fail "unexpected MapReduce in function"
                (* end case *))
          and doExp env exp = (case exp
                 of S.E_Var x => S.E_Var(rename env x)
                  | S.E_Select(x, fld) => S.E_Select(rename env x, fld)
                  | S.E_Lit _ => exp
                  | S.E_Kernel _ => exp
                  | S.E_Apply(f, xs) => S.E_Apply(SimpleFunc.use f, List.map (rename env) xs)
                  | S.E_Prim(f, tys, xs, ty) =>
                      S.E_Prim(f, tys, List.map (rename env) xs, ty)
                  | S.E_Tensor(xs, ty) => S.E_Tensor(List.map (rename env) xs, ty)
                  | S.E_Seq(xs, ty) => S.E_Seq(List.map (rename env) xs, ty)
                  | S.E_Tuple xs => S.E_Tuple(List.map (rename env) xs)
                  | S.E_Project(x, i) => S.E_Project(rename env x, i)
                  | S.E_Slice(x, idxs, ty) =>
                      S.E_Slice(rename env x, idxs, ty)
                  | S.E_Coerce{srcTy, dstTy, x} =>
                      S.E_Coerce{srcTy=srcTy, dstTy=dstTy, x=rename env x}
                  | S.E_BorderCtl(ctl, x) =>
                      S.E_BorderCtl(BorderCtl.map (rename env) ctl, rename env x)
                  | S.E_LoadSeq _ => exp
                  | S.E_LoadImage _ => exp
                  | S.E_InsideImage _ => raise Fail "unexpected InsideImage during inlining"
                (* end case *))
        (* build the initial environment by mapping parameters to arguments *)
          val env = ListPair.foldlEq
                (fn (x, x', env) => V.Map.insert(env, x, x'))
                  V.Map.empty (params, args)
          val blk as S.Block{code, ...} = doBlock (env, body)
          in
            preCode @ code @ postCode
          end

  (* inline expand user-function calls in a block *)
    fun expandBlock funcTbl = let
          val findFunc = F.Tbl.find funcTbl
          val decCnt = decCnt funcTbl
          fun inline (isDcl, lhs', f, args) = (case findFunc f
                 of NONE => NONE
                  | SOME(S.Func{params, body, ...}) => (case inlineInfo(f, body)
                       of CONT => (
                            SOME(beta (isDcl, false, lhs', params, body, args)) before decCnt f)
                        | RET => (
                            SOME(beta (isDcl, true, lhs', params, body, args)) before decCnt f)
                        | MIXED => NONE
                     (* end case *))
                (* end case *))
          fun expandBlk (S.Block{props, code}) =
                S.Block{props = props, code = List.foldr expandStm [] code}
          and expandStm (stm, stms') = (case stm
                 of S.S_Var(x, SOME(S.E_Apply(f, xs))) => (case inline(true, x, f, xs)
                       of NONE => stm :: stms'
                        | SOME stms => stms @ stms'
                       (* end case *))
                  | S.S_Assign(x, S.E_Apply(f, xs)) => (case inline(false, x, f, xs)
                       of NONE => stm :: stms'
                        | SOME stms => stms @ stms'
                       (* end case *))
                  | S.S_IfThenElse(x, b1, b2) =>
                      S.S_IfThenElse(x, expandBlk b1, expandBlk b2) :: stms'
                  | S.S_Foreach(x, xs, blk) =>
                      S.S_Foreach(x, xs, expandBlk blk) :: stms'
                  | _ => stm :: stms'
                (* end case *))
          in
            expandBlk
          end

    fun expandFunc funcTbl (S.Func{f, params, body}) = let
          val body' = expandBlock funcTbl body
          val func' = S.Func{f=f, params=params, body=body'}
          in
            F.Tbl.insert funcTbl (f, func')
          end

    fun expandStrand funcTbl = let
          val expandBlock = expandBlock funcTbl
          fun expand (S.Strand{
                name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
              }) =
                S.Strand{
                    name = name,
                    params = params,
                    spatialDim = spatialDim,
                    state = state,
                    stateInit = expandBlock stateInit,
                    startM = Option.map expandBlock startM,
                    updateM = expandBlock updateM,
                    stabilizeM = Option.map expandBlock stabilizeM
                  }
          in
            expand
          end

  (* return a function's expanded definition if it is still used, otherwise return NONE *)
    fun funcIsUsed funcTbl (S.Func{f, ...}) = if (F.useCount f > 0)
          then SOME(F.Tbl.lookup funcTbl f)
          else NONE

    fun transform (prog as S.Program{funcs=[], ...}) = prog
      | transform prog = let
          val S.Program{
                  props, consts, inputs, constInit, globals, funcs,
                  globInit, strand, create, start, update
                } = prog
        (* a table that maps function names to their definitions *)
          val funcTbl = F.Tbl.mkTable (List.length funcs, Fail "funcTbl")
        (* first we inline expand the function bodies in definition order *)
          val _ = List.app (expandFunc funcTbl) funcs
          val expandBlock = expandBlock funcTbl
          val globInit' = expandBlock globInit
          val strand' = expandStrand funcTbl strand
          val create' = Create.map expandBlock create
          val start' = Option.map expandBlock start
          val update' = Option.map expandBlock update
          in
            S.Program{
                props = props,
                consts = consts,
                inputs = inputs,
                constInit = constInit,
                globals = globals,
                globInit = globInit',
                funcs = List.mapPartial (funcIsUsed funcTbl) funcs,
                strand = strand',
                create = create',
                start = start',
                update = update'
              }
          end


  end
