(* gen-world.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenWorld : sig

    val genStruct : CodeGenEnv.t * Atom.atom * int -> CLang.decl

  (* generate the function that creates the initial set of strands *)
    val genCreateFun : CodeGenEnv.t * TreeIR.block * TreeIR.strand * TreeIR.create -> CLang.decl

  end = struct

    structure IR = TreeIR
    structure CL = CLang
    structure RN = CxxNames
    structure Env = CodeGenEnv
    structure Util = CodeGenUtil
    structure ToCxx = TreeToCxx

  (* generate the struct declaration for the world representation *)
    fun genStruct (env : CodeGenEnv.t, strandName, nAxes) = let
          val spec = Env.target env
          fun memberVar (ty, name) = CL.mkVarDcl(ty, name)
          val members = [memberVar(CL.T_Named "strand_array", "_strands")]
          val members = if #hasGlobals spec orelse #hasConsts spec
                then memberVar(RN.globalPtrTy, "_globals") :: members
                else members
          val members = if #exec spec orelse not(#hasInputs spec)
                then members
                else memberVar(CL.T_Named "defined_inputs", "_definedInp") :: members
          val members = (case #spatialDim spec
                 of SOME d => memberVar(CL.T_Ptr(CL.T_Template("diderot::kdtree", [
                        CL.T_Named(Int.toString d), Env.realTy env, CL.T_Named "strand_array"
                      ])), "_tree") :: members
                  | NONE => members
                (* end case *))
        (* add world method decls *)
          fun memberFun (ty, name, params) = CL.mkProto(ty, name, params)
          val members = CL.mkConstrProto("world", []) :: members
          val members = CL.mkDestrProto "world" :: members
          val members = memberFun (CL.boolTy, "init", []) :: members
          val members = memberFun (CL.boolTy, "alloc", [
                  CL.PARAM([], CL.T_Array(CL.int32, SOME nAxes), "base"),
                  CL.PARAM([], CL.T_Array(CL.uint32, SOME nAxes), "size")
                ]) :: members
          val members = memberFun (CL.boolTy, "create_strands", []) :: members
          val members = if #hasStartMeth spec andalso not(TargetSpec.isParallel spec)
                then memberFun (CL.voidTy, "run_start_methods", []) :: members
                else members
          val members = memberFun (CL.uint32, "run", [CL.PARAM([], CL.uint32, "max_nsteps"), CL.PARAM([], CL.T_Ptr (CL.T_Named "int32_t"), "gpu_id")]) :: members
          val members = memberFun (CL.voidTy, "swap_state", []) :: members
          val members = if #hasGlobalStart spec
                then memberFun (CL.voidTy, "global_start", []) :: members
                else members
          val members = if #hasGlobalUpdate spec
                then memberFun (CL.voidTy, "global_update", []) :: members
                else members
          val members = if #hasStabilizeAll spec
                then memberFun (CL.voidTy, "stabilize_all", []) :: members
                else members
        (* for standalone executables that produce collections, we need kill_all in the
         * case where the step limit expires.
         *)
          val members = if TargetSpec.killAll spec
                then memberFun (CL.voidTy, "kill_all", []) :: members
                else members
          in
            CL.D_ClassDef{
                name = "world",
                args = NONE,
                from = SOME "public diderot::world_base",
                public = List.rev members,
                protected = [],
                private = []
              }
          end

    fun genCreateFun (env : CodeGenEnv.t, globInit, strand, create) = let
          val IR.Strand{name, stateInit=IR.Method{hasG, needsW, ...}, ...} = strand
          val strandName = Atom.toString name
          val env = Env.insert(env, PseudoVars.world, "this")
          val thisV = CL.mkVar "this"
          val spec = Env.target env
          val {dim, locals, prefix, loops, body} = Util.decomposeCreate create
        (* for each loop in the nest, we return the tuple
         *      (stms, loExp, hiExp, szExp, mkLoop)
         * where `stms` are the statements needed to define any new variables,
         * `loExp` and `hiExp` are CLang expressions for the low and high loop
         * bounds, `szExp` is the number of loop iterations, and mkLoop is a
         * function for buildåing the CLang representation of the loop.
         *)
          fun doLoop env (Util.ForLoop(i, lo, hi)) = let
                  val (loV, loStms) = ToCxx.trExpToVar (env, CL.intTy, "lo", lo)
                  val (hiV, hiStms) = ToCxx.trExpToVar (env, CL.intTy, "hi", hi)
                  val szE = CL.mkBinOp(CL.mkBinOp(hiV, CL.#-, loV), CL.#+, CL.mkInt 1)
                  val stms = loStms @ hiStms
                  fun mkLoop (env, mkBody) = let
                        val iV = TreeVar.name i
                        in
                          CL.mkFor(
                            CL.intTy, [(iV, loV)],
                            CL.mkBinOp(CL.mkVar iV, CL.#<=, hiV),
                            [CL.mkPostOp(CL.mkVar iV, CL.^++)],
                            mkBody (Env.insert (env, i, iV)))
                        end
                  in
                    (stms, loV, hiV, szE, mkLoop)
                  end
            | doLoop env (Util.ForeachLoop(i, seq)) = let
                  val seqTy = ToCxx.trType (env, TreeTypeOf.exp seq)
                  val (seqV, stms) = ToCxx.trExpToVar (env, seqTy, "seq", seq)
                  val szE = CL.mkDispatch(seqV, "length", [])
                  val beginE = CL.mkDispatch(seqV, "cbegin", [])
                  val endE = CL.mkDispatch(seqV, "cend", [])
                  fun mkLoop (env, mkBody) = let
                        val itV = CodeGenUtil.freshVar "it"
                        val iV = TreeVar.name i
                        val body = mkBody (Env.insert (env, i, iV))
                        in
                          CL.mkFor(
                            CL.autoTy, [(itV, beginE)],
                            CL.mkBinOp(CL.mkVar itV, CL.#!=, endE),
                            [CL.mkUnOp(CL.%++, CL.mkVar itV)],
                            CL.prependStm(
                              CL.mkDeclInit(CL.autoTy, iV, CL.mkUnOp(CL.%*, CL.mkVar itV)),
                              body))
                        end
                  in
                    (stms, CL.mkInt 0, szE, szE, mkLoop)
                  end
          fun tr env = let
                val (env, prefixCode) = TreeToCxx.trStms (env, prefix)
                val loopInfo = List.map (doLoop env) loops
              (* collect the statements that define the loop bounds *)
                val bndsStms = List.foldr
                      (fn ((stms, _, _, _, _), stms') => stms @ stms')
                        [] loopInfo
                val allocStm =
                      CL.mkIfThen(CL.mkIndirectDispatch(thisV, "alloc", [
                          CL.mkVar "base",
                          CL.mkVar "size"
                        ]),
                      (* then *)
                        CL.mkBlock [
                            CL.mkReturn(SOME(CL.mkVar "true"))
                          ])
                      (* endif *)
                fun mkArrDcl (ty, name, dim, init) = CL.mkDecl(
                      CL.T_Array(ty, SOME dim), name,
                      SOME(CL.I_Exps(List.map CL.I_Exp init)))
              (* code to allocate strands *)
                val allocCode = (case dim
                       of NONE => let (* collection of strands *)
                            val (sz1::szs) = List.map #4 loopInfo
                            val sizeExp = List.foldl
                                  (fn (sz, lhs) => CL.mkBinOp(lhs, CL.#*, sz))
                                    sz1 szs
                            in [
                              mkArrDcl(CL.int32, "base", 1, [CL.mkInt 0]),
                              mkArrDcl(CL.uint32, "size", 1, [CL.mkStaticCast(CL.uint32, sizeExp)]),
                              allocStm
                            ] end
                        | SOME d => let (* grid of strands *)
                            val baseInit = List.map #2 loopInfo
                            val sizeInit = List.map
                                  (fn info => CL.mkStaticCast(CL.uint32, #4 info))
                                    loopInfo
                            in [
                              mkArrDcl(CL.int32, "base", d, baseInit),
                              mkArrDcl(CL.uint32, "size", d, sizeInit),
                              allocStm
                            ] end
                      (* end case *))
                val idx = "ix"  (* for indexing into the strand-state array *)
                val loopCode = let
                      val idxV = CL.mkVar idx
                      fun statePtr inout =
                            CL.mkAddrOf(CL.mkSubscript(CL.mkIndirect(thisV, inout), idxV))
                      fun mkNest [] env = ToCxx.trWithLocals (env, #locals body,
                            fn env => let
                                val (env, stms') = ToCxx.trStms (env, #stms body)
                                val (_, args) = #newStm body
                              (* NOTE: the args' list must match the parameters in GenStrand *)
                                val args' = List.map (fn e => ToCxx.trExp(env, e)) args
                                val args' = let
                                      val strands = RN.strandArray env
                                      in
                                        if TargetSpec.dualState spec
                                          then CL.mkDispatch(strands, "local_state", [idxV])
                                            :: CL.mkDispatch(strands, "out_state", [idxV])
                                            :: args'
                                          else CL.mkDispatch(strands, "strand", [idxV]) :: args'
                                      end
                                val args' = if hasG
                                      then CL.mkIndirect(thisV, "_globals") :: args'
                                      else args'
                                val args' = if needsW
                                      then thisV :: args'
                                      else args'
                                val newStm = CL.mkCall(strandName ^ "_init", args')
                                val incStm = CL.mkExpStm (CL.mkUnOp(CL.%++, idxV))
                                in
                                  stms' @ [newStm, incStm]
                                end)
                        | mkNest ((_, _, _, _, mkLoop)::r) env = mkLoop (env, mkNest r)
                      in
                        mkNest loopInfo env
                      end
                val stms = prefixCode @ bndsStms @ allocCode @ [
                        CL.mkDecl(CL.uint32, idx, SOME(CL.I_Exp(CL.E_Int(0, CL.uint32)))),
                        loopCode,
                        CL.mkCall("this->swap_state", []),  (* required for dual-state *)
                        CL.mkAssign(
                          CL.mkIndirect(thisV, "_stage"),
                          CL.mkVar "diderot::POST_CREATE"),
                        CL.mkReturn(SOME(CL.mkVar "false"))
                      ]
                val stms = if #hasGlobals spec
                      then CL.mkDeclInit (
                          RN.globalPtrTy, RN.globalsVar, CL.mkIndirect(thisV, "_globals")) ::
                        stms
                      else stms
                val stms = if #hasGlobalInit spec
                      then CL.mkIfThen (CL.mkApply ("init_globals", [thisV]),
                          CL.mkReturn(SOME(CL.mkVar "true"))
                        ) :: stms
                      else stms
                in
                  stms
                end (* tr *)
          val body = TreeToCxx.trWithLocals (env, locals, tr)
          in
            CL.mkFuncDcl(CL.boolTy, "world::create_strands", [], body)
          end

  end
