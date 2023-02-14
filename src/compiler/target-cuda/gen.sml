(* gen.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2023 The University of Chicago
 * All rights reserved.
 *)

structure CudaGen : CODEGEN =
  struct

    structure IR = TreeIR (* Contains globals *)
    structure GV = TreeGlobalVar
    structure CL = CLang
    structure CGU = CodeGenUtil
    structure Env = CodeGenEnv
    structure RN = CxxNames
    structure Out = CodeOutput
    structure CGen = CPUGen
    structure TSpec = TargetSpec
    structure TyCxx = TypeToCxx
    structure TrCxx = TreeToCxx
    structure TrTy = TreeTypes
    structure Frags = CUDAFragments

     fun mkEnv spec = if TSpec.dualState spec
          then Env.new {
              world = RN.worldVar,
              global = RN.globalsVar,
              selfLocal = RN.selfLocalVar,
              selfIn = RN.selfInVar,
              selfOut = RN.selfOutVar,
              spec = spec
            }
          else Env.new {
              world = RN.worldVar,
              global = RN.globalsVar,
              selfLocal = RN.selfVar,
              selfIn = RN.selfVar,
              selfOut = RN.selfVar,
              spec = spec
            }

    val openCudaOut = Out.openOut {ext = "cu", ppDecl = PrintAsCUDA.output}

    fun createBaseName (spec: TSpec.t) =
          OS.Path.joinDirFile{dir = #outDir spec, file = #outBase spec}

(* FIXME: switch to tupled arguments *)
    fun mkCUDAMallocStm var size = CL.mkCall("cuda_err", [
            CL.mkApply("cudaMalloc", [(CL.mkAddrOf var), size]),
            CL.mkStr("CUDA allocation failed.")
          ])

    fun mkCUDACopyToGPU src dest size = CL.mkCall("cuda_err", [
            CL.mkApply ("cudaMemcpyAsync", [
                CL.mkAddrOf src, CL.mkAddrOf dest, size,
                CL.mkVar "cudaMemcpyHostToDevice"
              ])
          ])

    fun mkCopyImage name g_var gpu_g_var imgty spec = let
          val varName = "gv_" ^ name;
          val var = CL.mkIndirect(g_var, varName);
          val copyFn = CL.mkSelect(var, "copy_to_device_async");
          in [
            CL.mkCall("cuda_err_passthrough", [
                CL.mkApplyExp(copyFn,[]),
                CL.mkStr(concat["Failed to copy image \"", varName ,"\" to GPU"])
              ])
          ] end

    fun copy_other tt name gvg_var gpu_g_var spec = []

    fun copy_list name g_var gpu_g_var spec (SOME _) = let
          val varName = "gv_" ^ name;
          val var = CL.mkIndirect(g_var, varName);
          val copyFn = CL.mkSelect (var, "copy_to_device_async");
          val target_var = CL.mkIndirect(gpu_g_var, varName);
          val target_ref = CL.mkAddrOf target_var
          in
            [CL.mkCall("cuda_err_passthrough", [CL.mkApplyExp(copyFn, [target_ref]), CL.mkStr(concat["Failed to copy list \"", varName ,"\" to GPU"])])]
          end
        | copy_list name gvg_var gpu_g_var spec NONE = raise Fail "Not implemented: dynamic length list copy"

    fun copy_field_match (TrTy.TensorRefTy lengths) name gvg_var gpu_g_var spec = raise Fail "not Implemented: tensorref"
      | copy_field_match (TrTy.ImageTy info) name gvg_var gpu_g_var spec = mkCopyImage name gvg_var gpu_g_var (TrTy.ImageTy info) spec
      | copy_field_match (TrTy.TupleTy ttypes) name gvg_var gpu_g_var spec = raise Fail "not Implemented: tuple"
      | copy_field_match (TrTy.SeqTy (stype_i,len_opt)) name gvg_var gpu_g_var spec = copy_list name gvg_var gpu_g_var spec len_opt
      | copy_field_match tt name gvg_var gpu_g_var spec = copy_other tt name gvg_var gpu_g_var spec

    fun copy_field (IR.GV{ty, name, ...}) gvg_var gpu_g_var =  copy_field_match ty name gvg_var gpu_g_var

    fun copy_fields globals gvg_var gpu_g_var spec = List.concat (List.map (fn gv => copy_field gv gvg_var gpu_g_var spec) globals)

    fun create_prep_copy global_var global_ty = let
          val target_var = CL.mkVar "gpu_prep_globals";
          val target_var_assign = CL.mkAssign(target_var, (CL.mkNew(global_ty, [CL.mkVar "globals"])));
          in
            (target_var, target_var_assign)
          end

    fun copy_globals globals inputs consts spec = let
        val gvs = List.foldr (fn (inp, gvs) => Inputs.varOf inp :: gvs) globals inputs
        val gvs = List.foldr (op ::) gvs consts
        val prep_copy_var = CL.mkVar "gpu_prep_globals";
        val globTy = CL.structTy(CL.T_Named "globals")
        val prep_copy_param = CL.PARAM ([] , CL.T_Ptr(globTy), "gpu_prep_globals")
        val gpu_var = CL.mkVar "gpu_globals";
        val gpu_param = CL.PARAM ([], CL.T_Ptr(globTy), "gpu_globals");
        val return0 = CL.mkReturn (SOME (CL.mkVar "cudaSuccess"))
        val field_copy_stmt = copy_fields gvs prep_copy_var gpu_var spec;
        val copy_struct = CL.mkCall("cuda_err_passthrough", [CL.mkApply("cudaMemcpyAsync", [gpu_var, prep_copy_var, CL.mkSizeof(globTy), CL.mkVar "cudaMemcpyHostToDevice"]), CL.mkStr("Failed to copy global structure to GPU")])
        val block = CL.mkBlock (field_copy_stmt @ [copy_struct, return0]);
    in
      [ CL.D_Verbatim ["#ifndef DIDEROT_NO_GLOBALS"],
        CL.mkFuncDcl(CL.T_Named "cudaError_t", "copy_globals", [prep_copy_param, gpu_param], block),
        CL.D_Verbatim ["#endif"]
      ]
    end


    fun mkEnv spec = if TSpec.dualState spec
          then Env.new {
              world = RN.worldVar,
              global = RN.globalsVar,
              selfLocal = RN.selfLocalVar,
              selfIn = RN.selfInVar,
              selfOut = RN.selfOutVar,
              spec = spec
            }
          else Env.new {
              world = RN.worldVar,
              global = RN.globalsVar,
              selfLocal = RN.selfVar,
              selfIn = RN.selfVar,
              selfOut = RN.selfVar,
              spec = spec
            }

    type method_props = {present : bool, needsW : bool, hasG : bool}

  (* create the target-specific substitution list *)
    fun mkSubs (spec, strand, create) = let
          val IR.Strand{name, stateInit, startM, updateM, stabilizeM, ...} = strand
        (* make the parameter-type string for a strand method.  This code should be kept in
         * sync with the genMethodDef function in gen-strand.sml
         *)
          fun mkMethodParams (_, name, {present=false, ...} : method_props) = (name, "")
            | mkMethodParams (needWorker, name, {needsW, hasG, ...}) = let
                val params = GenStrand.methodParams {
                        world = fn _ => "world *wrld, ",
                        wcache = fn _ => "worker_cache *strands, ",
                        globals = fn _ => "globals *glob, "
                      } spec {
                        cxt = (), needWorker = needWorker, needsW = needsW, hasG = hasG
                      }
                in
                  (name, String.concat params)
                end
        (* make the argument string for a strand method.  This code should be kept in
         * sync with the code above and the genMethodDef function in gen-strand.sml
         *)
          fun mkMethodArgs (_, _, name, {present=false, ...} : method_props) = (name, "")
            | mkMethodArgs (needWorker, inWrld, name, {needsW, hasG, ...}) = let
                val args = GenStrand.methodParams {
                        world = fn true => "this, " | false => "wrld, ",
                        wcache = fn _ => "strands, ",
                        globals = fn _ => "glob, "
                      } spec {
                        cxt = inWrld, needWorker = needWorker, needsW = needsW, hasG = hasG
                      }
                in
                  (name, String.concat args)
                end

           fun mkCudaMethodArgs (_, _, name, {present=false, ...} : method_props) = (name, "")
            | mkCudaMethodArgs (needWorker, inWrld, name, {needsW, hasG, ...}) = let
                val args = GenStrand.methodParams {
                        world = fn true => "this, " | false => "gpu_wrld, ",
                        wcache = fn _ => "gpu_strands, ",
                        globals = fn _ => "gpu_globals, "
                      } spec {
                        cxt = inWrld, needWorker = needWorker, needsW = needsW, hasG = hasG
                      }
                in
                  (name, String.concat (args))
                end

        (* get method properties *)
          fun methodProps NONE = {present=false, needsW=false, hasG=false}
            | methodProps (SOME(IR.Method{needsW, hasG, ...})) =
                {present=true, needsW=needsW, hasG=hasG}
        (* add properties from the second method to the first *)
          fun combineProps ({present, needsW, hasG}, props : method_props) = {
                  present = present,
                  needsW = present andalso (needsW orelse #needsW props),
                  hasG = present andalso (hasG orelse #hasG props)
                }

          fun requiresGlobalWorldCopy [] = (false, false)
            | requiresGlobalWorldCopy (NONE :: ms) = requiresGlobalWorldCopy ms
            | requiresGlobalWorldCopy (SOME (IR.Method{needsW, hasG, ...}) :: ms) = let
                val (otherNeedsW, otherHasG) = requiresGlobalWorldCopy ms
              in
                (needsW orelse otherNeedsW, hasG orelse otherHasG)
              end
          (* extra parameters/arguments for various functions that are defined in the code
           * fragments.  There are some inclusion requirements on these:
           *
           *    START_PARAMS ⊆ STABILIZE_PARAMS         (because run_start_methods)
           *
           * There are two argument forms, which depend on whether the calling function
           * is a world method or not.
           *)
          val updateMProps = methodProps (SOME updateM)
          val stabilizeMProps = methodProps stabilizeM
          val startMProps = combineProps (methodProps startM, stabilizeMProps)
          val (reqWorldCopy, reqGlobalCopy) = requiresGlobalWorldCopy([(SOME updateM), startM, stabilizeM])
          in [
            ("CFILE",                   OS.Path.joinBaseExt{base= #outBase spec, ext= SOME "c"}),
            ("CXXFILE",                 OS.Path.joinBaseExt{base= #outBase spec, ext= SOME "cxx"}),
            ("H_FILE",                  OS.Path.joinBaseExt{base= #outBase spec, ext= SOME "h"}),
            ("LOG_FILE",                OS.Path.joinBaseExt{base= #outBase spec, ext= SOME "evtlog"}),
            ("PREFIX",                  #namespace spec),
            ("SRCFILE",                 #srcFile spec),
            ("DATE",                    Date.toString(Date.fromTimeLocal(Time.now()))),
            ("PROG_NAME",               #outBase spec),
            ("STRAND",                  Atom.toString name),
            ("STRANDTY",                Atom.toString name ^ "_strand"),
            ("IS_GRID",                 Bool.toString(#isGrid spec)),
            ("NUM_AXES",                Int.toString(Option.getOpt(Create.arrayDim create, 1))),
            ("SPATIAL_DIM",             Int.toString(Option.getOpt(#spatialDim spec, 0))),
            ("DIDEROTC_CMD",            #diderotc spec),
            ("DIDEROTC_ARGV",           String.concatWith " " (#argv spec)),
            ("DIDEROTC_VERSION",        #version spec),
            ("DIDEROT_FLOAT_PRECISION", TSpec.floatPrecisionDef spec),
            ("DIDEROT_INT_PRECISION",   TSpec.intPrecisionDef spec),
            ("DIDEROT_TARGET",          TSpec.targetDef spec),
            ("REALTY",                  if #double spec then "double" else "float"),
            ("INTTY",                   if #longint spec then "int64_t" else "int32_t"),
            ("DIDEROT_REAL_SIZE",       if #double spec then "64" else "32"),
            ("DIDEROT_INT_SIZE",        if #longint spec then "64" else "32"),
            ("BOOLTY",                  SizeOf.c_bool),
            ("CUDA_INNER_BLK_SIZE",     "128"),
            ("CUDA_NO_BLOCKS",          "256"),
            ("CUDA_WORK_BLK_SIZES",     "64"),
            ("NEED_WORLD_COPY",         if reqGlobalCopy then "#define GLOBAL_COPY_REQUIRED" else ""),
            ("NEED_GLOBAL_COPY",        if reqWorldCopy then "#define WORLD_COPY_REQUIRED" else ""),
          (* START_{PARAMS,ARGS,ARGS_IN_WRLD} used for
           *    worker_cache::run_start_methods (@START_PARAMS@ ...)
           *    worker_cache::strand_start (@START_PARAMS@ ...)
           *    strand_status @STRAND@_start (@START_PARAMS@ ...)
           *)
            mkMethodParams (true, "START_PARAMS", startMProps),
            mkMethodArgs (true, true, "START_ARGS_IN_WRLD", startMProps),
            mkMethodArgs (true, false, "START_ARGS", startMProps),
          (* UPDATE_{PARAMS,ARGS,ARGS_IN_WRLD} used for
           *    worker_cache::run_start_methods (@START_PARAMS@ ...)
           *    worker_cache::strand_start (@START_PARAMS@ ...)
           *    strand_status @STRAND@_start (@START_PARAMS@ ...)
           *)
            mkMethodParams (true, "UPDATE_PARAMS", updateMProps),
            mkMethodArgs (true, true, "UPDATE_ARGS_IN_WRLD", updateMProps),
            mkMethodArgs (true, false, "UPDATE_ARGS", updateMProps),
            mkCudaMethodArgs (true, false, "CUDA_UPDATE_ARGS", updateMProps),

          (* used for
           *    worker_cache::strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@ ...)
           *    strand_stabilize (@STABILIZE_PARAMS@ ...)
           *    strand_array::strand_stabilize (@STABILIZE_PARAMS@ ...)
           *)
            mkMethodParams (false, "STABILIZE_PARAMS", stabilizeMProps),
            mkMethodArgs (false, true, "STABILIZE_ARGS_IN_WRLD", stabilizeMProps),
            mkMethodArgs (false, false, "STABILIZE_ARGS", stabilizeMProps)
          ]
          end

    fun condCons (true, x, xs) = x::xs
      | condCons (false, _, xs) = xs

    fun verbFrag (spec, parFrag, seqFrag, subs) =
          CL.verbatimDcl [if (TSpec.isParallel spec) then parFrag else seqFrag] subs

  (* translate function parameters and initialize the environment *)
    fun trParams (spec, params) = List.foldr
         (fn (x, (env, ps)) => let
             val (env, p) = TreeToCxx.trParam(env, x)
             in
               (env, p::ps)
             end)
           (Env.empty spec, []) params

  (* generate code for a user-defined function *)
    fun genFunc spec (IR.Func{name, params, body}) = let
          val (resTy, _) = TreeFunc.ty name
          val (env, params) = trParams (spec, params)
          val (env, params) = if TreeFunc.hasGlobals name
                then (
                    Env.insert(env, PseudoVars.global, RN.globalsVar),
                    CL.PARAM([], RN.globalPtrTy, RN.globalsVar) :: params
                  )
                else (env, params)
          val (env, params) = if TreeFunc.needsWorld name
                then (
                    Env.insert(env, PseudoVars.world, RN.worldVar),
                    CL.PARAM([], RN.worldPtrTy, RN.worldVar) :: params
                  )
                else (env, params)
          in
            CL.D_Func([], TreeToCxx.trType(env, resTy), [], TreeFunc.qname name, params,
              TreeToCxx.trBlock (env, body))
          end

(* QUESTION: should be move the init_consts code into a constructor for the
 * globals struct and make init_globals a member of the world struct?
 *)

    fun genInitConsts (env, IR.Block{locals, body}) = let
          val body = IR.Block{locals=locals, body=body}
          in
            CL.D_Func(["static"], CL.voidTy, [], "init_consts",
              [RN.worldParam],
              GenUtil.genBodyWithGlobPtr (env, body))
          end

    fun genInitGlobals (env, inputs, globals, IR.Block{locals, body}) = let
          val body = GenUtil.genBodyWithGlobPtr (env, IR.Block{locals=locals, body=body})
        (* register global references to image data *)
          val regStms = let
                fun isImg gv = (case GV.ty gv
                       of TreeTypes.ImageTy _ => true
                        | _ => false
                      (* end case *))
                val imgs = List.filter isImg globals
                val imgs = List.foldr
                      (fn (inp, gvs) => let val gv = Inputs.varOf inp
                        in
                          if isImg gv then gv :: gvs else gvs
                        end) imgs inputs
                fun mkStm gv = CL.mkExpStm(
                      CL.mkDispatch(CL.mkIndirect(CL.mkVar "glob", GV.qname gv),
                        "register_global", []))
                in
                (* the init_globals function returns false on success *)
                  List.map mkStm imgs @ [CL.mkReturn(SOME(CL.mkVar "false"))]
                end
          val body = CL.mkBlock(CL.unBlock body @ regStms)
          in
            CL.D_Func(["static"], CL.boolTy, [], "init_globals", [RN.worldParam], body)
          end

  (* specialize the fragments that implement the run and run_start_methods world methods *)
    fun runFrag (spec : TSpec.t, subs) = let
          val frags = (case (#cudaBatch spec)
                 of (false) => [Frags.runCuda]
                  | (true) => [Frags.runCudaBatch]
                (* end case *))
          val frags = (case (#hasStartMeth spec)
                 of (true) => Frags.runStartMethods :: frags
                  | _ => frags
                (* end case *))
          in
            [CL.verbatimDcl frags subs]
          end

    fun compile (spec : TSpec.t, basename) = let
        (* generate the C compiler flags *)
          val cflags = ["-I" ^ Paths.diderotInclude(), "-I" ^ Paths.teemInclude()]
          val cflags = #cuda Paths.cxxFlags :: cflags
          val cflags = if #debug spec
                then #debug Paths.cxxFlags :: cflags
                else #ndebug Paths.cxxFlags :: cflags
          in
            RunCC.compile (basename, cflags)
          end

    fun ldFlags (spec : TSpec.t) = let
          val extraLibs = #cuda Paths.cxxFlags :: Paths.teemCudaLinkFlags() @ [#base Paths.extraLibs, #cuda Paths.extraLibs]
          val rtLib = TSpec.runtimeLibName spec
          in
            rtLib :: extraLibs
          end

  (* generate defines that control specialization of the code for various features (e.g.,
   * parallelism, global updates, ...
   *)
    fun outputDefines (outS, spec, defs, substitutions) = let
          val ppDecl = Out.decl outS
          fun pp (true, dcl) = ppDecl (CL.D_Verbatim ["#define " ^ dcl])
            | pp _ = ()
          fun cmdLineDef (symb, value) =
                ppDecl (CL.D_Verbatim [concat["#define ", symb, " ", value]])
          val _ = if (#cudaPermute spec andalso #cudaGlobalQueue spec)
                  then raise Fail "Cannot have permutation and GlobalQueueing enabled at the same time"
                  else true
          val _ = if (#cudaBatch spec andalso #cudaGlobalQueue spec)
                  then raise Fail "Cannot have batching and GlobalQueueing enabled at the same time"
                  else true
          val _ = if (#cudaBatch spec andalso #cudaUnified spec)
                  then raise Fail "Cannot have batching and unified memory enabled at the same time"
                  else true
          val cudaDefault = (not (#cudaPermute spec)) andalso (not (#cudaGlobalQueue spec))
          in
            pp (#exec spec andalso #snapshot spec, "DIDEROT_EXEC_SNAPSHOT");
            pp (not(TSpec.noBSP spec), "DIDEROT_BSP");
            pp (#hasStartMeth spec, "DIDEROT_HAS_START_METHOD");
            pp (#hasStabilizeMeth spec, "DIDEROT_HAS_STABILIZE_METHOD");
            pp (TSpec.dualState spec, "DIDEROT_DUAL_STATE");
            pp (TSpec.indirectState spec, "DIDEROT_INDIRECT_STATE");
            pp (#strandConstr spec, "DIDEROT_STRAND_HAS_CONSTR");
            pp (#hasConsts spec, "DIDEROT_HAS_CONSTS");
            pp (not(#hasConsts spec orelse #hasGlobals spec), "DIDEROT_NO_GLOBALS");
            pp (not(#hasInputs spec), "DIDEROT_NO_INPUTS");
            pp (#hasDie spec orelse #hasKillAll spec, "DIDEROT_HAS_STRAND_DIE");
            pp (#hasNew spec, "DIDEROT_HAS_STRAND_NEW");
            pp (#isGrid spec, "DIDEROT_STRAND_ARRAY");
            pp (#hasCom spec, "DIDEROT_HAS_STRAND_COMMUNICATION");
            pp (not(#useKDTree spec), "DIDEROT_NO_SPACE_PARTITION");
            pp (#hasGlobalStart spec, "DIDEROT_HAS_GLOBAL_START");
            pp (#hasGlobalUpdate spec, "DIDEROT_HAS_GLOBAL_UPDATE");
            pp (TSpec.killAll spec, "DIDEROT_HAS_KILL_ALL");
            pp (#hasStabilizeAll spec, "DIDEROT_HAS_STABILIZE_ALL");
            pp (#hasReduce spec, "DIDEROT_HAS_MAPREDUCE");
            pp (#runtimeLog spec, "DIDEROT_ENABLE_LOGGING");
            pp (cudaDefault, "DIDEROT_CUDA_DEFAULT");
            pp (#cudaPermute spec, "DIDEROT_CUDA_PERMUTATIONS");
            pp (#cudaGlobalQueue spec, "DIDEROT_CUDA_GLOBAL_QUEUE");
            pp (#cudaUnified spec, "DIDEROT_CUDA_UNIFIED");
            pp (#cudaBenchmark spec, "DIDEROT_CUDA_BENCHMARK");
            List.app cmdLineDef (CmdLineConstants.defines defs)
          end

  (* include the appropriate definition of the strand_array type based on the target *)
    fun strandArrayDcl spec = (
          case (TSpec.isParallel spec, TSpec.dualState spec, TSpec.indirectState spec)
           of (_, false, false) => Frags.sarr
           | (_, _, _) => raise Fail "not implemented: seq"
           (*of (true, false, false) => CUDAFragments.parSArrayDir
            | (true, true, false) => CUDAFragments.parSArrayDualDir
            | (true, false, true) => CUDAFragments.parSArrayInd
            | (true, true, true) => CUDAFragments.parSArrayDualInd
            | (_, false, false) => CUDAFragments.seqSArrayDir
            | (_, true, false) => CUDAFragments.seqSArrayDualDir
            | (_, false, true) => CUDAFragments.seqSArrayInd
            | (_, true, true) => CUDAFragments.seqSArrayDualInd*)
          (* end case *))

  (* generate source code that is common to both libraries and standalone executables *)
    fun outputSrc (outS, env, spec, prog, strand, outputs, substitutions, genInputCode, gCpy) = let
          val IR.Program{
                  consts, inputs, globals, funcs, constInit, globInit,
                  create, start, update, ...
                } = prog
          val IR.Strand{name=strandName, ...} = strand
          val dim = Create.arrayDim create
          val ppDecl = Out.decl outS
          val {structDefs, methods} = GenStrand.gen (env, strand)
          in
            List.app ppDecl
              (GenGlobals.gen{env=env, consts=consts, inputs=inputs, globals=globals});
            List.app ppDecl structDefs;
            List.map (List.app ppDecl) gCpy;
            ppDecl (CL.verbatimDcl [strandArrayDcl spec] substitutions);
            ppDecl (GenWorld.genStruct(env, strandName, Option.getOpt(dim, 1)));
            List.app ppDecl (genInputCode());
            List.app (ppDecl o genFunc spec) funcs;
            if #hasConsts spec
              then ppDecl (genInitConsts (env, constInit))
              else ();
            if #hasGlobalInit spec
              then ppDecl (genInitGlobals (env, inputs, globals, globInit))
              else ();
            List.app ppDecl methods;
            List.app ppDecl (GenOutputs.gen (env, dim, outputs));
            ppDecl (CL.verbatimDcl [Frags.worldMethods] substitutions);
            ppDecl (GenWorld.genCreateFun (env, globInit, strand, create));
            List.app ppDecl (runFrag (spec, substitutions));
            Option.app (fn blk => ppDecl (GenGlobalUpdate.gen (env, "start", blk))) start;
            Option.app (fn blk => ppDecl (GenGlobalUpdate.gen (env, "update", blk))) update
          end


    fun exec (spec : TSpec.t, defs, prog) = let
        val IR.Program{
                  consts, inputs, globals, funcs, constInit, globInit,
                  create, start, update, strand, ...
                } = prog
        val baseName = createBaseName spec
        val outS = openCudaOut baseName
        val env = mkEnv spec
        val substitutions = mkSubs (spec, strand, create)
        val gCpy = copy_globals globals inputs consts spec
    (* output to C++ file *)
        val ppDecl = Out.decl outS
        val fragment = Out.fragment substitutions outS
        val {preWorld, postWorld} = GenTysAndOps.gen (env, CollectInfo.collect prog)
        val outputs = OutputUtil.gatherOutputs (spec, prog)
        in
        ppDecl (CL.verbatimDcl [CxxFragments.cxxHead] substitutions);
        outputDefines (outS, spec, defs, substitutions);
        ppDecl (CL.verbatimDcl [CxxFragments.execIncl] substitutions);
        List.app ppDecl preWorld;
        ppDecl (CL.verbatimDcl [CxxFragments.namespaceOpen] substitutions);
        ppDecl (CL.verbatimDcl [CxxFragments.nrrdSaveHelper] substitutions);
        outputSrc (outS, env, spec, prog, strand, outputs, substitutions,
            fn () => (
            postWorld @
            GenInputs.genInputsStruct (env, inputs) @
            GenInputs.genExecInputFuns (env, prog) @
            GenOutputsUtil.genRegisterOutputOpts (env, outputs)),
            [gCpy]);
        ppDecl (CL.verbatimDcl [CxxFragments.namespaceClose] substitutions);
        (* generate main function after closing off the namespace *)
        ppDecl (CL.verbatimDcl [Frags.exitWithError] substitutions);
        ppDecl (verbFrag (spec, Frags.main, Frags.main, substitutions));
        Out.closeOut outS;
        compile (spec, baseName);
        RunCC.linkExec (baseName, ldFlags spec)
        end

    fun library (spec : TSpec.t, defs, prog)  = let
          val IR.Program{
                consts, inputs, globals, funcs, constInit, globInit,
                create, start, update, strand, ...
              } = prog
          val env = mkEnv spec
          val substitutions = mkSubs (spec, strand, create)
          val gCpy = copy_globals globals inputs consts spec
        (* output to C++ file *)
          val baseName = createBaseName spec
          val outS = openCudaOut baseName
          val ppDecl = Out.decl outS
          val fragment = Out.fragment substitutions outS
        (* gather the outputs *)
          val outputs = OutputUtil.gatherOutputs (spec, prog)
          val {preWorld, postWorld} = GenTysAndOps.gen (env, CollectInfo.collect prog)
          in
            if not (TSpec.isDebugger spec)
              then (* generate the library .h file *)
                GenLibraryInterface.gen {
                    subs = substitutions,
                    env = env,
                    rt = NONE, (* ?? *)
                    inputs = inputs,
                    outputs = outputs
                  }
              else ();
          (* generate the optional JSON description of the library API *)
            if (#jsonAPI spec)
              then GenLibraryJSON.gen {
                  subs = substitutions,
                  env = env,
                  rt = NONE, (* ?? *)
                  strand = strand,
                  inputs = inputs,
                  outputs = outputs
                }
              else ();
            ppDecl (CL.verbatimDcl [CxxFragments.cxxHead] substitutions);
            outputDefines (outS, spec, defs, substitutions);
            if (TSpec.isDebugger spec)
              then ppDecl (CL.verbatimDcl [CxxFragments.debugIncl] substitutions)
              else ppDecl (CL.verbatimDcl [CxxFragments.libCXXIncl] substitutions);
            List.app ppDecl preWorld;
            ppDecl (CL.verbatimDcl [CxxFragments.namespaceOpen] substitutions);
            ppDecl (CL.verbatimDcl [CxxFragments.nrrdSaveHelper] substitutions);
            List.app ppDecl (GenInputs.genDefinedInpStruct inputs);
            outputSrc (outS, env, spec, prog, strand, outputs, substitutions,
              fn () => (postWorld @ GenInputs.genLibraryInputFuns (env, prog)),[gCpy]);
            if (TSpec.isDebugger spec)
              then List.app ppDecl (GenDebuggerHooks.gen (env, prog))
              else ();
            ppDecl (CL.verbatimDcl [CxxFragments.namespaceClose] substitutions);
            ppDecl (CL.verbatimDcl [Frags.cWrappers] substitutions);
            Out.closeOut outS;
          (* compile and link *)
            compile (spec, baseName);
            RunCC.linkLib (#staticLib spec, baseName, ldFlags spec)
          end
end

