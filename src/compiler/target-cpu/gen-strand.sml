(* gen-strand.sml
 *
 * Code generation for strands.  If the program does not have strand communication,
 * then we represent a strand S's state using a single struct type (S_strand).
 * If there is communication, then define three struct types:
 *
 *    S_shared, which contains those state variables that are both varying and shared
 *
 *    S_local, which contains the state variables not in S_shared, and
 *
 *    S_strand, which is a wrapper that contains one instance of S_local and two
 *    of S_shared.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenStrand : sig

    val gen : CodeGenEnv.t * TreeIR.strand -> {
            structDefs : CLang.decl list,       (* representation of a strand's state *)
            methods : CLang.decl list           (* implementation of a strand's methods *)
          }

  (* utility function for collecting together the extra parameters needed for a strand
   * method.
   *)
    val methodParams : {
            world : 'a -> 'b,           (* make the world parameter *)
            wcache : 'a -> 'b,          (* make the worker_cache parameter *)
            globals : 'a -> 'b          (* make the globals parameter *)
          } -> TargetSpec.t -> {
            cxt : 'a,                   (* context information used to make parameters *)
            needWorker : bool,          (* true if the method needs the worker_cache *)
            needsW : bool,              (* true if the method needs the world pointer *)
            hasG : bool                 (* true if the method refers to globals *)
          } -> 'b list

  end = struct

    structure IR = TreeIR
    structure SV = TreeStateVar
    structure CL = CLang
    structure RN = CxxNames
    structure Env = CodeGenEnv
    structure TSpec = TargetSpec

    val strandStatusTy = CL.T_Named "diderot::strand_status"

    fun methodParams {world, wcache, globals} spec {cxt, needWorker, needsW, hasG} = let
          val params = if hasG then [globals cxt] else []
          val params = if needWorker andalso TSpec.isParallel spec andalso TSpec.indirectState spec
                      then wcache cxt :: params
                      else params
          val params = if needsW then world cxt :: params else params
          in
            params
          end

  (* generate the `pos` method for the strand state *)
    fun genPosMethod (env, substruct, posV) = let
          val realTy = RawTypes.toString(Env.rawRealTy env)
          fun mkDcl s = CL.D_Verbatim[concat s]
          val (addrof, select) = (case SV.ty posV
                 of TreeTypes.VecTy(1, 1) => ("&", "sv_pos")
                  | TreeTypes.TensorTy[d] => ("", "sv_pos.base()")
                (* end case *))
          in
(* FIXME: once CLang supports "const" functions, we should switch to building AST *)
            mkDcl [
                "const ", realTy, " *pos (uint32_t inIdx) const { return ",
                addrof, "this->", substruct, select, "; }"
              ]
          end

  (* is a state variable the special "pos" variable? *)
    fun isPosVar x = (SV.name x = "pos")

  (* generate the strand-state struct declaration *)
    fun genStrandStructs (env, strandName, state) = let
          fun mkField sv = CL.D_Var([], TreeToCxx.trType(env, SV.ty sv), [], SV.qname sv, NONE)
          fun mkStruct (name, svs, meths) = CL.D_ClassDef{
                  name = name,
                  args = NONE, from = NONE,
                  public = List.foldr (fn (sv, membs) => mkField sv :: membs) meths svs,
                  protected = [],
                  private = []
                }
          val structName = RN.strandTyName strandName
          val dualState = TargetSpec.dualState(Env.target env)
          fun mkPosMeth () = if #hasCom(Env.target env)
                then (case List.find isPosVar state
                   of SOME sv => if not dualState
                          then [genPosMethod (env, "", sv)]
                        else if SV.inSharedStruct sv
                          then [genPosMethod (env, "_shared[inIdx].", sv)]
                          else [genPosMethod (env, "_local.", sv)]
                    | NONE => raise Fail "impossible: missing \"pos\" state variable"
                  (* end case *))
                else []
          in
            if dualState
              then let
(* QUESTION: should we special-case the situations where either sharedSVs or localSVs is empty? *)
                val (sharedSVs, localSVs) = List.partition SV.inSharedStruct state
                val localName = RN.localTyName strandName
                val localStruct = mkStruct (localName, localSVs, [])
                val sharedName = RN.sharedTyName strandName
                val sharedStruct = mkStruct (sharedName, sharedSVs, [])
                val strandStruct = CL.D_ClassDef{
                    name = structName,
                    args = NONE, from = NONE,
                    public =
                        CL.D_Var([], CL.T_Named localName, [], "_local", NONE) ::
                        CL.D_Var([], CL.T_Array(CL.T_Named sharedName, SOME 2), [], "_shared", NONE) ::
                        mkPosMeth(),
                    protected = [],
                    private = []
                  }

                in
                  [localStruct, sharedStruct, strandStruct]
                end
              else [mkStruct (structName, state, mkPosMeth())]
          end

    fun mkParams {spec, needWorker, needsW, hasG} = methodParams {
            world = fn _ => RN.worldParam,
            wcache = fn _ => RN.workerParam,
            globals = fn _ => RN.globalsParam
          } spec {
            cxt = (), needWorker = needWorker, needsW = needsW, hasG = hasG
          }

  (* generate the strand-state initialization code.  The variables are the strand
   * parameters.
   * NOTE: the calling convention generated here must match that used in GenWorld.genCreateFun
   * and genNew below.
   *)
    fun genStateInit (env, strandName, stateParams, params, def) = let
          val IR.Method{needsW, hasG, body} = def
          val fName = strandName ^ "_init"
          val (env, params) = List.foldr
                (fn (x, (env, ps)) => let
                    val (env, p) = TreeToCxx.trParam(env, x)
                    in
                      (env, p::ps)
                    end)
                  (env, []) params
          val params = mkParams {
                  spec = Env.target env, needWorker = false, needsW = needsW, hasG = hasG
                } @ stateParams @ params
          val body = TreeToCxx.trBlock (env, body)
          in
            CL.D_Func(["static"], CL.voidTy, [], fName, params, body)
          end

  (* generate the function for dynamically creating new strands (if needed) *)
    fun genNew (env, strandName, strandPtrTy, params, initDef) = if #hasNew(Env.target env)
          then let
            val spec = Env.target env
            val IR.Method{needsW, hasG, ...} = initDef
            val fName = strandName ^ "_new"
            val params' = RN.worldParam :: List.map
                  (fn x => CL.PARAM([], TreeToCxx.trType(env, TreeVar.ty x), TreeVar.name x))
                    params
          (* parallel targets with indirect strands need the worker cache to implement new/die *)
            val params' = if TargetSpec.isParallel spec andalso TargetSpec.indirectState spec
                    then RN.workerParam :: params'
                    else params'
          (* access to strand states *)
            val strandsDispatch = if TargetSpec.isParallel spec
                  then (fn (f, args) => CL.mkIndirectDispatch(CL.mkVar RN.workerVar, f, args))
                  else let val strands = RN.strandArray env
                    in
                      fn (f, args) => CL.mkDispatch(strands, f, args)
                    end
          (* allocate new strand *)
            val allocStm = CL.mkDeclInit(CL.autoTy, "ix",
                  strandsDispatch("new_strand", []))
          (* arguments to init function *)
            val args = List.map (fn x => CL.mkVar(TreeVar.name x)) params
          (* extend args with strand *)
            val args = if TargetSpec.dualState spec
                  then strandsDispatch("new_local_state", [CL.mkVar "ix"])
                    :: strandsDispatch("new_out_state", [CL.mkVar "ix"])
                    :: args
                  else strandsDispatch("new_strand_state", [CL.mkVar "ix"])
                    :: args
          (* extend args globals (if necessary) *)
            val args = if hasG
                then CL.mkIndirect(CL.mkVar RN.worldVar, "_globals") :: args
                else args
          (* extend args and params with the world (if necessary) *)
            val args = if needsW
                  then CL.mkVar RN.worldVar :: args
                  else args
          (* initialize new strand *)
            val initStm = CL.mkCall(strandName ^ "_init", args)
            in [
              CL.D_Func(["static"], CL.voidTy, [], fName, params', CL.mkBlock [allocStm, initStm])
            ] end
          else []

  (* generate a function definition for a strand method *)
    fun genMethodDef (env, strandName, stateParams, methTy, methName, needsWorker, needsWrld, usesGlobs, body) =
          let
          val spec = Env.target env
          val fName = concat[strandName, "_", methName]
          val params = mkParams {
                  spec = Env.target env, needWorker = needsWorker, needsW = needsWrld, hasG = usesGlobs
                } @ stateParams
          in
            CL.D_Func(["static"], methTy, [], fName, params, body)
          end

  (* generate a function definition for a strand method *)
    fun genMethod (env, strandName, stateParams, methTy, methName, needsWorker, IR.Method{needsW, hasG, body}) =
          genMethodDef (
            env, strandName, stateParams, methTy, methName,
            needsWorker, needsW, hasG, TreeToCxx.trBlock (env, body))

    fun genStartMethod (_, _, _, NONE) = []
      | genStartMethod (env, strandName, stateParams, SOME meth) =
          [genMethod (env, strandName, stateParams, strandStatusTy, "start", true, meth)]

    fun genUpdateMethod (env, strandName, stateParams, meth) =
          genMethod (env, strandName, stateParams, strandStatusTy, "update", true, meth)

    fun genStabilizeMethod (_, _, _, NONE) = []
      | genStabilizeMethod (env, strandName, stateParams, SOME meth) =
          [genMethod (env, strandName, stateParams, CL.voidTy, "stabilize", false, meth)]

    fun gen (env, strand) = let
          val IR.Strand{name, params, state, stateInit, startM, updateM, stabilizeM, ...} = strand
          val name = Atom.toString name
          val strandPtrTy = CL.T_Ptr(RN.strandTy name)
          val (initParams, updateParams) = if TargetSpec.dualState(Env.target env)
                then let
                  fun mkParam (ty, x) = CL.PARAM([], CL.T_Ptr(CL.T_Named ty), x)
                  val localParam = mkParam(RN.localTyName name, RN.selfLocalVar)
                  val inParam = mkParam(RN.sharedTyName name, RN.selfInVar)
                  val outParam = mkParam(RN.sharedTyName name, RN.selfOutVar)
                  in
                    ([localParam, outParam], [localParam, inParam, outParam])
                  end
                else let
                  val params = [CL.PARAM([], CL.T_Ptr(RN.strandTy name), RN.selfVar)]
                  in
                    (params, params)
                  end
          in {
            structDefs = genStrandStructs (env, name, state),
            methods = genStateInit (env, name, initParams, params, stateInit) ::
              genNew (env, name, strandPtrTy, params, stateInit) @
              genStartMethod (env, name, updateParams, startM) @
              genUpdateMethod (env, name, updateParams, updateM) ::
              genStabilizeMethod (env, name, updateParams, stabilizeM)
          } end

  end
