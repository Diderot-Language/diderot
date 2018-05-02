(* target-spec.sml
 *
 * Properties, including the target description, that affect code generation.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TargetSpec =
  struct

    datatype platform = datatype TargetOptions.platform

    type t = {
      (* properties from the command line *)
        diderotc : string,              (* the diderotc command used to invoke this compiler *)
        argv : string list,             (* command-line arguments used to invoke this compiler *)
        version : string,               (* version message string for this compiler *)
      (* properties from the target options *)
        srcFile : string,               (* source filename *)
        outDir : string,                (* directory to put output *)
        outBase : string,               (* basename for output or name of executable *)
        exec : bool,                    (* generate standalone executable? *)
        staticLib : bool,               (* true if exec is false and the generated library *)
                                        (* should be statically linked *)
        jsonAPI : bool,                 (* generate a JSON description of library API? *)
        snapshot : bool,                (* generate functions to get snapshots? *)
        platform : platform,            (* target platform *)
        namespace : string,             (* optional namespace prefix *)
        double : bool,                  (* true for double-precision candidates *)
        longint : bool,                 (* true for 64-bit integers (not currently used) *)
        scalar : bool,                  (* true if scalar code (not SSE) should be generated *)
        debug : bool,                   (* true if debugging of the runtime/generated code is
                                         * enabled.  Note that this does not mean the Diderot
                                         * debugger!! That is specified as a target "platform"
                                         *)
        runtimeLog : bool,              (* true if runtime event logging is enabled *)
        bsp : bool,                     (* true if BSP style execution should always be used *)
        useKDTree : bool,               (* true if a kdtree is used to accelerate spatial queries *)
      (* properties from analysing the program *)
        spatialDim : int option,        (* the spatial dimension used for queries *)
        hasConsts : bool,               (* true if the program has const variables *)
        hasGlobals : bool,              (* true if the program has global variables; not including
                                         * residual constants.
                                         *)
        hasInputs : bool,               (* true if the program has input variables *)
        hasGlobalInit : bool,           (* true if the program has global initialization code *)
        isGrid : bool,                  (* true for programs that use a fixed grid of strands *)
        hasDynSeq : bool,               (* true for programs that have create dynamic sequences;
                                         * this does not include programs that just load sequences
                                         * from nrrd files. *)
        hasStartMeth : bool,            (* true for programs with user-defined start methods *)
        hasStabilizeMeth : bool,        (* true for programs with user-defined stabilize methods *)
        hasDie : bool,                  (* true for programs that have "die" *)
        hasNew : bool,                  (* true for programs that have "new" *)
        hasCom : bool,                  (* true for programs that have strand communication *)
        dualState : bool,               (* trur for programs with shared varying state *)
        strandConstr : bool,            (* true for programs where the strand state has dynamic
                                         * sequences in its state (and so needs a constructor) *)
        hasGlobalStart : bool,          (* true for programs that have a global start block. *)
        hasGlobalUpdate : bool,         (* true for programs that have a global update block *)
        hasKillAll : bool,              (* true for programs that have a global die statement *)
        hasStabilizeAll : bool,         (* true for programs that have a global stabilize statement *)
        hasReduce : bool                (* true for programs that have global reduce *)
      }

    local
      structure P = Properties
    in

  (* create the property record from the target description and program properties *)
    fun mk (tgt : TargetOptions.t, prog) : t = let
          val TreeIR.Program{props, strand=TreeIR.Strand{state, spatialDim, ...}, ...} = prog
          val hasCom = P.hasProp P.StrandCommunication props
          in {
            diderotc = CommandLine.name(),
            argv = CommandLine.arguments(),
            version = Version.message,
            srcFile = #srcFile tgt,
            outDir = #outDir tgt,
            outBase = #outBase tgt,
            exec = #exec tgt,
            staticLib = #staticLib tgt,
            jsonAPI = #jsonAPI tgt,
            snapshot = #snapshot tgt,
            platform = #platform tgt,
            namespace = #namespace tgt,
            double = #double tgt,
            longint = #longint tgt,
            scalar = #scalar tgt,
            debug = #debug tgt,
            runtimeLog = #runtimeLog tgt,
            bsp = #bsp tgt orelse P.hasProp P.NeedsBSP props,
            useKDTree = #kdtree tgt,
            spatialDim = spatialDim,
            hasConsts = P.hasProp P.HasConsts props,
            hasGlobals = P.hasProp P.HasGlobals props,
            hasInputs = P.hasProp P.HasInputs props,
            hasGlobalInit = P.hasProp P.GlobalInit props,
            isGrid = P.hasProp P.StrandArray props,
            hasDynSeq = P.hasProp P.DynamicSeq props,
            hasStartMeth = P.hasProp P.HasStartMethod props,
            hasStabilizeMeth = P.hasProp P.HasStabilizeMethod props,
            hasDie = P.hasProp P.StrandsMayDie props,
            hasNew = P.hasProp P.NewStrands props,
            hasCom = hasCom,
            dualState = hasCom andalso List.exists TreeStateVar.inSharedStruct state,
            strandConstr = true, (* FIXME: analyze strand state to see if constructor is required *)
            hasGlobalStart = P.hasProp P.GlobalStart props,
            hasGlobalUpdate = P.hasProp P.GlobalUpdate props,
            hasKillAll = P.hasProp P.KillAll props,
            hasStabilizeAll = P.hasProp P.StabilizeAll props,
            hasReduce = P.hasProp P.GlobalReduce props
          } end

    end (* local *)

  (* return true if the target is parallel *)
    fun isParallel (p : t) = (case #platform p of PARALLEL => true | _ => false)

  (* return true if the target is the Diderot debugger *)
    fun isDebugger (p : t) = (case #platform p of DEBUGGER => true | _ => false)

  (* returns true if BSP style execution should be used.
   * this property can be set on the command line, but
   * also holds when the program has communication, etc.
   *)
    fun bsp (p : t) = #bsp p orelse #hasNew p orelse #hasCom p orelse #hasGlobalUpdate p

  (* returns false if BSP style execution should always be used;
   * this property can be set on the command line, but
   * also holds when the program has communication, etc.
   *)
    fun noBSP (p : t) = not(bsp p)

  (* returns true if the program does not use inter-strand communication *)
    fun noComm (p : t) = not(#hasCom p)

  (* return true if we need to keep two copies of the state: in and out.
   * NOTE: this function is more precise than Properties.dualState,
   * but that is safe, since Properties.dualState is just used to enable
   * optimizations.
   *)
    fun dualState (p : t) = (#dualState p)

  (* return true if we need to use an indirect representation of the state.  This is
   * the case when the program uses the "new" operation or when it uses the "die"
   * operation and BSP is in effect (if there is no BSP, the there is no need to
   * compact the strand array and thus no need for indirect state access)
   *)
    fun indirectState (p : t) = (#hasNew p) orelse ((#hasDie p) andalso (bsp p))

  (* does the world object have a "kill_all" method.  We need this for programs
   * that use "die" in global update and for standalone executables that generate
   * collections, since kill_all is used when the step limit expires.
   *)
    fun killAll (p : t) = (#hasKillAll p) orelse ((#exec p) andalso not(#isGrid p))

  (* convert a target specification to the name of the runtime library *)
    fun runtimeLibName (spec : t) = let
          val l = if #debug spec
                  then ["-debug.o"]
                else if #runtimeLog spec
                  then ["-log.o"]
                  else [".o"]
          val t = (case #platform spec
                 of SEQUENTIAL => "-seq"
                  | PARALLEL => "-par"
                  | DEBUGGER => "-seq"  (* QUESTION: should there be a special debugger version of the runtime? *)
                  | OPENCL => "-cl"
                  | CUDA => "-cuda"
                (* end case *))
          in
            OS.Path.concat(
              Paths.diderotLib(),
              String.concat("diderot-rt" :: t :: l))
          end

  (* return the CPP floating-point precision definition for the target *)
    fun floatPrecisionDef (p : t) = if #double p
          then "DIDEROT_DOUBLE_PRECISION"
          else "DIDEROT_SINGLE_PRECISION"

  (* return the CPP integer precision definition for the target *)
    fun intPrecisionDef (p : t) = if #longint p
          then "DIDEROT_LONGINT"
          else "DIDEROT_INT"

  (* return the CPP target definition for the target *)
    fun targetDef (p : t) = "DIDEROT_TARGET_" ^ TargetOptions.platformToString (#platform p)

  (* qualify a C function or type name by the target namespace, but only for libraries *)
    fun qualifyCId name (spec : t) =
          if (#exec spec) then name else concat[#namespace spec, "_", name]
    fun qualifyCId' base (spec : t, name) =
          if (#exec spec)
            then concat[base, "_", name]
            else concat[#namespace spec, "_", base, "_", name]

  end
