(* gen-outputs.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Generate strand output functions.  The output formats always have a single axis for the
 * data elements followed by one, or more, axes for the output structure.  There are four
 * cases that we handle:
 *
 *      grid, fixed-size elements:
 *              nrrd has object axis followed by grid axes
 *
 *      collection, fixed-size elements
 *              nrrd has object axis followed by a single axis
 *
 *      grid, dynamic-size elements
 *              nLengths nrrd has size 2 for objects (offset, length) followed by grid axes
 *              nData nrrd has object axis followed by a single axis
 *
 *      collection, dynamic-size elements
 *              nLengths nrrd has size 2 for objects (offset, length) followed by a single axis
 *              nData nrrd has object axis followed by a single axis
 *
 * The object axis kind depends on the output type, but it will either be one of the tensor types
 * that Teem knows about or else nrrdKindList.  In any case, the data elements are written as a
 * flat vector following the in-memory layout.  The other axes in the file will have nrrdKindSpace
 * as their kind.
 *
 * TODO: some of this code will be common across all targets (e.g., writing outputs to files), so
 * we will want to refactor it.
 *
 * TODO: for sequences of tensors (e.g., tensor[3][2]), we should use a separate axis for the
 * sequence dimension with kind nrrdKindList.
 *
 * TODO: since the runtime tracks numbers of strands in various states, we should be
 * able to use that information directly from the world without having to recompute it!
 *)

structure GenOutputs : sig

  (* gen (props, nAxes, outputs)
   *    returns a list of function declarations for getting the output/snapshot nrrds from
   *    the program state.  The arguments are:
   *        props       - the target information
   *        nAxes       - the number of axes in the grid of strands (NONE for a collection)
   *        outputs     - the list of output state variables paired with their API types
   *)
    val gen : CodeGenEnv.t * int option * OutputUtil.output_info list -> CLang.decl list

  end = struct

    structure IR = TreeIR
    structure V = TreeVar
    structure Ty = APITypes
    structure CL = CLang
    structure Nrrd = NrrdEnums
    structure U = GenOutputsUtil
    structure RN = CxxNames
    structure Env = CodeGenEnv
    structure GenAPI = GenLibraryInterface

    fun mapi f l = let
          fun mapf (i, [], l) = List.rev l
            | mapf (i, x::xs, l) = mapf (i+1, xs, f(i, x)::l)
          in
            mapf (0, l, [])
          end

    val nrrdPtrTy = CL.T_Ptr(CL.T_Named "Nrrd")
    val sizeTy = CL.T_Named "size_t"
    val wrldPtr = RN.worldPtrTy
    fun mkInt i = CL.mkInt(IntInf.fromInt i)

  (* variables in the generated code *)
    val wrldV = CL.mkVar "wrld"
    val sizesV = CL.mkVar "sizes"
    val iV = CL.mkVar "ix"
    val nV = CL.mkVar "n"
    val cpV = CL.mkVar "cp"
    val ipV = CL.mkVar "ip"
    val msgV = CL.mkVar "msg"
    val offsetV = CL.mkVar "offset"
    val nDataV = CL.mkVar "nData"
    val nLengthsV = CL.mkVar "nLengths"
    val numElemsV = CL.mkVar "numElems"
    val outSV = CL.mkVar "outS"
    val DIDEROT_DEAD = CL.mkVar "diderot::kDead"
    val DIDEROT_STABLE = CL.mkVar "diderot::kStable"
    val NRRD = CL.mkVar "NRRD"

    fun strandMeth (f, args) = CL.mkDispatch(CL.mkIndirect(wrldV, "_strands"), f, args)

  (* dymanic sequence operations *)
    fun seqLength seq = CL.mkDispatch(seq, "length", [])
    fun seqCopy (seq, dst) = CL.mkDispatch(seq, "copy_to", [dst])

  (* utility functions for initializing the sizes array *)
    fun sizes i = CL.mkSubscript(sizesV, mkInt i)
    fun setSizes (i, v) = CL.mkAssign(sizes i, v)

  (* get the number of alive or stable strands strands *)
    fun numStrandsExp snapshot = strandMeth (if snapshot then "num_alive" else "num_stable", [])

  (* code to access state variable
        wrld->outState[i].name
   * or
        wrld->state[i].name
   *)
    fun stateVar spec = let
          fun singleV _ name = CL.mkIndirect(strandMeth("strand", [iV]), "sv_"^name)
          fun dualV U.Shared name = CL.mkIndirect(strandMeth("in_state", [iV]), "sv_"^name)
            | dualV _ name = CL.mkIndirect(strandMeth("local_state", [iV]), "sv_"^name)
          in
            if TargetSpec.dualState spec then dualV else singleV
          end

  (* code fragment to loop over strands
        for (auto ix = wrld->_strands.begin_MODE(), ix != wrld->_strands.end_MODE(), ...) ...
   *
   * where "mode" is either "alive" or "stable".
   *)
    fun forStrands mode stm = CL.mkFor(
          CL.T_Named "auto", [("ix", strandMeth("begin_"^mode, []))],
          CL.mkBinOp(iV, CL.#!=, strandMeth("end_"^mode, [])),
          [CL.mkAssignOp(iV, CL.$=, strandMeth("next_"^mode, [iV]))],
          stm)

  (* code fragment to initialize the axes kinds; the data axis (axis[0]) is given, but we skip it
   * (by convention) if it is scalar. The other axes are the specified domAxisKind.
   *)
    fun initAxisKinds (nrrd, dataAxisKind, nAxes, domAxisKind) = let
        (* nData->axis[0].kind *)
          fun axisKind i = CL.mkSelect(CL.mkSubscript(CL.mkIndirect(nrrd, "axis"), mkInt i), "kind")
          fun init (i, k) = CL.mkAssign (axisKind i, CL.mkVar(Nrrd.kindToEnum k))
          val (firstSpace, dataAxis) = (case dataAxisKind
                 of Nrrd.KindScalar => (0, [])
                  | _ => (1, [init(0, dataAxisKind)])
                (* end case *))
          in
            dataAxis @ List.tabulate(nAxes, fn i => init(i+firstSpace, domAxisKind))
          end

  (* create the body of an output function for dynamic-sequence outputs.  The parameter 'ty'
   * is the element type of the dynamic sequence.  The structure of the
   * function body is:
   *
   *    declarations
   *    compute sizes array for nLengths
   *    allocate nrrd for nLengths
   *    compute sizes array for nData
   *    allocate nrrd for nData
   *    copy data from strands to nrrd
   *)
    fun genDynOutput (env, snapshot, nAxes, ty, name, kind) = let
          val spec = Env.target env
          val (elemCTy, nrrdType, axisKind, nElems) = OutputUtil.infoOf (env, ty)
          val stateVar = stateVar spec kind
          val (nAxes, domAxisKind) = (case nAxes
                 of NONE => (1, Nrrd.KindList)
                  | SOME n => (n, Nrrd.KindSpace)
                (* end case *))
        (* declarations *)
          val sizesDecl = CL.mkDecl(CL.T_Array(sizeTy, SOME(nAxes+1)), "sizes", NONE)
        (* count number of elements (and stable strands) *)
          val countElems = let
                val nElemsInit = CL.mkDeclInit(CL.uint32, "numElems", CL.mkInt 0)
                val cntElems = CL.S_Exp(CL.mkAssignOp(numElemsV, CL.+=, seqLength(stateVar name)))
                val forLoop = forStrands (if snapshot then "alive" else "stable")
                in [
                  CL.mkComment["count number of elements"],
                  nElemsInit,
                  forLoop cntElems
                ] end
        (* generate code to allocate the nLengths nrrd *)
          val lengthsNrrd = let
                val dimSizes = setSizes(0, CL.mkInt 2)  (* nLengths is 2-element vector *)
                in
                  CL.mkComment["allocate nLengths nrrd"] ::
                  (if #isGrid spec
                    then dimSizes ::
                      List.tabulate (nAxes, fn i =>
                        setSizes(i+1, CL.mkSubscript(CL.mkIndirect(wrldV, "_size"), mkInt(nAxes-i-1)))) @
                      [U.maybeAlloc (env, nLengthsV, Nrrd.tyToEnum Nrrd.TypeInt, nAxes+1)]
                    else [
                        dimSizes, setSizes(1, numStrandsExp snapshot),
                        U.maybeAlloc (env, nLengthsV, Nrrd.tyToEnum Nrrd.TypeInt, 2)
                      ])
                end
        (* code to check for no data to output (i.e., all of the output sequences are empty) *)
          val checkForEmpty = [
                  CL.mkComment["check for empty output"],
                  CL.mkIfThen(
                    CL.mkBinOp(mkInt 0, CL.#==, numElemsV),
                    CL.mkBlock[
                        CL.mkCall("nrrdEmpty", [nDataV]),
                        CL.mkReturn(SOME(CL.mkVar "false"))
                      ])
                ]
        (* generate code to allocate the data nrrd *)
          val dataNrrd = if (axisKind = Nrrd.KindScalar)
                then [ (* drop data axis for scalar data by convention *)
                    CL.mkComment["allocate nData nrrd"],
                    setSizes(0, numElemsV),
                    U.maybeAlloc (env, nDataV, Nrrd.tyToEnum nrrdType, 1)
                  ]
                else [
                    CL.mkComment["allocate nData nrrd"],
                    setSizes(0, mkInt nElems),
                    setSizes(1, numElemsV),
                    U.maybeAlloc (env, nDataV, Nrrd.tyToEnum nrrdType, 2)
                  ]
        (* generate the nLengths copy code *)
          val copyLengths = let
                val pInit = CL.mkDeclInit(CL.T_Ptr CL.uint32, "ip",
                      CL.mkReinterpretCast(CL.T_Ptr(CL.uint32), CL.mkIndirect(nLengthsV, "data")))
                val offsetDecl = CL.mkDeclInit(CL.uint32, "offset", CL.mkInt 0)
                val copyBlk = CL.mkBlock[
                        CL.mkDeclInit(CL.uint32, "n", seqLength(stateVar name)),
                        CL.mkAssign(CL.mkUnOp(CL.%*, CL.mkPostOp(ipV, CL.^++)), offsetV),
                        CL.mkAssign(CL.mkUnOp(CL.%*, CL.mkPostOp(ipV, CL.^++)), nV),
                        CL.S_Exp(CL.mkAssignOp(offsetV, CL.+=, nV))
                      ]
                val mode = if #isGrid spec orelse snapshot
                      then "alive"
                      else "stable"
                in
                  CL.mkComment["initialize nLengths nrrd"] ::
                  pInit ::
                  offsetDecl ::
                  forStrands mode copyBlk ::
                  initAxisKinds (nLengthsV, Nrrd.Kind2Vector, nAxes, domAxisKind)
                end
        (* generate the nData copy code *)
          val copyData = let
                val pInit = CL.mkDeclInit(CL.charPtr, "cp",
                      CL.mkReinterpretCast(CL.charPtr, CL.mkIndirect(nDataV, "data")))
                val copyStm = CL.mkAssign(cpV, seqCopy(stateVar name, cpV))
                val mode = if #isGrid spec
                      then "alive"
                      else if #snapshot spec
                        then "alive"
                        else "stable"
                in
                  CL.mkComment["initialize nLengths nrrd"] ::
                  pInit ::
                  forStrands mode copyStm ::
                  initAxisKinds (nDataV, axisKind, 1, Nrrd.KindList)
                end
        (* the function body *)
          val stms =
                sizesDecl ::
                countElems @
                lengthsNrrd @
                checkForEmpty @
                dataNrrd @
                copyLengths @
                copyData @
                [CL.mkReturn(SOME(CL.mkVar "false"))]
          in
            ([CL.PARAM([], nrrdPtrTy, "nLengths"), CL.PARAM([], nrrdPtrTy, "nData")], CL.mkBlock stms)
          end

  (* create the body of an output function for fixed-size outputs.  The structure of the
   * function body is:
   *
   *    declare and compute sizes array
   *    allocate nrrd nData
   *    copy data from strands to nrrd
   *)
    fun genFixedOutput (env, snapshot, nAxes, ty, name, kind) = let
          val spec = Env.target env
          val (elemCTy, nrrdType, axisKind, nElems) = OutputUtil.infoOf (env, ty)
          val stateVar = stateVar spec kind
          val (nAxes, domAxisKind) = (case nAxes
                 of NONE => (1, Nrrd.KindList)
                  | SOME n => (n, Nrrd.KindSpace)
                (* end case *))
          val nDataAxes = if (axisKind = Nrrd.KindScalar) then 0 else 1
        (* generate the sizes initialization code *)
          val initSizes = let
                val dimSizes = let
                      val dcl = CL.mkDecl(CL.T_Array(sizeTy, SOME(nAxes+nDataAxes)), "sizes", NONE)
                      in
                        if (axisKind = Nrrd.KindScalar)
                          then [dcl]
                          else [dcl, setSizes(0, mkInt nElems)]
                      end
                in
                  if #isGrid spec
                    then dimSizes @
                      List.tabulate (nAxes, fn i =>
                        setSizes(i+nDataAxes, CL.mkSubscript(CL.mkIndirect(wrldV, "_size"), mkInt(nAxes-i-1))))
                    else dimSizes @ [setSizes(nDataAxes, numStrandsExp snapshot)]
                end
        (* generate the copy code *)
          val copyCode = let
                val pDecl = CL.mkDeclInit(CL.charPtr, "cp",
                      CL.mkReinterpretCast(CL.charPtr, CL.mkIndirect(nDataV, "data")))
                val copyBlk = CL.mkBlock[
                        CL.mkCall("memcpy", [
                            cpV,
                            CL.mkUnOp(CL.%&, stateVar name),
                            CL.mkBinOp(mkInt nElems, CL.#*, CL.mkSizeof elemCTy)
                          ]),
                        CL.mkExpStm(CL.mkAssignOp(cpV, CL.+=,
                          CL.mkBinOp(mkInt nElems, CL.#*, CL.mkSizeof elemCTy)))
                      ]
                val mode = if #isGrid spec orelse snapshot
                      then "alive"
                      else "stable"
                in
                  pDecl :: forStrands mode copyBlk :: initAxisKinds (nDataV, axisKind, nAxes, domAxisKind)
                end
        (* the function body *)
          val stms =
                CL.mkComment["Compute sizes of nrrd file"] ::
                initSizes @
                CL.mkComment["Allocate nData nrrd"] ::
                U.maybeAlloc (env, nDataV, Nrrd.tyToEnum  nrrdType, nAxes+nDataAxes) ::
                CL.mkComment["copy data to output nrrd"] ::
                copyCode @
                [CL.mkReturn(SOME(CL.mkVar "false"))]
          in
            ([CL.PARAM([], nrrdPtrTy, "nData")], CL.mkBlock stms)
          end

    fun gen (env, nAxes, outputs) = let
          val spec = Env.target env
          val mkFunc = if (#exec spec)
                then (fn (funcName, params, body) => CL.D_Func(
                        [], CL.boolTy, [], funcName,
                        CL.PARAM([], RN.worldPtrTy, "wrld")::params,
                        body))
                else let
                  val wrldParam = CL.PARAM([], GenAPI.worldTy spec, "cWrld")
                  val wrldCastStm = CL.mkDeclInit(RN.worldPtrTy, "wrld",
                        CL.mkReinterpretCast(RN.worldPtrTy, CL.mkVar "cWrld"))
                  in
                    fn (funcName, params, body) => CL.D_Func(
                        ["extern \"C\""], CL.boolTy, [], funcName,
                        wrldParam :: params,
                        CL.prependStm(wrldCastStm, body))
                  end
          fun getFn snapshot {name, ty, kind} = let
                val funcName = if snapshot
                      then GenAPI.snapshotGet(spec, name)
                      else GenAPI.outputGet(spec, name)
                val (params, body) = (case ty
                       of Ty.SeqTy(ty', NONE) =>
                            genDynOutput(env, snapshot, nAxes, ty', name, kind)
                        | _ => genFixedOutput(env, snapshot, nAxes, ty, name, kind)
                      (* end case *))
                in
                  mkFunc (funcName, params, body)
                end
          val getFns = List.map (getFn false) outputs
          val getFns = if (#snapshot spec)
                then List.map (getFn true) outputs @ getFns
                else getFns
          in
            if (#exec spec)
              then getFns @ U.genOutput(env, outputs)
              else getFns
          end

  end
