(* gen-outputs-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Target-independent code for generating the output code.  For standalone executables,
 * we generate command-line options based on the number of output files and whether
 * snapshots are enabled.  The scheme is as follows:
 *
 * Single output variable with non-sequence type (say "out"):
 *
 *  "-o,--output" option to specify _full path_ to output file (default "out.nrrd")
 *  "-sp,--snapshot-prefix" option to specify _prefix_ of the snapshot files (default "out")
 *
 * Single output variable with sequence type (say "out"):
 *
 *  "-o,--output" option to specify _prefix_ to output file (default "out")
 *  "-sp,--snapshot-prefix" option to specify _prefix_ of the snapshot files (default "out")
 *
 * Multiple output variables.
 *
 *  With non-sequence type (say "foo"):
 *
 *    "-o-foo,--output-foo" option to specify _full path_ to output file (default "foo.nrrd")
 *    "-sp-foo,--snapshot-prefix-foo" option so specify _prefix_ of the snapshot files (default "foo")
 *
 * With sequence type (say "bar"):
 *
 *   "-o-foo,--output-bar" option to specify _prefx_ to output file (default "bar")
 *   "-sp-foo,--snapshot-prefix-bar" option so specify _prefix_ of the snapshot files (default "bar")
 *)

structure GenOutputsUtil : sig

    datatype kind = Global | Shared | Local

    type output_info = OutputUtil.output_info

  (* code fragment to allocate nrrd data and check for errors *)
    val maybeAlloc : CodeGenEnv.t * CLang.exp * CLang.var * int -> CLang.stm

  (* generate code to register command-line options for redirecting the output in standalone
   * executables.  This function returns a list consisting of the global C variables that hold
   * the option values and the registration function.
   *)
    val genRegisterOutputOpts : CodeGenEnv.t * output_info list -> CLang.decl list

  (* generate the nrrd-file output and print functions used by standalone executables *)
    val genOutput : CodeGenEnv.t * output_info list -> CLang.decl list

  end = struct

    structure Ty = APITypes
    structure CL = CLang
    structure Env = CodeGenEnv
    structure RN = CxxNames

    datatype kind = datatype OutputUtil.kind
    type output_info = OutputUtil.output_info

    val nrrdPtrTy = CL.T_Ptr(CL.T_Named "Nrrd")
    val filePtrTy = CL.T_Ptr(CL.T_Named "FILE")

    fun getFn true = GenLibraryInterface.snapshotGet
      | getFn false = GenLibraryInterface.outputGet

  (* variables in the generated code *)
    val wrldV = CL.mkVar "wrld"
    val sizesV = CL.mkVar "sizes"
    val nDataV = CL.mkVar "nData"
    val nLengthsV = CL.mkVar "nLengths"
    val NRRD = CL.mkVar "NRRD"
    val msgV = CL.mkVar "msg"

  (* code fragment to allocate nrrd data and check for errors
        if (nrrdMaybeAlloc_nva(<nrrdVar>, <nrrdType>, <nDims>, sizes) != 0) {
            char *msg = biffGetDone(NRRD);
            biffMsgAdd (wrld->_errors, msg);
            std::free (msg);
            return true;
        }
   *)
    fun maybeAlloc (env, nrrdVar, nrrdType, nDims) =
          CL.mkIfThen(
            CL.mkBinOp(
              CL.mkApply("nrrdMaybeAlloc_nva", [
                  nrrdVar, CL.mkVar nrrdType, CL.mkInt(IntInf.fromInt nDims), sizesV
                ]),
              CL.#!=,
              CL.mkInt 0),
          (* then *)
            CL.mkBlock[
                CL.mkDeclInit(CL.charPtr, "msg", CL.mkApply("biffGetDone", [NRRD])),
                TreeToCxx.errorMsgAdd (env, msgV),
                CL.mkCall("std::free", [msgV]),
                CL.mkReturn(SOME(CL.mkVar "true"))
              ]
          (* endif*))

  (* global variable names for output file/stems *)
    val outfile = "OutputFile"
    fun outstem (false, _) = outfile (* single output variable *)
      | outstem (true, name) = "OutPrefix_" ^ name
    val snapfile = "SnapshotPrefix"
    fun snapstem (false, name) = snapfile (* single output variable *)
      | snapstem (true, name) = "SnapshotPrefix_" ^ name

  (* register a command-line option *)
    fun addOption {name, desc, var} =
          CL.mkExpStm(CL.mkIndirectDispatch(CL.mkVar "opts", "add", [
              CL.mkStr name,
              CL.mkStr desc,
              CL.mkUnOp(CL.%&, CL.mkVar var),
              CL.mkBool true
            ]))

  (* generate code to register command-line options for redirecting the output in standalone
   * executables.
   *)
    fun genRegisterOutputOpts (env, outputs : output_info list) = let
          val hasSnapshots = #snapshot(Env.target env)
        (* make a global variable declaration *)
          fun mkDecl (name, value) =
                CL.D_Var(["static"], CL.T_Named "std::string", [], name,
                  SOME(CL.I_Exp(CL.mkStr value)))
        (* register options for an output.  multiVar should be true if there
         * is more than one output.
         *)
          fun registerOut multiVar = let
                fun doOutput ({name, ty, kind}, (stms, dcls)) = let
                      val hasSeqTy = (case ty of Ty.SeqTy(_, NONE) => true | _ => false)
                      val stms = if hasSnapshots
                            then addOption {
                                name = if multiVar
                                  then concat["sp-", name, ",", "snapshot-prefix-", name]
                                  else "sp,snapshot-prefix",
                                desc = if multiVar
                                  then "specify snapshot-file prefix for " ^ name
                                  else "specify snapshot-file prefix",
                                var = snapstem (multiVar, name)
                              } :: stms
                            else stms
                      val stms = addOption {
                              name = if multiVar
                                then concat["o-", name, ",", "output-", name]
                                else "o,output",
                              desc = (case (multiVar, hasSeqTy)
                                 of (true, true) => "specify output-file prefix for " ^ name
                                  | (true, false) => "specify output file for " ^ name
                                  | (false, true) => "specify output-file prefix"
                                  | (false, false) => "specify output file"
                                (* end case *)),
                              var = outstem (multiVar, name)
                            } :: stms
                      val dcls = if hasSnapshots
                            then mkDecl (snapstem(multiVar, name), name) :: dcls
                            else dcls
                      val dcls = mkDecl (
                            outstem (multiVar, name),
                            if hasSeqTy then name else name ^ ".nrrd") :: dcls
                      in
                        (stms, dcls)
                      end
                in
                  doOutput
                end
          val (stms, dcls) =
                List.foldr
                  (registerOut (case outputs of _::_::_ => true | _ => false))
                    ([], []) outputs
          val registerFn = CL.D_Func(
                ["static"], CL.voidTy, [], "register_outputs",
                [CL.PARAM([], RN.optionsPtrTy, "opts")],
                CL.mkBlock stms)
          in
            dcls @ [registerFn]
          end

    fun ++ (a, b) = CL.mkBinOp (a, CL.#+, b)
    infix ++

  (* generate the nrrd-file output and snapshot functions used by standalone executables *)
    fun genOutput' (env, snapshot, outputs : output_info list) = let
          val spec = Env.target env
          val outputGet = getFn snapshot
          fun isDyn ty = (case ty of Ty.SeqTy(_, NONE) => true | _ => false)
          fun error NONE = CL.mkReturn(SOME(CL.mkVar "true"))
            | error (SOME msg) = CL.mkBlock[
                  CL.mkExpStm(CL.mkIndirectDispatch(wrldV, "error", [msg])),
                  CL.mkReturn(SOME(CL.mkVar "true"))
                ]
          val outDecls = if List.exists (isDyn o #ty) outputs
                then [CL.mkDecl(nrrdPtrTy, "nLengths", NONE), CL.mkDecl(nrrdPtrTy, "nData", NONE)]
                else [CL.mkDecl(nrrdPtrTy, "nData", NONE)]
          val prDecls = outDecls @ [CL.mkDecl(filePtrTy, "outS", NONE)]
          fun nrrdNew v = CL.mkAssign(v, CL.mkApply("nrrdNew", []))
          fun nrrdNuke v = CL.mkCall("nrrdNuke", [v])
          val isMultiOutput = (case outputs of _::_::_ => true | _ => false)
        (* make the name of an output file (to be passed to nrrd_save_helper) from
         * a variable name.
         *)
          fun mkOutFile name = if snapshot
                then CL.mkVar (snapstem (isMultiOutput, name))
                  ++ CL.mkVar "suffix"
                  ++ CL.mkStr ".nrrd"
                else CL.mkVar (outstem (isMultiOutput, name))
        (* make the dynseq_save_helper argument list from a variable name *)
          fun mkDynseqHelperArgs name = if snapshot
                then [
                    CL.mkVar (snapstem (isMultiOutput, name)), CL.mkVar "suffix",
                    nLengthsV, nDataV
                  ]
                else [CL.mkVar (outstem (isMultiOutput, name)), nLengthsV, nDataV]
          fun writeNrrd {name, ty, kind} = if isDyn ty
                then [
                    nrrdNew (nLengthsV),
                    nrrdNew (nDataV),
                    CL.mkIfThenElse(
                      CL.mkApply(outputGet(spec, name), [wrldV, nLengthsV, nDataV]),
                    (* then *)
                        error (SOME(CL.mkStr(concat[
                            "Error getting nrrd data for '", name, "'"
                          ]))),
                    (* else *)
                      CL.mkIfThen(
                        CL.mkApply("dynseq_save_helper", mkDynseqHelperArgs name),
                      (* then *)
                        error NONE
                      (* endif *))
                    (* endif *)),
                    nrrdNuke nLengthsV,
                    nrrdNuke nDataV
                  ]
                else [
                    nrrdNew (nDataV),
                    CL.mkIfThenElse(
                      CL.mkApply(outputGet(spec, name), [wrldV, nDataV]),
                    (* then *)
                        error (SOME(CL.mkStr(concat[
                            "Error getting nrrd data for '", name, "'"
                          ]))),
                    (* else *)
                      CL.mkIfThen(
                        CL.mkApply("nrrd_save_helper", [mkOutFile name, nDataV]),
                      (* then *)
                        error NONE
                      (* endif *))
                    (* endif *)),
                    nrrdNuke nDataV
                  ]
          val name = if snapshot then "write_snapshot" else "write_output"
          val params = if snapshot
                then [CL.PARAM([], CL.T_Named "std::string const &", "suffix")]
                else []
          val params = RN.worldParam :: params
          in
            CL.D_Func(
              ["static"], CL.boolTy, [], name, params,
              CL.mkBlock(
		outDecls @
		List.foldr (fn (output, l) => writeNrrd output @ l)
		  [CL.mkReturn(SOME(CL.mkVar "false"))] outputs))
          end

  (* generate the nrrd-file output and snapshot functions used by standalone executables *)
    fun genOutput (env, outputs : output_info list) = let
          val dcls = [genOutput' (env, false, outputs)]
          val dcls = if #snapshot(Env.target env)
                then genOutput' (env, true, outputs) :: dcls
                else dcls
          in
            dcls
          end

  end
