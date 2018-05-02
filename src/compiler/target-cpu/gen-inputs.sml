(* gen-inputs.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Generate code to handle input variables for the C target.
 *)

structure GenInputs : sig

  (* input variable descriptor: type, name, description, and default *)
    type input_desc = GenInputsUtil.input_desc

  (*** Support for standalone executables ***)

  (* generate the input initialization structure that we use to initialize input
   * globals from command-line arguments in stand-alone executables.
   *)
    val genInputsStruct : CodeGenEnv.t * input_desc list -> CLang.decl list

  (* generate the functions that handle inputs for standalone executables.  These are:
   *    init_defaults    -- called to initialize the default input values
   *    register_inputs  -- called to register the command-line options for the input globals
   *    init_inputs      -- called to initialize the input globals from the values specified
   *                        on the command line.
   *)
    val genExecInputFuns : CodeGenEnv.t * TreeIR.program -> CLang.decl list

  (*** Support for libraries ***)

  (* generate the typedef for the defined-input flag struct. *)
    val genDefinedInpStruct : input_desc list -> CLang.decl list

  (* generate functions that handle inputs for libraries.  These are:
   *    init_defined_inputs -- function that initializes the defined-input flag struct
   *    init_defaults       -- function to initialize the default input values
   * plus the exported functions to specify inputs for the library API.
   *)
    val genLibraryInputFuns : CodeGenEnv.t * TreeIR.program -> CLang.decl list

  end = struct

    structure IR = TreeIR
    structure Ty = APITypes
    structure GVar = TreeGlobalVar
    structure CL = CLang
    structure RN = CxxNames
    structure ToC = TreeToCxx
    structure U = GenInputsUtil
    structure Env = CodeGenEnv
    structure GenAPI = GenLibraryInterface
    structure Inp = Inputs

    type input_desc = U.input_desc

    val genInputsStruct = U.genInputsStruct
    val genDefinedInpStruct = U.genDefinedInpStruct

  (* translate an API type to the C types used to represent it in the external API *)
    fun trType (env, ty) = (case ty
           of Ty.IntTy => Env.intTy env
            | Ty.BoolTy => Env.boolTy env
            | Ty.TensorTy[] => Env.realTy env
            | Ty.TensorTy dd => CL.T_Array(Env.realTy env, SOME(List.foldl Int.* 1 dd))
            | Ty.StringTy => CL.constPtrTy CL.charTy
            | Ty.ImageTy(dim, _) => CL.T_Ptr(CL.T_Named "nrrd")
            | Ty.SeqTy(ty, NONE) => CL.T_Ptr(CL.T_Named "nrrd")
            | Ty.SeqTy(ty, SOME n) => CL.T_Array(trType(env, ty), SOME n)
          (* end case *))
(* TODO dynamic sequences:
 *
For example, if we have a global

    input tensor[3,3] g[];

the tensor type is represented as float[9] (or double[9]), so we could use something like

    void Diderot_input_get_g (Diderot_world_t *wrld, uint32_t *len, float ( **v )[9]);
    void Diderot_input_set_g (Diderot_world_t *wrld, uint32_t len, float ( *v )[9]);
 *
 *)

    val nrrdPtrTy = CL.T_Ptr(CL.T_Named "Nrrd")

  (* world pointer *)
    val wrldV = CL.mkVar "wrld"

  (* an l-value expression for accessing a global variable *)
    fun global gv = CL.mkIndirect(CL.mkIndirect(CL.mkVar "wrld", "_globals"), GVar.qname gv)

  (* define a function that is exported to C *)
    fun cFunc (ty, name, params, body) = CL.D_Func(["extern \"C\""], ty, [], name, params, body)

  (* generate code to initialize the global input variables from the command-line inputs *)
    fun genInitInputs (env, inputs) = let
        (* the world pointer type *)
          val worldPtrTy = RN.worldPtrTy
        (* the global state pointer type *)
          val globPtrTy = RN.globalPtrTy
        (* some common variables *)
          val inpV = CL.mkVar "inp"
          val optsV = CL.mkVar "opts"
        (* access globals via the "glob" pointer *)
          fun global name = CL.mkIndirect(CL.mkVar RN.globalsVar, GVar.qname name)
        (* initialize a given input global; for sequences and images, this requires
         * loading the value from the specified nrrd file, while for other types
         * we just copy the values.
         *)
          fun initInput (Inp.INP{var, name, ty, desc, init}, stms) = let
                val gv = CL.mkIndirect(inpV, GVar.qname var)
                in
                  case ty
                   of Ty.SeqTy(_, NONE) => ToC.loadNrrd (global var, gv, NONE) :: stms
                    | Ty.ImageTy _ => ToC.loadNrrd (global var, gv, Inp.proxy init) :: stms
                    | ty => U.copy{env=env, ty=ty, dst=global var, src=gv} :: stms
                  (* end case *)
                end
          in
            CL.D_Func(
              ["static"], CL.boolTy, [], "init_inputs",
              [CL.PARAM([], worldPtrTy, "wrld"), CL.PARAM([], RN.inputsPtrTy, "inp")],
              CL.mkBlock(
                CL.mkDeclInit(globPtrTy, RN.globalsVar, CL.mkIndirect(CL.mkVar "wrld", "_globals")) ::
                List.foldr initInput [CL.mkReturn(SOME(CL.mkVar "false"))] inputs))
          end

  (* generate the functions that handle inputs for standalone executables.  These are:
   *    init_defaults    -- called to initialize the default input values
   *    register_inputs  -- called to register the command-line options for the input globals
   *    init_inputs      -- called to initialize the input globals from the values specified
   *                        on the command line.
   *)
    fun genExecInputFuns (env, prog as IR.Program{inputs, ...}) =
          if List.null inputs
            then []
            else U.genExecInputFuns (env, prog) @ [genInitInputs (env, inputs)]

    fun genCheckInputs (env, inputs) = let
        (* the world pointer type *)
          val worldPtrTy = RN.worldPtrTy
          val wrldParam = CL.PARAM([], worldPtrTy, "wrld")
        (* check that the specified input has been defined and, if not, define it to its default *)
          fun check (Inp.INP{var, name, ty, init, ...}, stms) = let
                fun undef () = CL.mkBlock[
                        ToC.errorMsgAdd(env, CL.mkStr(concat["undefined input \"", name, "\"\n"])),
                        CL.mkReturn(SOME(CL.mkBool true))
                      ]
                fun mkIfStm stm = CL.mkIfThen(CL.mkUnOp(CL.%!, U.defined var), stm) :: stms
                in
                  case init
                   of Inp.NoDefault => mkIfStm(undef ())
                    | Inp.ConstExpr => stms (* initialization was handled in constInit *)
                    | Inp.LoadSeq file =>
                        mkIfStm(ToC.loadNrrd (global var, CL.mkStr file, NONE))
                    | Inp.Proxy(file, info) =>
                        mkIfStm(ToC.loadNrrd (global var, CL.mkStr file, SOME info))
                    | Inp.Image _ => mkIfStm(undef ())
                  (* end case *)
                end
          val body = List.foldr check [CL.mkReturn(SOME(CL.mkBool false))] inputs
          in
            CL.D_Func(
              ["static"], CL.boolTy, [], "check_defined",
              [wrldParam],
              CL.mkBlock body)
          end

  (* for each input variable we generate two or three top-level declarations in the
   * exported API.
   *)
    fun genInputFuns (_, [], extras) = extras
      | genInputFuns (env, inputs, extras) = let
          val spec = Env.target env
        (* the C world pointer type *)
          val cWorldPtrTy = GenAPI.worldTy spec
          val wrldParam = CL.PARAM([], cWorldPtrTy, "cWrld")
        (* the C++ world pointer type *)
          val worldPtrTy = RN.worldPtrTy
          val wrldCastStm = CL.mkDeclInit(worldPtrTy, "wrld",
                CL.mkReinterpretCast(worldPtrTy, CL.mkVar "cWrld"))
        (* create decls for an input variable *)
          fun mkInputDecls (Inp.INP{var, name, ty, desc, init}) = let
              (* create a description declaration for the input variable *)
                val descDcl = (case desc
                       of SOME desc => [
                              CL.mkVarInitDcl(CL.T_Ptr(CL.T_Named "const char"),
                                GenAPI.inputDesc(spec, name),
                                CL.I_Exp(CL.mkStr desc))
                            ]
                        | NONE => []
                      (* end case *))
                val getDcl = if (Inp.isDefault init)
                        then let
                          val getName = GenAPI.inputGet(spec, name)
                        (* generate code for a function that returns the current value of
                         * an input global.
                         *)
                          fun mkFunc outTy = cFunc(
                                CL.voidTy, getName,
                                [wrldParam, CL.PARAM([], outTy, "v")],
                                CL.mkBlock[
                                    wrldCastStm,
                                    U.copyToC {
                                        env = env,
                                        ty = ty,
                                        dst = (case outTy
                                             of CL.T_Ptr _ => CL.mkUnOp(CL.%*, CL.mkVar "v")
                                              | CL.T_Array _ => CL.mkVar "v"
                                              | _ => raise Fail "not l-value type"
                                            (* end cade *)),
                                        src = global var
                                      }
                                  ])
(* QUESTION: for images and sequences, it is not clear what the get function should return.
 * For now, we just return 0
 *)
                        (* for images and sequences, we currently return 0 *)
                          fun nrrdFunc () = cFunc(
                                CL.voidTy, getName,
                                [wrldParam, CL.PARAM([], CL.T_Ptr CL.voidPtr, "v")],
                                CL.mkAssign(CL.mkUnOp(CL.%*, CL.mkVar "v"), CL.mkInt 0))
(* FIXME: need to track the file name and allow returning it for debugger support
                                CL.mkBlock[
                                    wrldCastStm,
                                    CL.mkAssign(
                                      CL.mkUnOp(CL.%*, CL.mkVar "v"),
                                      CL.mkDispatch(global var, "c_str", []))
                                  ])
*)
                          val func = (case ty
                                 of Ty.BoolTy => mkFunc(CL.T_Ptr(trType(env, ty)))
                                  | Ty.StringTy => mkFunc(CL.T_Ptr CL.charPtr)
                                  | Ty.IntTy => mkFunc(CL.T_Ptr(trType(env, ty)))
                                  | Ty.TensorTy[] => mkFunc(CL.T_Ptr(trType(env, ty)))
                                  | Ty.TensorTy _ => mkFunc(trType(env, ty))
                                  | Ty.SeqTy(_, SOME _) => mkFunc(trType(env, ty))
                                  | Ty.SeqTy(_, NONE) => nrrdFunc ()
                                  | Ty.ImageTy _ => nrrdFunc ()
                                (* end case *))
                          in
                            [func]
                          end
                        else []
                val setDcl = (case ty
                       of Ty.ImageTy _ => [
                              cFunc(
                                CL.boolTy, GenAPI.inputSetByName(spec, name),
                                [wrldParam, CL.PARAM(["const"], CL.charPtr, "s")],
                                CL.mkBlock[
                                    wrldCastStm,
                                    ToC.loadNrrd (global var, CL.mkVar "s", Inp.proxy init),
                                    CL.mkReturn(SOME(CL.mkBool false))
                                  ]),
                              cFunc(
                                CL.boolTy, GenAPI.inputSet(spec, name),
                                [wrldParam, CL.PARAM([], nrrdPtrTy, "nin")],
                                CL.mkBlock[
                                    wrldCastStm,
                                    ToC.loadNrrd (global var, CL.mkVar "nin", Inp.proxy init),
                                    CL.mkReturn(SOME(CL.mkBool false))
                                  ])
                            ]
                        | Ty.SeqTy(elemTy, NONE) => [
                              cFunc(
                                CL.boolTy, GenAPI.inputSetByName(spec, name),
                                [wrldParam, CL.PARAM(["const"], CL.charPtr, "s")],
                                CL.mkBlock[
                                    wrldCastStm,
                                    CL.mkAssign(U.defined var, CL.mkBool true),
                                    ToC.loadNrrd (global var, CL.mkVar "s", NONE),
                                    CL.mkReturn(SOME(CL.mkBool false))
                                  ]),
                              cFunc(
                                CL.boolTy, GenAPI.inputSet(spec, name),
                                [wrldParam, CL.PARAM([], nrrdPtrTy, "nin")],
                                CL.mkBlock[
                                    wrldCastStm,
                                    CL.mkAssign(U.defined var, CL.mkBool true),
                                    ToC.loadNrrd (global var, CL.mkVar "nin", NONE),
                                    CL.mkReturn(SOME(CL.mkBool false))
                                  ])
                            ]
                        | _ => [
                              cFunc(
                                CL.boolTy, GenAPI.inputSet(spec, name),
                                [wrldParam, CL.PARAM([], trType(env, ty), "v")],
                                CL.mkBlock(
                                  wrldCastStm ::
                                  CL.mkAssign(U.defined var, CL.mkBool true) ::
                                  U.copyToCxx {env=env, ty=ty, dst=global var, src=CL.mkVar "v"} ::
                                    [CL.mkReturn(SOME(CL.mkVar "false"))]))
                            ]
                      (* end case *))
                in
                  (descDcl @ getDcl @ setDcl)
                end
          val extras = genCheckInputs (env, inputs) :: extras
          in
            List.foldr (fn (input, dcls) => mkInputDecls input @ dcls) extras inputs
          end

    fun genLibraryInputFuns (env, prog as IR.Program{inputs, ...}) =
          genInputFuns (env, inputs, U.genLibraryInputFuns(env, prog))

  end
