(* gen-inputs-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure GenInputsUtil : sig

    type input_desc = TreeGlobalVar.t Inputs.input

  (*** Support for standalone executables ***)

  (* generate the input initialization structure that we use to initialize input
   * globals from command-line arguments in stand-alone executables.
   *)
    val genInputsStruct : CodeGenEnv.t * input_desc list -> CLang.decl list

  (* generate the common functions that handle inputs for standalone executables.  These are:
   *    init_defaults    -- called to initialize the default input values
   *    register_inputs  -- called to register the command-line options for the input globals
   *)
    val genExecInputFuns : CodeGenEnv.t * TreeIR.program -> CLang.decl list

  (*** Support for libraries ***)

  (* generate the typedef for the defined-input flag struct. *)
    val genDefinedInpStruct : input_desc list -> CLang.decl list

  (* generate the function that initializes the defined-input flag struct. *)
    val genDefineInp : input_desc list -> CLang.decl

  (* generate the common functions that handle inputs for libraries.  These are:
   *    init_defined_inputs -- function that initializes the defined-input flag struct
   *    init_defaults       -- fucntion to initialize the default input values
   *)
    val genLibraryInputFuns : CodeGenEnv.t * TreeIR.program -> CLang.decl list

  (*** Utility functions ***)

    val castAPIPtr : {
            env : CodeGenEnv.t, ty : APITypes.t, src : CLang.exp
          } -> CLang.exp

  (* generate code to copy from a C++ global variable to a C reference *)
    val copyToC : {
            env : CodeGenEnv.t, ty : APITypes.t, src : CLang.exp, dst : CLang.exp
          } -> CLang.stm

  (* generate code to copy from a C input argument to the C++ global *)
    val copyToCxx : {
            env : CodeGenEnv.t, ty : APITypes.t, src : CLang.exp, dst : CLang.exp
          } -> CLang.stm

  (* generate code to copy a value from one variable to another *)
    val copy : {
            env : CodeGenEnv.t, ty : APITypes.t, src : CLang.exp, dst : CLang.exp
          } -> CLang.stm

  (* an l-value expression for accessing a defined-input flag for the given global input *)
    val defined : TreeGlobalVar.t -> CLang.exp

  end = struct

    structure IR = TreeIR
    structure Ty = APITypes
    structure GVar = TreeGlobalVar
    structure CL = CLang
    structure ToC = TreeToCxx
    structure Env = CodeGenEnv
    structure Inp = Inputs
    structure RN = CxxNames

    type input_desc = GVar.t Inp.input

    val worldPtrTy = RN.worldPtrTy
    val globalPtrTy = RN.globalPtrTy

  (* translate the API type of an input to the C++ type used to representation the input
   * in the input struct.  Note that the representation of nrrd inputs (images and
   * dynamic sequences) depends on if we are building a library or stand-alone executable.
   * In the former case, we use a nrrd pointer, whereas we use a string in the latter case.
   *)
    local
      fun nrrdTy env = if #exec(Env.target env)
            then CL.T_Named "std::string"
            else CL.T_Ptr(CL.T_Named "Nrrd")
    in
    fun trType (env, ty) = (case ty
           of Ty.IntTy => Env.intTy env
            | Ty.BoolTy => CL.boolTy
            | Ty.TensorTy[] => Env.realTy env
            | Ty.TensorTy dd => RN.tensorTy dd
            | Ty.StringTy => CL.T_Named "std::string"
            | Ty.ImageTy(dim, shp) => nrrdTy env
            | Ty.SeqTy(ty, NONE) => nrrdTy env
            | Ty.SeqTy(ty, SOME n) => CL.T_Array(trType(env, ty), SOME n)
          (* end case *))
    end (* local *)

  (* generate the input initialization structure that we use to initialize input
   * globals from command-line arguments in stand-alone executables.
   *)
    fun genInputsStruct (_, []) = []
      | genInputsStruct (env : Env.t, inputs) = let
          fun mkField (Inp.INP{var, ty, ...}) = (trType(env, ty), GVar.qname var)
          in
            [CL.D_StructDef(NONE, List.map mkField inputs, SOME RN.inputsTyName)]
          end

  (* generate code to initialize the default input values *)
    fun genInitDefaults (env, inputs, dstTy, dst, IR.Block{locals, body}) = let
        (* initialization for dynamic sequence and image inputs, which are specified by
         * strings.
         *)
          val mkNrrdInit = if #exec(Env.target env)
                then (fn (field, nrrd, stms) => CL.mkAssign(
                  CL.mkIndirect(CL.mkVar dst, field),
                  CL.mkStr nrrd) :: stms)
                else (fn (_, _, stms) => stms)
          fun initDefault (Inp.INP{var, init, ...}, stms) = (case init
                 of Inp.LoadSeq nrrd => mkNrrdInit (GVar.qname var, nrrd, stms)
                  | Inp.Proxy(nrrd, _) => mkNrrdInit (GVar.qname var, nrrd, stms)
                  | _ => stms
                (* end case *))
          val inputInits = List.foldr initDefault
          val body = ToC.trWithLocals (
                Env.insert(env, PseudoVars.global, dst),
                !locals,
                fn env => List.foldr initDefault (#2 (ToC.trStms (env, body))) inputs)
          in
            CL.D_Func(
              ["static"], CL.voidTy, [], "init_defaults",
              [CL.PARAM([], dstTy, dst)],
              body)
          end

  (* generate code to register command-line options for setting the input variables *)
    fun genRegisterInputs (env, inputs) = let
        (* pointer to the inputs *)
          val inputsV = "inp"
          val inputsE = CL.mkVar inputsV
        (* some common variables *)
          val optsV = CL.mkVar "opts"
        (* register a given input *)
          fun registerInput (Inp.INP{var, name, ty, desc, init}) = let
                val args = [CL.mkBool(Inp.isDefault init)]
                val args = (case ty
                       of Ty.TensorTy(shape as _::_) =>
                            CL.mkInt(IntLit.fromInt (List.foldl Int.* 1 shape)) ::
                            CL.mkSelect(
                              CL.mkIndirect(inputsE, GVar.qname var), "_data") ::
                            args
                        | Ty.SeqTy(_, SOME n) =>
                            CL.mkInt(IntLit.fromInt n) ::
                            CL.mkIndirect(inputsE, GVar.qname var) ::
                            args
                        | _ => CL.mkAddrOf(CL.mkIndirect(inputsE, GVar.qname var)) :: args
                      (* end case *))
                val args = (case desc
                        of NONE => CL.mkStr "" :: args
                         | SOME s => CL.mkStr s :: args
                       (* end case *))
                val args = CL.mkStr name :: args
                in
                  CL.mkExpStm (CL.mkIndirectDispatch (optsV, "add", args))
                end
          in
            CL.D_Func(
              ["static"], CL.voidTy, [], "register_inputs",
              [CL.PARAM([], RN.inputsPtrTy, inputsV), CL.PARAM([], RN.optionsPtrTy, "opts")],
              CL.mkBlock(List.map registerInput inputs))
          end

  (* generate the common functions that handle inputs for standalone executables.  These are:
   *    init_defaults    -- called to initialize the default input values
   *    register_inputs  -- called to register the command-line options for the input globals
   *)
    fun genExecInputFuns (env, IR.Program{inputs=[], constInit, ...}) =
          if IR.emptyBlk constInit
            then []
            else [genInitDefaults (env, [], RN.inputsPtrTy, "inp", constInit)]
      | genExecInputFuns (env, IR.Program{inputs, constInit, ...}) = [
            genInitDefaults (env, inputs, RN.inputsPtrTy, "inp", constInit),
            genRegisterInputs (env, inputs)
          ]

  (* an l-value expression for accessing a defined-input flag *)
    fun defined gv = CL.mkSelect(CL.mkIndirect(CL.mkVar "wrld", "_definedInp"), GVar.qname gv)

  (* generate the typedef for the defined-input flag struct. *)
    fun genDefinedInpStruct [] = []
      | genDefinedInpStruct inputs = let
          fun mkField (Inp.INP{var, ...}) = (CL.boolTy, GVar.qname var)
          in
            [CL.D_StructDef(NONE, List.map mkField inputs, SOME "defined_inputs")]
          end

  (* generate the function that initializes the defined-input flag struct. *)
    fun genDefineInp inputs = let
        (* the world pointer type *)
          val wrldParam = CL.PARAM([], worldPtrTy, "wrld")
          fun initFlag (Inp.INP{var, ...}) = CL.mkAssign(defined var, CL.mkBool false)
          in
            CL.D_Func(
              ["static"], CL.voidTy, [], "init_defined_inputs",
              [wrldParam],
              CL.mkBlock(List.map initFlag inputs))
          end

  (* generate the common functions that handle inputs for libraries.  These are:
   *    init_defined_inputs -- function that initializes the defined-input flag struct
   *    init_defaults       -- fucntion to initialize the default input values
   *)
    fun genLibraryInputFuns (env, IR.Program{inputs=[], constInit, ...}) =
          if IR.emptyBlk constInit
            then []
            else [genInitDefaults (env, [], RN.globalPtrTy, RN.globalsVar, constInit)]
      | genLibraryInputFuns (env, IR.Program{inputs, constInit, ...}) = [
            genDefineInp inputs,
            genInitDefaults (env, inputs, RN.globalPtrTy, RN.globalsVar, constInit)
          ]

  (* generate a static cast to a pointer to the given type *)
    fun castAPIPtr {env, ty, src} = (case ty
           of Ty.IntTy => CL.mkStaticCast(CL.T_Ptr(Env.intTy env), src)
            | Ty.BoolTy => CL.mkStaticCast(CL.T_Ptr CL.boolTy, src)
            | Ty.TensorTy _ => CL.mkStaticCast(CL.T_Ptr(Env.realTy env), src)
            | Ty.StringTy => raise Fail "cast for StringTy"
            | Ty.ImageTy _ => raise Fail "cast for ImageTy"
            | Ty.SeqTy(ty, SOME _) => castAPIPtr {env=env, ty=ty, src=src}
            | Ty.SeqTy(_, NONE) => raise Fail "cast for dynamic SeqTy"
          (* end case *))

  (* generate code to copy from a C++ global variable to a C reference *)
    fun copyToC {env, ty, src, dst} = let
          fun assign () = CL.mkAssign(dst, src)
          fun addrOf (CL.E_UnOp(CL.%*, x)) = x
            | addrOf x = CL.mkUnOp(CL.%&, x)
          fun memcpy src =
                CL.mkCall("std::memcpy", [dst, src, CL.mkSizeof(trType(env, ty))])
          in
            case ty
             of Ty.IntTy => assign ()
              | Ty.BoolTy => assign ()
              | Ty.TensorTy[] => assign ()
              | Ty.TensorTy shp => memcpy(CL.mkDispatch(src, "addr", [CL.mkInt 0]))
              | Ty.StringTy => CL.mkCall("std::memcpy", [
                    addrOf dst, CL.mkDispatch(src, "c_str", []), CL.mkDispatch(src, "size", [])
                  ])
              | Ty.ImageTy _ => raise Fail "unexpected image copy"
              | Ty.SeqTy(_, SOME _) => memcpy(addrOf src)
              | Ty.SeqTy(_, NONE) => raise Fail "unexpected dynamic sequence copy"
            (* end case *)
          end

  (* generate code to copy from a C input argument to the C++ global *)
    fun copyToCxx {env, ty, src, dst} = let
          fun assign () = CL.mkAssign(dst, src)
          fun addrOf (CL.E_UnOp(CL.%*, x)) = x
            | addrOf x = CL.mkUnOp(CL.%&, x)
          fun memcpy () =
                CL.mkCall("std::memcpy", [addrOf dst, addrOf src, CL.mkSizeof(trType(env, ty))])
          in
            case ty
             of Ty.IntTy => assign ()
              | Ty.BoolTy => assign ()
              | Ty.TensorTy[] => assign ()
              | Ty.TensorTy shp => assign ()  (* uses overloaded assignment *)
              | Ty.StringTy => assign()
              | Ty.ImageTy _ => raise Fail "unexpected image copy"
              | Ty.SeqTy(_, SOME _) => memcpy()
              | Ty.SeqTy(_, NONE) => raise Fail "unexpected dynamic sequence copy"
            (* end case *)
          end

  (* generate code to copy a value from one variable to another *)
    fun copy {env, ty, src, dst} = let
          fun assign () = CL.mkAssign(dst, src)
          fun addrOf (CL.E_UnOp(CL.%*, x)) = x
            | addrOf x = CL.mkUnOp(CL.%&, x)
          fun memcpy () =
                CL.mkCall("std::memcpy", [addrOf dst, addrOf src, CL.mkSizeof(trType(env, ty))])
          in
            case ty
             of Ty.IntTy => assign ()
              | Ty.BoolTy => assign ()
              | Ty.TensorTy[] => assign ()
              | Ty.TensorTy shp => assign ()  (* uses overloaded assignment *)
              | Ty.StringTy => assign()
              | Ty.ImageTy _ => raise Fail "unexpected image copy"
              | Ty.SeqTy(_, SOME _) => memcpy()
              | Ty.SeqTy(_, NONE) => raise Fail "unexpected dynamic sequence copy"
            (* end case *)
          end

  end
