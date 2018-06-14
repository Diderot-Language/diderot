(* gen-tys-and-ops.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenTysAndOps : sig

    val gen : CodeGenEnv.t * CollectInfo.t -> {
            preWorld : CLang.decl list,         (* decls to place before world outside of namespace *)
            postWorld : CLang.decl list         (* decls to place after world inside of namespace *)
          }

  end = struct

    structure IR = TreeIR
    structure Ty = TreeTypes
    structure CL = CLang
    structure RN = CxxNames
    structure Env = CodeGenEnv

    val zero = RealLit.zero false

    fun mkReturn exp = CL.mkReturn(SOME exp)
    fun mkInt i = CL.mkInt(IntInf.fromInt i)
    fun mkFunc (ty, name, params, body) = CL.D_Func(["inline"], ty, [], name, params, body)
   (* make a constructor function's prototype and out-of-line definition *)
    fun mkConstr (cls, params, inits) = (
            CL.D_Constr([], [], cls, params, NONE),
            CL.D_Constr(["inline"], [CL.SC_Type(CL.T_Named cls)], cls, params, SOME(inits, CL.mkBlock[]))
          )
   (* make a member function prototype and out-of-line definition *)
    fun mkMemberFn (cls, ty, f, params, body) = (
            CL.D_Proto([], ty, f, params),
            CL.D_Func(["inline"], ty, [CL.SC_Type(CL.T_Named cls)], f, params, body)
          )

    fun trType namespace env ty = TypeToCxx.trQType(env, namespace, ty)

    fun sequenceTy (elemTy, sz) =
          CL.T_Template("diderot::array", [elemTy, CL.T_Named(Int.toString sz)])

    fun genTyDecl env = let
          val (realTy, realTyName, realTySz) = if #double(Env.target env)
                then (CL.double, "double", 8)
                else (CL.float, "float", 4)
        (* generate the type and member declarations for a recorded type *)
          fun genDecl (ty, (tyDcls, fnDefs)) = (case ty
                 of Ty.VecTy(w, pw) => let
                      val cTyName = RN.vecTyName w
                      val cTy = CL.T_Named cTyName
                      val typedefDcl = CL.D_Verbatim[concat[
                              "typedef ", realTyName, " ", cTyName,
                              " __attribute__ ((vector_size (",
                              Int.toString(realTySz * pw), ")));"
                            ]]
                      in
                        (typedefDcl :: tyDcls, fnDefs)
                      end
                  | Ty.TensorRefTy shape => let
                      val name = RN.tensorRefStruct shape
                      val baseCls = concat[
                              "diderot::tensor_ref<", realTyName, ",",
                              Int.toString(List.foldl Int.* 1 shape), ">"
                            ]
                      fun mkConstr' (paramTy, paramId, arg) = mkConstr (
                            name,
                            [CL.PARAM([], paramTy, paramId)],
                            [CL.mkApply(baseCls, [arg])])
                    (* constructor from float/double pointer *)
                      val (constrProto1, constrDef1) = mkConstr' (
                            CL.constPtrTy realTy, "src", CL.mkVar "src")
                    (* constructor from tensor struct *)
                      val (constrProto2, constrDef2) = mkConstr' (
                            CL.T_Named(concat["struct ", RN.tensorStruct shape, " const &"]),
                            "ten", CL.mkSelect(CL.mkVar "ten", "_data"))
                    (* copy constructor *)
                      val (constrProto3, constrDef3) = mkConstr' (
                            CL.T_Named(name ^ " const &"), "ten",
                            CL.mkSelect(CL.mkVar "ten", "_data"))
                      val thisData = CL.mkIndirect(CL.mkVar "this", "_data")
                    (* last vector as tensor_ref *)
                      val lastDcl = (case shape
                            of [] => raise Fail "unexpected TensorRef[]"
                             | [_] => []
                             | _::dd => let
                                val d = List.last dd
                                in [
                                  CL.D_Func([], RN.tensorRefTy[d], [], "last",
                                    [CL.PARAM([], CL.uint32, "i")],
                                    CL.mkReturn(
                                      SOME(CL.mkAddrOf(CL.mkSubscript(thisData, CL.mkVar "i")))))
                                ] end
                            (* end case *))
                      val members = constrProto1 :: constrProto2 :: constrProto3 :: lastDcl
                      val structDcl = CL.D_ClassDef{
                              name = name,
                              args = NONE,
                              from = SOME("public " ^ baseCls),
                              public = members,
                              protected = [],
                              private = []
                            }
                      in
                        (structDcl :: tyDcls, constrDef1 :: constrDef2 :: constrDef3 :: fnDefs)
                      end
                  | Ty.TensorTy shape => let
                      val len = List.foldl Int.* 1 shape
                      val name = RN.tensorStruct shape
                      val baseCls = concat[
                              "diderot::tensor<", realTyName, ",",
                              Int.toString(List.foldl Int.* 1 shape), ">"
                            ]
                      fun mkConstr (paramTy, paramId, arg) = CL.D_Constr (
                            [], [], name,
                            [CL.PARAM([], paramTy, paramId)],
                            SOME([CL.mkApply(baseCls, [arg])], CL.mkBlock[]))
                    (* default constructor *)
                      val constrDcl1 = CL.D_Constr (
                            [], [], name, [], SOME([CL.mkApply(baseCls, [])], CL.mkBlock[]))
                    (* constructor from initializer list *)
                      val constrDcl2 = mkConstr (
                            CL.T_Template("std::initializer_list", [realTy]), "const & il",
                            CL.mkVar "il")
                    (* constructor from float/double pointer *)
                      val constrDcl3 = mkConstr (
                            CL.constPtrTy realTy, "src", CL.mkVar "src")
                    (* copy constructor *)
                      val constrDcl4 = mkConstr (
                            CL.T_Named(name ^ " const &"), "ten",
                            CL.mkSelect(CL.mkVar "ten", "_data"))
                    (* destructor *)
                      val destrDcl = CL.D_Destr([], [], name, SOME(CL.mkBlock[]))
                      val thisData = CL.mkIndirect(CL.mkVar "this", "_data")
                      val returnThis = CL.mkReturn(SOME(CL.mkUnOp(CL.%*, CL.mkVar "this")))
                    (* assignment from Tensor *)
                      val (assignProto1, assignDef1) = mkMemberFn(name,
                              CL.T_Named(name ^ " &"), "operator=",
                              [CL.PARAM([], CL.T_Named name, "const & src")],
                              CL.mkBlock[
                                  CL.mkCall("this->copy", [CL.mkSelect(CL.mkVar "src", "_data")]),
                                  returnThis
                                ])
                    (* assignment from TensorRef *)
                      val (assignProto2, assignDef2) = mkMemberFn(name,
                              CL.T_Named(name ^ " &"), "operator=",
                              [CL.PARAM([], CL.T_Named(RN.tensorRefStruct shape), "const & src")],
                              CL.mkBlock[
                                  CL.mkCall("this->copy", [CL.mkSelect(CL.mkVar "src", "_data")]),
                                  returnThis
                                ])
                    (* assignment from initializer list *)
                      val (assignProto3, assignDef3) = mkMemberFn(name,
                              CL.T_Named(name ^ " &"), "operator=",
                              [CL.PARAM([], CL.T_Template("std::initializer_list", [realTy]), "const & il")],
                              CL.mkBlock[
                                  CL.mkCall("this->copy", [CL.mkVar "il"]),
                                  returnThis
                                ])
                    (* assignment from array *)
                      val (assignProto4, assignDef4) = mkMemberFn(name,
                              CL.T_Named(name ^ " &"), "operator=",
                              [CL.PARAM([], CL.constPtrTy realTy, "src")],
                              CL.mkBlock[
                                  CL.mkCall("this->copy", [CL.mkVar "src"]),
                                  returnThis
                                ])
                    (* last vector as tensor_ref *)
                      val lastDcl = (case shape
                            of [] => raise Fail "unexpected TensorTy[]"
                             | [_] => []
                             | _::dd => let
                                val d = List.last dd
                                in [
                                  CL.D_Func([], RN.tensorRefTy[d], [], "last",
                                      [CL.PARAM([], CL.uint32, "i")],
                                      CL.mkReturn(
                                        SOME(CL.mkAddrOf(CL.mkSubscript(thisData, CL.mkVar "i")))))
                                ] end
                            (* end case *))
                      val structDcl = CL.D_ClassDef{
                              name = name,
                              args = NONE,
                              from = SOME("public " ^ baseCls),
                              public =
                                  constrDcl1 :: constrDcl2 :: constrDcl3 :: constrDcl4 ::
                                  destrDcl ::
                                  assignProto1 :: assignProto2 :: assignProto3 :: assignProto4 ::
                                  lastDcl,
                              protected = [],
                              private = []
                            }
                      val fnDefs = assignDef1 :: assignDef2 :: assignDef3 :: assignDef4 :: fnDefs
                      in
                        (structDcl :: tyDcls, fnDefs)
                      end
                  | Ty.TupleTy tys => raise Fail "FIXME: TupleTy"
(* TODO
                  | Ty.SeqTy(ty, NONE) =>
                  | Ty.SeqTy(ty, SOME n) =>
*)
                  | ty => (tyDcls, fnDefs)
                (* end case *))
          in
            genDecl
          end

  (* generate an instance of the dynamic-sequence trait.  This struct has the following
   * components (see src/lib/include/diderot/dynseq.hxx)
   *
   *    template <>
   *    struct dynseq_traits<T> {
   *        using value_type = T;
   *        using base_type = BASE_TY;
   *        static const __details::load_fn_ptr<base_type> *load_fn_tbl;
   *        static const uint32_t values_per_elem = N;
   *    };
   *    const __details::load_fn_ptr< dynseq_traits< T >::base_type > *dynseq_traits< T >::load_fn_tbl
   *        = LOAD_FN;
   *
   * where
   *    T is the C++ element type of the dynamic sequence (e.g., "tensor_2" for "vec2[]")
   *    BASE_TY is the type of values that comprise an element (e.g., "float" for "vec2[]")
   *    N is the number of values per sequence element (e.g., 1 for "int[]" and 9 for "tensor[3,3][]")
   *    LOAD_FN is the nrrd load function for the base type (e.g., nrrdDLoad if BASE_TY is double)
   *)
    fun genSeqTrait env = let
          val ns = #namespace(Env.target env)
          val realTy = Env.realTy env
          val trType = trType TypeToCxx.NSDiderot env
          fun trait ({argTy, baseTy, elemTy, nValsPerElem}, dcls) = let
              (* the name of the teem function table for the given base type *)
                val loadTbl = (case baseTy
                       of Ty.BoolTy => "nrrdILoad"
                        | Ty.IntTy => "nrrdILoad"
                        | Ty.VecTy(1, 1) => if #double(Env.target env)
                            then "nrrdDLoad"
                            else "nrrdFLoad"
                        | ty => raise Fail("genSeqTrait.loadFn: unexpected type " ^ Ty.toString ty)
                      (* end case *))
                val loadTblTy = CL.constPtrTy(CL.T_Named "__details::load_fn_ptr<base_type>")
                val seqTy = CL.T_Template("dynseq_traits", [argTy])
                val scope = CL.SC_Type seqTy
                in
                  CL.D_Template([], CL.D_ClassDef{
                      name = "dynseq_traits",
                      args = SOME[argTy],
                      from = NONE,
                      public = [
                          CL.D_Typedef("value_type", elemTy),
                          CL.D_Typedef("base_type", trType baseTy),
                          CL.D_Var(
                            ["static"],
                            CL.constPtrTy(CL.T_Named "__details::load_fn_ptr<base_type>"),
                            [], "load_fn_tbl", NONE),
                          CL.D_Var(
                            ["static", "const"], CL.uint32, [], "values_per_elem",
                            SOME(CL.I_Exp(mkInt nValsPerElem)))
                        ],
                      protected = [],
                      private = []
                    }) ::
                  CL.D_Var(
                    ["const"],
                    CL.T_Ptr(
                      CL.T_Template("__details::load_fn_ptr", [CL.T_Member(seqTy, "base_type")])),
                    [scope], "load_fn_tbl",
                    SOME(CL.I_Exp(CL.mkVar loadTbl))) ::
                  dcls
                end
          fun elemTy (Ty.SeqTy(ty, _)) = elemTy ty
            | elemTy (Ty.TensorTy[]) = Ty.realTy
            | elemTy ty = ty
          fun genTrait (ty, dcls) = (case ty
                 of Ty.SeqTy(argTy, NONE) => let
                      val argTy = trType argTy
                    (* for sequences of scalar values, we set nDims to 0 so that it matches the
                     * format of a nrrd, where the dimension is not represented.
                     *)
                      fun scalarSeqTrait ty = trait ({
                                argTy = argTy, baseTy = ty, elemTy = argTy, nValsPerElem = 1
                              },
                            dcls)
                      in
                        case elemTy ty
                         of ty as Ty.TensorTy(shp as _::_) => trait ({
                                  argTy = argTy, baseTy = Ty.realTy,
                                  elemTy = argTy,
                                  nValsPerElem = List.foldl Int.* 1 shp
                                },
                              dcls)
                          | ty as Ty.BoolTy => scalarSeqTrait ty
                          | ty as Ty.IntTy => scalarSeqTrait ty
                          | ty as Ty.VecTy(1, 1) => scalarSeqTrait ty
(* QUESTION: strands map to uint32_t and do not support loading; do we need a trait? *)
                          | ty as Ty.StrandIdTy _ => dcls
                          | ty => raise Fail("unexpected dynamic sequence of " ^ Ty.toString ty)
                        (* end case *)
                      end
                  | _ => dcls
                (* end case *))
          in
            genTrait
          end

    datatype operation = datatype CollectInfo.operation

    val ostreamRef = CL.T_Named "std::ostream&"

    fun output (e, e') = CL.mkBinOp(e, CL.#<<, e')

  (* generate code for the expression "e << s", where "s" is string literal *)
    fun outString (CL.E_BinOp(e, CL.#<<, CL.E_Str s1), s2) =
          output (e, CL.mkStr(s1 ^ String.toCString s2))
      | outString (e, s) = output (e, CL.mkStr(String.toCString s))

  (* generate a printing function for tensors with the given shape *)
    fun genTensorPrinter shape = let
          fun ten i = CL.mkSubscript(CL.mkSelect(CL.mkVar "ten", "_data"), mkInt i)
          fun prefix (true, lhs) = lhs
            | prefix (false, lhs) = outString(lhs, ",")
          fun lp (isFirst, lhs, i, [d]) = let
                fun lp' (_, lhs, i, 0) = (i, outString(lhs, "]"))
                  | lp' (isFirst, lhs, i, n) =
                      lp' (false, output (prefix (isFirst, lhs), ten i), i+1, n-1)
                in
                  lp' (true, outString(lhs, "["), i, d)
                end
            | lp (isFirst, lhs, i, d::dd) = let
                fun lp' (_, lhs, i, 0) = (i, outString(lhs, "]"))
                  | lp' (isFirst, lhs, i, n) = let
                      val (i, lhs) = lp (true, prefix (isFirst, lhs), i, dd)
                      in
                        lp' (false, lhs, i, n-1)
                      end
                in
                  lp' (true, outString(lhs, "["), i, d)
                end
          val params = [
                  CL.PARAM([], ostreamRef, "outs"),
                  CL.PARAM([], RN.tensorRefTy shape, "const & ten")
                ]
          val (_, exp) = lp (true, CL.mkVar "outs", 0, shape)
          in
            CL.D_Func(["static"], ostreamRef, [], "operator<<", params, mkReturn exp)
          end

  (* generate a printing function for fixed-size sequences *)
    fun genSeqPrinter (env, elemTy, size) = let
          val elemTy' = trType TypeToCxx.NSDiderot env elemTy
          val seqTy = sequenceTy (elemTy', size)
          val params = [
                  CL.PARAM([], ostreamRef, "outs"),
                  CL.PARAM([], seqTy, "const & seq")
                ]
          val outsV = CL.mkVar "outs"
          val seqV = CL.mkVar "seq"
          val body = [
                  CL.mkExpStm(outString(outsV, "}")),
                  mkReturn outsV
                ]
        (* get a sequence element *)
          fun getElem ix = CL.mkSubscript(seqV, ix)
          val body = if (size > 0)
                then CL.mkExpStm(output(outsV, getElem(CL.mkInt 0))) ::
                  CL.mkFor(
                    CL.int32, [("i", CL.mkInt 1)],
                    CL.mkBinOp(CL.mkVar "i", CL.#<, CL.mkDispatch(seqV, "size", [])),
                    [CL.mkUnOp(CL.%++, CL.mkVar "i")],
                    CL.mkExpStm(output(outString(outsV, ","), getElem(CL.mkVar "i"))))
                  :: body
                else body
          val body = CL.mkExpStm(outString(outsV, "{")) :: body
          in
            CL.D_Func(["static"], ostreamRef, [], "operator<<", params, CL.mkBlock body)
          end

  (* builds AST for the expression "(x <= lo) ? lo : (hi <= x) ? hi : x;" *)
    fun mkClampExp (lo, hi, x) =
          CL.mkCond(CL.mkBinOp(x, CL.#<=, lo), lo,
            CL.mkCond(CL.mkBinOp(hi, CL.#<=, x), hi,
              x))

    fun expandFrag (env, frag) =
          CL.verbatimDcl [frag] [("REALTY", if #double(Env.target env) then "double" else "float")]

    fun imageTy (realTy, d) =
          CL.T_Template(RN.qImageTyName d, [realTy, CL.T_Named "TY", CL.T_Named "VOXSZ"])

    fun doOp env (rator, dcls) = let
          val realTy = Env.realTy env
          fun mkVec (w, pw, f) = CL.mkVec(
                RN.vecTy w,
                List.tabulate(pw, fn i => if i < w then f i else CL.mkFlt(zero, realTy)))
          fun mkVMap (ty, name, f, w, pw) = let
                fun f' i = CL.mkApply(f, [CL.mkSubscript(CL.mkVar "v", mkInt i)])
                in
                  mkFunc(ty, name, [CL.PARAM([], ty, "v")], mkReturn (mkVec (w, pw, f')))
                end
          val dcl = (case rator
                 of Print(Ty.TensorRefTy shape) => genTensorPrinter shape
                  | Print(Ty.TupleTy tys) => raise Fail "FIXME: printer for tuples"
                  | Print(Ty.SeqTy(ty, NONE)) => CL.D_Verbatim[] (* no printer needed *)
                  | Print(Ty.SeqTy(ty, SOME n)) => genSeqPrinter (env, ty, n)
                  | Print ty => CL.D_Verbatim[] (* no printer needed *)
                  | RClamp => let
                      val params = [
                              CL.PARAM([], realTy, "lo"),
                              CL.PARAM([], realTy, "hi"),
                              CL.PARAM([], realTy, "x")
                            ]
                      in
                        mkFunc(realTy, "clamp", params,
                          mkReturn(mkClampExp (CL.mkVar "lo", CL.mkVar "hi", CL.mkVar "x")))
                      end
                  | RLerp => let
                      val params = [
                              CL.PARAM([], realTy, "a"),
                              CL.PARAM([], realTy, "b"),
                              CL.PARAM([], realTy, "t")
                            ]
                      in
                        mkFunc(realTy, "lerp", params,
                          mkReturn (
                            CL.mkBinOp(
                              CL.mkVar "a",
                              CL.#+,
                              CL.mkBinOp(
                                CL.mkVar "t",
                                CL.#*,
                              CL.mkBinOp(CL.mkVar "b", CL.#-, CL.mkVar "a")))))
                      end
                  | VScale(w, pw) => let
                      val cTy = RN.vecTy w
                      in
                        mkFunc(cTy, RN.vscale w,
                          [CL.PARAM([], realTy, "s"), CL.PARAM([], cTy, "v")],
                          mkReturn(
                            CL.mkBinOp(mkVec(w, pw, fn _ => CL.mkVar "s"), CL.#*, CL.mkVar "v")))
                      end
                  | VSum(w, pw) => let
                      val name = RN.vsum w
                      val params = [CL.PARAM([], RN.vecTy w, "v")]
                      fun mkSum 0 = CL.mkSubscript(CL.mkVar "v", mkInt 0)
                        | mkSum i = CL.mkBinOp(mkSum(i-1), CL.#+, CL.mkSubscript(CL.mkVar "v", mkInt i))
                      in
                        mkFunc(realTy, name, params, mkReturn(mkSum(w-1)))
                      end
                  | VDot(w, pw) => let
                      val name = RN.vdot w
                      val vTy = RN.vecTy w
                      val params = [CL.PARAM([], vTy, "u"), CL.PARAM([], vTy, "v")]
                      fun mkSum 0 = CL.mkSubscript(CL.mkVar "w", mkInt 0)
                        | mkSum i = CL.mkBinOp(mkSum(i-1), CL.#+, CL.mkSubscript(CL.mkVar "w", mkInt i))
                      in
                        mkFunc(realTy, name, params,
                          CL.mkBlock[
                              CL.mkDeclInit(vTy, "w", CL.mkBinOp(CL.mkVar "u", CL.#*, CL.mkVar "v")),
                              mkReturn(mkSum(w-1))
                            ])
                      end
                  | VCeiling(w, pw) => mkVMap (RN.vecTy w, RN.vceiling w, "diderot::ceiling", w, pw)
                  | VFloor(w, pw) => mkVMap (RN.vecTy w, RN.vfloor w, "diderot::floor", w, pw)
                  | VRound(w, pw) => mkVMap (RN.vecTy w, RN.vround w, "diderot::round", w, pw)
                  | VTrunc(w, pw) => mkVMap (RN.vecTy w, RN.vtrunc w, "diderot::trunc", w, pw)
                  | VToInt(layout as {wid, pieces, ...}) => let
                      val intTy = Env.intTy env
                      val seqTy = sequenceTy (CL.intTy, wid)
                      fun mkItem i =
                            CL.I_Exp(CL.mkCons(intTy, [CL.mkSubscript(CL.mkVar "src", mkInt i)]))
                      val vParamTys = Ty.piecesOf layout
                      val vParams = List.mapi
                            (fn (i, Ty.VecTy(w, _)) => CL.PARAM([], RN.vecTy w, "v"^Int.toString i))
                              vParamTys
                      val initItems = let
                            fun doPiece (i, Ty.VecTy(w, _)) = let
                                  val src = CL.mkVar("v"^Int.toString i)
                                  fun mkItem j =
                                        CL.I_Exp(CL.mkCons(intTy, [CL.mkSubscript(src, mkInt j)]))
                                  in
                                    List.tabulate (w, mkItem)
                                  end
                            in
                              List.concat(List.mapi doPiece vParamTys)
                            end
                      in
                        mkFunc(seqTy, RN.vtoi wid,
                          vParams,
                          CL.mkBlock[
                              CL.mkDecl(seqTy, "res", SOME(CL.I_Exps initItems)),
                              mkReturn(CL.mkVar "res")
                            ])
                      end
                  | VLoad(w, pw) => let
                      val name = RN.vload w
                      val cTy = RN.vecTy w
                      fun arg i = CL.mkSubscript(CL.mkVar "vp", mkInt i)
                      in
                        mkFunc(cTy, name,
                          [CL.PARAM(["const"], CL.T_Ptr realTy, "vp")],
                          mkReturn(mkVec (w, pw, arg)))
                      end
                  | VCons(w, pw) => let
                      val name = RN.vcons w
                      val cTy = RN.vecTy w
                      val params = List.tabulate(w, fn i => CL.PARAM([], realTy, "r"^Int.toString i))
                      fun arg i = CL.mkVar("r"^Int.toString i)
                      in
                        mkFunc(cTy, name, params, mkReturn(mkVec (w, pw, arg)))
                      end
                  | VPack layout => let
                      val name = RN.vpack (#wid layout)
                      val vParamTys = Ty.piecesOf layout
                      val vParams = List.mapi
                            (fn (i, Ty.VecTy(w, _)) => CL.PARAM([], RN.vecTy w, "v"^Int.toString i))
                              vParamTys
                      val dstTy = RN.tensorTy[#wid layout]
                      fun mkAssign (i, v, j) =
                            CL.mkAssign(
                              CL.mkSubscript(CL.mkSelect(CL.mkVar "dst", "_data"), mkInt i),
                              CL.mkSubscript(v, mkInt j))
                      fun mkAssignsForPiece (dstStart, pieceIdx, wid, stms) = let
                            val piece = CL.mkVar("v"^Int.toString pieceIdx)
                            fun mk (j, stms) = if (j < wid)
                                  then mk (j+1, mkAssign (dstStart+j, piece, j) :: stms)
                                  else stms
                            in
                              mk (0, stms)
                            end
                      fun mkAssigns (_, [], _, stms) = CL.mkBlock(List.rev stms)
                        | mkAssigns (i, Ty.VecTy(w, _)::tys, offset, stms) =
                            mkAssigns (i+1, tys, offset+w, mkAssignsForPiece(offset, i, w, stms))
                      in
                        mkFunc(CL.voidTy, name,
                          CL.PARAM([], dstTy, "&dst") :: vParams,
                          mkAssigns (0, vParamTys, 0, []))
                      end
                  | TensorCopy shp => CL.D_Verbatim[]
(*
                  | TensorCopy shp => let
                      val name = RN.tensorCopy shp
                      val dim = List.foldl Int.* 1 shp
                      val dstTy = CL.T_Array(realTy, SOME dim)
                      in
                        mkFunc(CL.voidTy, name,
                          [CL.PARAM([], dstTy, "dst"), CL.PARAM([], CL.constPtrTy realTy, "src")],
                          CL.mkCall("std::memcpy", [
                              CL.mkVar "dst", CL.mkVar "src", CL.mkSizeof dstTy
                            ]))
                      end
*)
                  | Transform d => let
                      val e = CL.mkDispatch(CL.mkVar "img", "world2image", [])
                      val (resTy, e) = if (d = 1)
                            then (realTy, e)
                            else let val ty = RN.tensorRefTy[d, d]
                              in (ty, CL.mkCons(ty, [e])) end
                      in
                        CL.D_Template([CL.TypeParam "TY", CL.ConstParam(CL.intTy, "VOXSZ")],
                          mkFunc(resTy, "world2image",
                            [CL.PARAM([], imageTy (realTy, d), "const & img")],
                            CL.mkReturn(SOME e)))
                      end
                  | Translate d => let
                      val e = CL.mkDispatch(CL.mkVar "img", "translate", [])
                      val (resTy, e) = if (d = 1)
                            then (realTy, e)
                            else let val ty = RN.tensorRefTy[d]
                              in (ty, CL.mkCons(ty, [e])) end
                      in
                        CL.D_Template([CL.TypeParam "TY", CL.ConstParam(CL.intTy, "VOXSZ")],
                          mkFunc(resTy, "translate",
                            [CL.PARAM([], imageTy (realTy, d), "const & img")],
                            CL.mkReturn(SOME e)))
                      end
                  | Inside(layout, s) => let
                      val dim = #wid layout
                      val vTys = List.map
                            (fn ty => TypeToCxx.trType (env, ty))
                              (TreeTypes.piecesOf layout)
                      val xs = List.mapi (fn (i, ty) => "x"^Int.toString i) vTys
                      val vParams =
                            ListPair.map (fn (ty, x) => CL.PARAM([], ty, x)) (vTys, xs)
                    (* make the tests `(x < img.size(i)-s)` and `((s-1) < x)` *)
                      fun mkTests (x, i) = [
                              CL.mkBinOp(x, CL.#<,
                                CL.mkBinOp(
                                  CL.mkDispatch(CL.mkVar "img", "size", [mkInt i]),
                                  CL.#-, mkInt s)),
                              CL.mkBinOp(mkInt(s-1), CL.#<, x)
                            ]
                    (* build the test expression from the pieces *)
                      fun mkExps (i, w, v::vr, pw::pwr, tests) =
                            if (i < dim)
                              then if (w < pw)
                                then let
                                  val x = if (pw = 1)
                                        then CL.mkVar v
                                        else CL.mkSubscript(CL.mkVar v, mkInt w)
                                  in
                                    mkExps (i+1, w+1, v::vr, pw::pwr,
                                      mkTests(x, w) @ tests)
                                  end
                                else mkExps (i, pw, vr, pwr, tests)
                              else List.rev tests
                        | mkExps _ = raise Fail "inconsistent"
                      val (t1::tr) = mkExps (0, 0, xs, #pieces layout, [])
                      val exp = List.foldr
                            (fn (e1, e2) => CL.mkBinOp(e2, CL.#&&, e1))
                              t1 tr
                      in
                        CL.D_Template([CL.TypeParam "TY", CL.ConstParam(CL.intTy, "VOXSZ")],
                          mkFunc(CL.boolTy, RN.inside(dim, s),
                            vParams @ [CL.PARAM([], imageTy (realTy, dim), "img")],
                            mkReturn exp))
                      end
                  | EigenVals2x2 => expandFrag (env, CxxFragments.eigenvals2x2)
                  | EigenVals3x3 => expandFrag (env, CxxFragments.eigenvals3x3)
                  | EigenVecs2x2 => expandFrag (env, CxxFragments.eigenvecs2x2)
                  | EigenVecs3x3 => expandFrag (env, CxxFragments.eigenvecs3x3)
                  | SphereQuery(d, s) => let
                      val seqTy = CL.T_Template("diderot::dynseq", [CL.T_Named "strand_array::sid_t"])
                      val (posParam, posExp) = if d > 1
                            then (
                                CL.PARAM([], TypeToCxx.trType(env, Ty.TensorRefTy[d]), "pos"),
                                CL.mkDispatch(CL.mkVar "pos", "base", [])
                              )
                            else (CL.PARAM([], realTy, "pos"), CL.mkAddrOf(CL.mkVar "pos"))
                      val wrldV = CL.mkVar RN.worldVar
                      val selfParam = CL.PARAM([], CL.constPtrTy(RN.strandTy s), RN.selfVar)
                      val radiusParam = CL.PARAM([], realTy, "radius")
                      in
                        mkFunc(
                          seqTy, "sphere_query",
                          [RN.worldParam, selfParam, posParam, radiusParam],
                          CL.mkReturn(SOME(
                            CL.mkIndirectDispatch(
                              CL.mkIndirect(wrldV, "_tree"),
                              "sphere_query",
                              [CL.mkVar RN.selfVar, posExp, CL.mkVar "radius"]))))
                      end
                  | RIfWrap => let
                      val t1 = "float IfWrap (bool  e1, float e3, float e4)"
                      val t2 = "\n\t{"
                      val t3 = " \n\t\t if(e1){return e3;}"
                      val t4 = "\n\t\t    else{return e4;}"
                      val t5 = "\n\t}"
                      val es = [t1,t2,t3,t4,t5]
                      in CL.D_Verbatim es end
                (* end case *))
          in
            dcl :: dcls
          end

    val firstTy = CL.D_Comment["***** Begin synthesized types *****"]
    val lastTy = CL.D_Comment["***** End synthesized types *****"]
    val noDclsTy = CL.D_Comment["***** No synthesized types *****"]

    val firstOp = CL.D_Comment["***** Begin synthesized operations *****"]
    val lastOp = CL.D_Comment["***** End synthesized operations *****"]
    val noDclsOp = CL.D_Comment["***** No synthesized operations *****"]

    fun gen (env, info) = let
          val spec = Env.target env
          val genTrait = genSeqTrait env
          val genTyDecl = genTyDecl env
          val opDcls = List.foldl (doOp env) [] (CollectInfo.listOps info)
          val tys = CollectInfo.listTypes info
          val (tyDcls, fnDefs) = List.foldr genTyDecl ([], []) tys
          val dcls = tyDcls @ fnDefs
          val traitDcls = List.foldl genTrait [] tys
          val preDcls = if List.null dcls andalso List.null traitDcls
                then [noDclsTy]
                else let
                  val res = [lastTy]
                  val res = if List.null traitDcls
                        then res
                        else CL.D_Namespace("diderot", traitDcls) :: res
                  val res = if List.null dcls
                        then res
                        else CL.D_Namespace(#namespace(Env.target env), dcls) :: res
                  in
                    firstTy :: res
                  end
          val postDcls = if List.null opDcls
                then [noDclsOp]
                else firstOp :: opDcls @ [lastOp]
          in
            {preWorld = preDcls, postWorld = postDcls}
          end

  end
