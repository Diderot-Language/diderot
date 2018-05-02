(* gen-debugger-hooks.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure GenDebuggerHooks : sig

  (* generate the extra C wrapper functions used by the debugger to query the program
   * state.
   * NOTE: the names of these functions must agree with the names used in the JSON
   * description of the program's API.
   *)
    val gen : CodeGenEnv.t * TreeIR.program -> CLang.decl list

  end = struct

    structure Env = CodeGenEnv
    structure IR = TreeIR
    structure TSV = TreeStateVar
    structure CL = CLang
    structure GenLib = GenLibraryInterface
    structure Util = GenInputsUtil

    val externC = ["extern \"C\""]

    fun dataPtr (ty, var) = (case ty
           of APITypes.TensorTy(_::_) => CL.mkDispatch(var, "base", [])
            | APITypes.StringTy => CL.mkDispatch(var, "c_str", [])
            | APITypes.ImageTy _ => raise Fail "unexpected ImageTy"
            | APITypes.SeqTy _ => CL.mkDispatch(var, "data", [])
            | _ => CL.mkAddrOf var
          (* end case *))

    fun getSize (ty, var) = (case ty
           of APITypes.StringTy => CL.mkDispatch(var, "size", [])
            | APITypes.ImageTy _ => CL.mkDispatch(var, "size", [])
            | APITypes.SeqTy _ => CL.mkDispatch(var, "length", [])
            | _ => raise Fail "does not have dynamic size"
          (* end case *))

  (* Generate the get function for a state variable.  The basic form for
   * statically sized variables is:
   *
   *    extern "C" void *NS_STRAND_get_VAR (NS_world_t *wrld, uint32_t id)
   *    {
   *        NS::world *w = reinterpret_cast<NS::world *>(wrld);
   *        if (w->_strands.validIndex(id) && w->_strands.isAlive(id)) {
   *            return &(w->_strands.XXX_state(id)->VAR);
   *        }
   *        else {
   *            return nullptr;
   *        }
   *    }
   *
   * where
   *    NS      - the name space
   *    STRAND  - the name of the strand type
   *    VAR     - the name of the variable
   *    TY      - the C type for representing the variable
   *    XXX     - either "in" or "local", depending on whether the state variable is
   *              shared or not.
   *
   * for state variables that have tensor, sequence, or dynamic sequence type, we
   * use the appropriate method to return the data pointer.
   *
   * For dynamic sequences, we define two functions:
   *
   *    extern "C" uint32_t NS_STRAND_sizeof_VAR (NS_world_t *wrld, uint32_t id)
   *    {
   *        NS::world *w = reinterpret_cast<NS::world *>(wrld);
   *        if (w->_strands.validIndex(id) && w->_strands.isAlive(id)) {
   *            return &(w->_strands.XXX_state(id)->VAR.length());
   *        }
   *        else {
   *            return 0;
   *        }
   *    }
   *
   * and
   *
   *    extern "C" void *NS_STRAND_elemof_VAR (NS_world_t *wrld, uint32_t id, uint32_t ix)
   *    {
   *        NS::world *w = reinterpret_cast<NS::world *>(wrld);
   *        if (w->_strands.validIndex(id) && w->_strands.isAlive(id)
   *	    && (ix < w->_strands.XXX_state(id)->VAR.length())) {
   *            return &(w->_strands.XXX_state(id)->VAR[ix]);
   *        }
   *        else {
   *            return nullptr;
   *        }
   *    }
   *)
    fun mkGetFn (env, strandName) = let
          val spec = Env.target env
          val ns = #namespace spec
          val wrldParam = CL.PARAM([], GenLib.worldTy spec, "wrld")
          val idParam = CL.PARAM([], CL.uint32, "id")
          val wrldPtrTy = CL.T_Ptr(CL.T_Named(ns ^ "::world"))
          val wrldDcl = CL.mkDeclInit(wrldPtrTy, "w",
                CL.mkReinterpretCast(wrldPtrTy, CL.mkVar "wrld"))
          val strands = CL.mkIndirect(CL.mkVar "w", "_strands")
          val idV = CL.mkVar "id"
          fun getFns (sv, dcls) = let
                val fname = concat[ns, "_", strandName, "_get_", TSV.name sv]
                val state = CL.mkDispatch(
                      strands,
                      if TSV.isShared sv then "in_state" else "local_state",
                      [CL.mkVar "id"])
                val ty = TSV.apiTy sv
                fun mkFunc getFn = CL.D_Func(
                      externC, CL.constVoidPtr, [], fname,
                      [wrldParam, idParam],
                      CL.mkBlock[
                          wrldDcl,
                          CL.mkIfThenElse(
                            CL.mkBinOp(
                              CL.mkDispatch(strands, "validIndex", [idV]),
                              CL.#&&,
                              CL.mkDispatch(strands, "isAlive", [idV])),
                            CL.mkReturn(SOME(getFn(ty, CL.mkIndirect(state, TSV.qname sv)))),
                            CL.mkReturn(SOME(CL.mkVar "nullptr")))
                        ])
                val dcls = if APITypes.hasDynamicSize ty
                      then mkFunc getSize :: dcls
                      else dcls
                in
                  mkFunc dataPtr :: dcls
                end
          in
            getFns
          end

  (* Generate the set function for a state variable.  The basic form is:
   *
   *    extern "C" bool NS_STRAND_set_VAR (NS_world_t *wrld, uint32_t id, void *value)
   *    {
   *        NS::world *w = reinterpret_cast<NS::world *>(wrld);
   *        if (w->_strands.validIndex(id) && w->_strands.isAlive(id)) {
   *            TY *out = reinterpret_cast<TY *>(outPtr);
   *            w->_strands.XXX_state(id)->VAR = *value;
   *            return false;
   *        }
   *        else {
   *            return true;
   *        }
   *    }
   *
   * where
   *    NS      - the name space
   *    STRAND  - the name of the strand type
   *    VAR     - the name of the variable
   *    TY      - the C type for representing the variable
   *    XXX     - either "in" or "local", depending on whether the state variable is
   *              shared or not.
   *)
    fun mkSetFn (env, strandName) = let
          val spec = Env.target env
          val ns = #namespace spec
          val wrldParam = CL.PARAM([], GenLib.worldTy spec, "wrld")
          val idParam = CL.PARAM([], CL.uint32, "id")
          val valParam = CL.PARAM([], CL.voidPtr, "value")
          val wrldPtrTy = CL.T_Ptr(CL.T_Named(ns ^ "::world"))
          val wrldDcl = CL.mkDeclInit(wrldPtrTy, "w",
                CL.mkReinterpretCast(wrldPtrTy, CL.mkVar "wrld"))
          val strands = CL.mkIndirect(CL.mkVar "w", "_strands")
          val idV = CL.mkVar "id"
          fun getFn sv = let
                val fname = concat[ns, "_", strandName, "_get_", TSV.name sv]
                val state = CL.mkDispatch(
                      strands,
                      if TSV.isShared sv then "in_state" else "local_state",
                      [CL.mkVar "id"])
                in
                  CL.D_Func(
                    externC, CL.boolTy, [], fname,
                    [wrldParam, idParam, valParam],
                    CL.mkBlock[
                        wrldDcl,
                        CL.mkIfThenElse(
                          CL.mkBinOp(
                            CL.mkDispatch(strands, "validIndex", [idV]),
                            CL.#&&,
                            CL.mkDispatch(strands, "isAlive", [idV])),
                          CL.mkBlock[
                              Util.copyToC {
                                  env = env,
                                  ty = TSV.apiTy sv,
                                  src = CL.E_UnOp(CL.%*, Util.castAPIPtr{
                                      env = env, ty = TSV.apiTy sv, src = CL.mkVar "value"
                                    }),
                                  dst = CL.mkIndirect(state, TSV.qname sv)
                                },
                              CL.mkReturn(SOME(CL.mkVar "false"))
                            ],
                          CL.mkReturn(SOME(CL.mkVar "true")))
                      ])
                end
          in
            getFn
          end

(* TODO: get functions for global variables *)

    fun gen (env, prog) = let
          val IR.Program{strand=IR.Strand{name, state, ...}, ...} = prog
          in
            CL.D_Comment["debugger hooks"] ::
            List.foldr (mkGetFn (env, Atom.toString name)) [] state
          end

  end
