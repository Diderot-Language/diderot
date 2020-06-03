(* tree-to-cxx.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Translate TreeIR to the C++ version of CLang.
 *)


structure TreeToCxx : sig

    val trType : CodeGenEnv.t * TreeTypes.t -> CLang.ty

    val trBlock : CodeGenEnv.t * TreeIR.block -> CLang.stm

    val trWithLocals : CodeGenEnv.t * TreeVar.t list * (CodeGenEnv.t -> CLang.stm list) -> CLang.stm

    val trStms : CodeGenEnv.t * TreeIR.stm list -> CodeGenEnv.t * CLang.stm list

    val trLit : CodeGenEnv.t * Literal.t -> CLang.exp

    val trExp : CodeGenEnv.t * TreeIR.exp -> CLang.exp

  (* returns an expression to access a state variable via the strands array.  The third
   * argument is the ID of the strand.
   *)
    val rvalueStateVarId : CodeGenEnv.t * TreeIR.state_var * CLang.exp -> CLang.exp

  (* translate application of a TreeIR function to a C++ expression *)
    val trApply : CodeGenEnv.t * TreeIR.func * TreeIR.exp list -> CLang.exp

  (* translate an expression to a variable form; return the variable (as an expression)
   * and the (optional) declaration.
   *)
    val trExpToVar : CodeGenEnv.t * CLang.ty * string * TreeIR.exp -> CLang.exp * CLang.stm list

  (* generate code to register an error message (require that a world pointer "wrld" is in scope) *)
    val errorMsgAdd : CodeGenEnv.t * CLang.exp -> CLang.stm

    val trParam : CodeGenEnv.t * TreeIR.var -> CodeGenEnv.t * CLang.param

  (* `loadNrrd (obj, arg, proxy)`
   *    returns code to load data from a nrrd.  The `obj` specifies either an image or dynamic
   *    sequence object, and the `arg` specifies either a nrrd or nrrd file.
   *    The generated code checks the status of the load attempt and will return "true"
   *    (i.e., error) if the load fails.
   *)
    val loadNrrd : CLang.exp * CLang.exp * TreeIR.proxy -> CLang.stm

  end = struct

    structure CL = CLang
    structure IR = TreeIR
    structure Op = TreeOps
    structure Ty = TreeTypes
    structure V = TreeVar
    structure TSV = TreeStateVar
    structure Env = CodeGenEnv
    structure RN = CxxNames

    val trType = TypeToCxx.trType
    val dynseqTy = TypeToCxx.dynseqTy

    fun worldVar env = CL.mkVar(Env.world env)

  (* translate a local variable that occurs in an l-value context *)
    fun lvalueVar (env, x) = CL.mkVar(Env.lookup(env, x))
  (* translate a variable that occurs in an r-value context *)
    fun rvalueVar (env, x) = CL.mkVar(Env.lookup(env, x))

  (* translate a global variable that occurs in an l-value context *)
    fun lvalueGlobalVar (env, x) = CL.mkIndirect(CL.mkVar(Env.global env), TreeGlobalVar.qname x)
  (* translate a global variable that occurs in an r-value context *)
    val rvalueGlobalVar = lvalueGlobalVar

    local
      fun isSharedVar (env, x) = TargetSpec.dualState(Env.target env)
            andalso TSV.isShared x andalso TSV.isVarying x
      fun strands env = CL.mkIndirect(worldVar env, "_strands")
    in
  (* translate a strand state variable that occurs in an l-value context *)
    fun lvalueStateVar (env, x) = let
          val sp = if isSharedVar (env, x)
                then Env.selfOut env
                else Env.selfLocal env
          in
            CL.mkIndirect(CL.mkVar sp, TSV.qname x)
          end
  (* translate a strand state variable that occurs in an r-value context *)
    fun rvalueStateVar (env, x) = let
          val sp = if isSharedVar (env, x)
                then Env.selfIn env
                else Env.selfLocal env
          in
            CL.mkIndirect(CL.mkVar sp, TSV.qname x)
          end
  (* returns an expression to access the (input) state variable for the strand
   * with the given ID.
   *)
    fun rvalueStateVarId (env, x, id) = let
          val m = if isSharedVar (env, x)
                then "id_to_in_state"
                else "id_to_local_state"
          in
            CL.mkIndirect(CL.mkDispatch(strands env, m, [id]), TSV.qname x)
          end
    end (* local *)

  (* generate new variables *)
    val freshVar = CodeGenUtil.freshVar

  (* integer literal expression *)
    fun mkInt (i : int) = CL.mkInt(IntInf.fromInt i)

    val zero = CL.mkInt 0

    fun addrOf e = CL.mkUnOp(CL.%&, e)

  (* make an application of a function from the "std" namespace *)
    fun mkStdApply (f, args) = CL.mkApply("std::" ^ f, args)

  (* make an application of a function from the "diderot" namespace *)
    fun mkDiderotApply (f, args) = CL.mkApply("diderot::" ^ f, args)
    fun mkDiderotCall (f, args) = CL.mkCall("diderot::" ^ f, args)
    fun mkDynseqApply (env, ty, f, args) =
          CL.mkQApply([CL.SC_Type(dynseqTy(env, ty))], f, args)

    fun loadNrrd (obj, arg, NONE) =
          CL.mkIfThen(CL.mkDispatch(obj, "load", [CL.mkVar "wrld", arg]),
            CL.mkReturn(SOME(CL.mkVar "true")))
      | loadNrrd (obj, arg, SOME info) = let
          val vSz = List.foldl Int.* 1 (ImageInfo.voxelShape info)
          val args = List.foldr
                (fn (d, args) => mkInt d :: args)
                  [mkInt vSz] (ImageInfo.sizes info)
          in
            CL.mkBlock[
                CL.mkDeclInit(CL.T_Named "diderot::nrrd_proxy", "proxy",
                  mkDiderotApply ("nrrd_proxy", args)),
                CL.mkIfThen(
                  CL.mkDispatch(obj, "load", [
                      CL.mkVar "wrld", arg, CL.mkAddrOf(CL.mkVar "proxy")
                    ]),
                  CL.mkReturn(SOME(CL.mkVar "true")))
              ]
          end

  (* Translate a TreeIR operator application to a CLang expression *)
    fun trOp (env, rator, cxxArgs) = (case (rator, cxxArgs)
           of (Op.IAdd, [a, b]) => CL.mkBinOp(a, CL.#+, b)
            | (Op.ISub, [a, b]) => CL.mkBinOp(a, CL.#-, b)
            | (Op.IMul, [a, b]) => CL.mkBinOp(a, CL.#*, b)
            | (Op.IDiv, [a, b]) => CL.mkBinOp(a, CL.#/, b)
            | (Op.IMod, [a, b]) => CL.mkBinOp(a, CL.#%, b)
            | (Op.INeg, [a]) => CL.mkUnOp(CL.%-, a)
            | (Op.RAdd, [a, b]) => CL.mkBinOp(a, CL.#+, b)
            | (Op.RSub, [a, b]) => CL.mkBinOp(a, CL.#-, b)
            | (Op.RMul, [a, b]) => CL.mkBinOp(a, CL.#*, b)
            | (Op.RDiv, [a, b]) => CL.mkBinOp(a, CL.#/, b)
            | (Op.RNeg, [a]) => CL.mkUnOp(CL.%-, a)
            | (Op.RClamp, [a, b, c]) => CL.mkApply("clamp", [a, b, c])
            | (Op.RLerp, [a, b, c]) => CL.mkApply("lerp", [a, b, c])
            | (Op.RCeiling, [a]) => mkStdApply("ceil", [a])
            | (Op.RFloor, [a]) => mkStdApply("floor", [a])
            | (Op.RRound, [a]) => mkStdApply("round", [a])
            | (Op.RTrunc, [a]) => mkStdApply("trunc", [a])
            | (Op.RealToInt, [a]) => mkStdApply("lround", [a])
            | (Op.LT ty, [a, b]) => CL.mkBinOp(a, CL.#<, b)
            | (Op.LTE ty, [a, b]) => CL.mkBinOp(a, CL.#<=, b)
            | (Op.EQ ty, [a, b]) => CL.mkBinOp(a, CL.#==, b)
            | (Op.NEQ ty, [a, b]) => CL.mkBinOp(a, CL.#!=, b)
            | (Op.GTE ty, [a, b]) => CL.mkBinOp(a, CL.#>=, b)
            | (Op.GT ty, [a, b]) => CL.mkBinOp(a, CL.#>, b)
            | (Op.BAnd, [a, b]) => CL.mkBinOp(a, CL.#&&, b)
            | (Op.BOr, [a, b]) => CL.mkBinOp(a, CL.#||, b)
            | (Op.BNot, [a]) => CL.mkUnOp(CL.%!, a)
            | (Op.Abs ty, args) => mkStdApply("abs", args)
            | (Op.Max ty, args) => mkStdApply("max", args)
            | (Op.Min ty, args) => mkStdApply("min", args)
            | (Op.VAdd d, [a, b]) => CL.mkBinOp(a, CL.#+, b)
            | (Op.VSub d, [a, b]) => CL.mkBinOp(a, CL.#-, b)
            | (Op.VScale(w, _), [a, b]) => CL.mkApply(RN.vscale w, [a, b])
            | (Op.VMul d, [a, b]) => CL.mkBinOp(a, CL.#*, b)
            | (Op.VNeg d, [a]) => CL.mkUnOp(CL.%-, a)
            | (Op.VSum(w, _), [a]) => CL.mkApply(RN.vsum w, [a])
            | (Op.VDot(w, _), [a, b]) => CL.mkApply(RN.vdot w, [a, b])
            | (Op.VIndex(w, p, i), [a]) => CL.mkSubscript(a, mkInt i)
            | (Op.VCeiling(w, _), [a]) => CL.mkApply(RN.vceiling w, [a])
            | (Op.VFloor(w, _), [a]) => CL.mkApply(RN.vfloor w, [a])
            | (Op.VRound(w, _), [a]) => CL.mkApply(RN.vround w, [a])
            | (Op.VTrunc(w, _), [a]) => CL.mkApply(RN.vtrunc w, [a])
            | (Op.VToInt{wid, ...}, args) => CL.mkApply (RN.vtoi wid, args)
            | (Op.TensorIndex(ty, idxs), [a]) => let
                val dd = (case ty
                       of Ty.TensorTy(_::dd) => dd
                        | Ty.TensorRefTy(_::dd) => dd
                        | _ => raise Fail "bogus type for TensorIndex"
                      (* end case *))
              (* dimensions/indices are slowest to fastest *)
                fun index ([], [i], acc) = acc + i
                  | index (d::dd, i::ii, acc) = index (dd, ii, d * (acc + i))
                in
                  CL.mkSubscript(a, mkInt(index (dd, idxs, 0)))
                end
            | (Op.ProjectLast(ty, idxs), [a]) => let
                val dd = (case ty
                       of Ty.TensorTy(_::dd) => dd
                        | Ty.TensorRefTy(_::dd) => dd
                        | _ => raise Fail "bogus type for ProjectLast"
                      (* end case *))
              (* dimensions/indices are slowest to fastest *)
                fun index ([], [], acc) = acc
                  | index (d::dd, i::ii, acc) = index (dd, ii, d * (acc + i))
                in
                  CL.mkDispatch(a, "last", [mkInt(index (dd, idxs, 0))])
                end
              (* NOTE: since C++ will do the coercion automatically, we don't really need
               * to generate the constructor application for TensorRef!
               *)
            | (Op.TensorRef shp, [a]) => CL.mkCons(RN.tensorRefTy shp, [a])
            | (Op.Select(Ty.TupleTy tys, i), [a]) =>
                CL.mkSelect(a, "tpl_" ^ Int.toString i)
(* QUESTION: if this is a sequence of tensors, will there be an extra copy? *)
            | (Op.Subscript ty, [a, b]) => CL.mkSubscript(a, b)
            | (Op.MkDynamic(ty, n), [a]) =>
                CL.mkCons(dynseqTy(env, ty), [mkInt n, CL.mkDispatch(a, "data", [])])
(* FIXME: eventually we should do some kind of liveness analysis to enable in situ operations *)
            | (Op.Prepend(seqTy, itemTy), [a, b]) => (case itemTy
                 of Ty.TensorRefTy _ => (* copy tensor data *)
                      CL.mkDispatch (b, "prepend", [CL.mkDispatch(a, "base", [])])
                  | _ => mkDynseqApply (env, seqTy, "prepend", [a, b])
                (* end case *))
            | (Op.Append(seqTy, itemTy), [a, b]) => (case itemTy
                 of Ty.TensorRefTy _ => (* copy tensor data *)
                      CL.mkDispatch (a, "append", [CL.mkDispatch(b, "base", [])])
                  | _ => mkDynseqApply (env, seqTy, "append", [a, b])
                (* end case *))
            | (Op.Concat ty, [a, b]) => mkDynseqApply (env, ty, "concat", [a, b])
            | (Op.Range, [a, b]) => CL.mkCons(dynseqTy(env, Ty.IntTy), [a, b])
            | (Op.Length ty, [a]) => CL.mkDispatch(a, "length", [])
            | (Op.SphereQuery(dim, Ty.StrandIdTy s), args) => let
              (* The sphere_query needs the self pointer to filter out the invoking strand.
               * In the dual-state case, we don't have a pointer of the right type, but
               * we can cast the local-state pointer, since it also points to the beginning
               * of the strand.
               *)
                val self = if TargetSpec.dualState(Env.target env)
                      then CL.mkReinterpretCast(
                        CL.T_Ptr(RN.strandTy(Atom.toString s)),
                        CL.mkVar(Env.selfLocal env))
                      else CL.mkVar(Env.selfIn env)
                in
                  CL.mkApply("sphere_query", CL.mkVar(Env.world env) :: self :: args)
                end
            | (Op.Sqrt, [a]) => mkStdApply("sqrt", [a])
            | (Op.Cos, [a]) => mkStdApply("cos", [a])
            | (Op.ArcCos, [a]) => mkStdApply("acos", [a])
            | (Op.Sin, [a]) => mkStdApply("sin", [a])
            | (Op.ArcSin, [a]) => mkStdApply("asin", [a])
            | (Op.Tan, [a]) => mkStdApply("tan", [a])
            | (Op.ArcTan, [a]) => mkStdApply("atan", [a])
            | (Op.Exp, [a]) => mkStdApply("exp", [a])
            | (Op.Sign, [a]) =>  mkDiderotApply("sign", [a])
            | (Op.IntToReal, [a]) => CL.mkStaticCast(Env.realTy env, a)
            | (Op.NumStrands StrandSets.ACTIVE, []) =>
                CL.mkDispatch(RN.strandArray env, "num_active", [])
            | (Op.NumStrands StrandSets.ALL, []) =>
                CL.mkDispatch(RN.strandArray env, "num_alive", [])
            | (Op.NumStrands StrandSets.STABLE, []) =>
                CL.mkDispatch(RN.strandArray env, "num_stable", [])
            | (Op.Transform info, [img]) => CL.mkApply("world2image", [img])
            | (Op.Translate info, [img]) => CL.mkApply("translate", [img])
            | (Op.BaseAddress info, [img]) => CL.mkDispatch(img, "base_addr", [])
            | (Op.ControlIndex(info, ctl, d), [img, idx]) =>
                CL.mkDispatch(img, IndexCtl.toString ctl, [mkInt d, idx])
            | (Op.LoadVoxel info, [addr, offp]) => let
                val voxel = CL.mkSubscript(addr, offp)
                in
                  if RawTypes.same(ImageInfo.sampleTy info, Env.rawRealTy env)
                    then voxel
                    else CL.mkStaticCast(Env.realTy env, voxel)
                end
            | (Op.Inside(layout, _, s), args) =>
                CL.mkApply (RN.inside(#wid layout, s), args)
            | (Op.IndexInside(info, s), [pos, img]) => CL.mkDispatch(img, "inside", [pos, mkInt s])
            | (Op.ImageDim(info, i), [img]) => CL.mkDispatch(img, "size", [mkInt i])
            | (Op.MathFn f, args) => mkStdApply(MathFns.toString f, args)
            | (Op.IfWrap, [a, b, c])    => CL.mkApply("IfWrap", [a,b,c])
            | _ => raise Fail(concat[
                   "unknown or incorrect operator ", Op.toString rator
                 ])
          (* end case *))

    fun trLit (env, lit) = (case lit
           of Literal.Int n => CL.mkIntTy(n, Env.intTy env)
            | Literal.Bool b => CL.mkBool b
            | Literal.Real f => CL.mkFlt(f, Env.realTy env)
            | Literal.String s => CL.mkStr s
          (* end case *))

    fun trExp (env, e) = (case e
           of IR.E_Global x => rvalueGlobalVar (env, x)
            | IR.E_State(NONE, x) => rvalueStateVar (env, x)
            | IR.E_State(SOME e, x) => rvalueStateVarId (env, x, trExp(env, e))
            | IR.E_Var x => rvalueVar (env, x)
            | IR.E_Lit lit => trLit (env, lit)
            | IR.E_Op(rator, args) => trOp (env, rator, trExps(env, args))
            | IR.E_Apply(f, args) => trApply (env, f, args)
            | IR.E_Vec(w, pw, args) => CL.mkApply(RN.vcons w, trExps (env, args))
            | IR.E_Cons(args, Ty.TensorTy shape) => raise Fail "unexpected E_Cons"
            | IR.E_Seq(args, ty) => raise Fail "unexpected E_Seq"
            | IR.E_Pack(layout, args) => raise Fail "unexpected E_Pack"
(* FIXME: check if e is aligned and use "vload_aligned" in that case *)
            | IR.E_VLoad(layout, e, i) =>
                CL.mkApply(RN.vload(Ty.nthWidth(layout, i)),
                  [CL.mkDispatch(trExp(env, e), "addr", [mkInt(Ty.offsetOf(layout, i))])])
            | _ => raise Fail "trExp"
          (* end case *))

    and trExps (env, exps) = List.map (fn exp => trExp(env, exp)) exps

    and trApply (env, f, args) = let
          val args = trExps (env, args)
          val args = if TreeFunc.hasGlobals f
                then CL.mkVar(Env.global env) :: args
                else args
          val args = if TreeFunc.needsWorld f
                then CL.mkVar(Env.world env) :: args
                else args
          in
            CL.mkApply(TreeFunc.qname f, args)
          end

(* QUESTION: not sure that we need this function? *)
    fun trExpToVar (env, ty, name, exp) = (case trExp (env, exp)
           of e as CL.E_Var _ => (e, [])
            | e => let
                val x = freshVar name
                in
                  (CL.mkVar x, [CL.mkDeclInit(ty, x, e)])
                end
          (* end case *))

(* FIXME: trAssign and trDecl do the same analysis of the rhs; we should factor that out *)
  (* translate the assignment `lhs = rhs` and add the code to the stms list.  Note that the
   * stms list is reverse order!
   *)
    fun trAssign env = let
          fun tr (lhs, rhs, stms) = (case rhs
                 of IR.E_Op(Op.TensorCopy shp, [a]) => CL.mkAssign(lhs, trExp (env, a)) :: stms
                  | IR.E_Op(Op.EigenVals2x2, [a]) =>
                      CL.mkCall("eigenvals", [trExp (env, a), lhs]) :: stms
                  | IR.E_Op(Op.EigenVals3x3, [a]) =>
                      CL.mkCall("eigenvals", [trExp (env, a), lhs]) :: stms
                  | IR.E_Pack({wid, ...}, args) =>
                      CL.mkCall (RN.vpack wid, lhs :: List.map (fn e => trExp(env, e)) args) :: stms
                  | IR.E_Cons(args, _) => let
                      fun trArg (i, arg, stms) = tr (CL.mkSubscript(lhs, mkInt i), arg, stms)
                      in
                        List.foldli trArg stms args
                      end
                  | IR.E_Seq(args, Ty.SeqTy(_, NONE)) => (* dynamic sequence *)
                      CL.mkAssign (lhs, CL.mkArray(trExps (env, args))) :: stms
                  | IR.E_Seq(args, _) => let (* fixed-size sequence *)
                      fun trArg (i, arg, stms) = tr (CL.mkSubscript(lhs, mkInt i), arg, stms)
                      in
                        List.foldli trArg stms args
                      end
                  | _ => CL.mkAssign(lhs, trExp (env, rhs)) :: stms
                (* end case *))
          in
            tr
          end

    fun trDecl (env, ty, lhs, rhs, stms) = let
          fun trArgs args = CL.mkDecl(
                ty, lhs, SOME(CL.I_Exps(List.map (fn arg => CL.I_Exp(trExp (env, arg))) args)))
          in
            case rhs
             of IR.E_Op(Op.TensorCopy shp, [a]) => CL.mkDeclInit(ty, lhs, trExp(env, a)) :: stms
              | IR.E_Op(Op.EigenVals2x2, [a]) =>
                  CL.mkCall("eigenvals", [trExp (env, a), CL.mkVar lhs]) ::
                  CL.mkDecl(ty, lhs, NONE) :: stms
              | IR.E_Op(Op.EigenVals3x3, [a]) =>
                  CL.mkCall("eigenvals", [trExp (env, a), CL.mkVar lhs]) ::
                  CL.mkDecl(ty, lhs, NONE) :: stms
              | IR.E_Pack({wid, ...}, args) =>
                  CL.mkCall (RN.vpack wid, CL.mkVar lhs :: List.map (fn e => trExp(env, e)) args) ::
                  CL.mkDecl(ty, lhs, NONE) :: stms
              | IR.E_Cons(args, _) => let
                  val init = List.map (fn arg => CL.I_Exp(trExp(env, arg))) args
                  in
                    CL.mkDecl(ty, lhs, SOME(CL.I_Exps init)) :: stms
                  end
              | IR.E_Seq(args, Ty.SeqTy(_, NONE)) => (* dynamic sequence *)
                  CL.mkDecl(ty, lhs, SOME(CL.I_Exps(map CL.I_Exp (trExps (env, args))))) :: stms
              | IR.E_Seq(args, _) => let
                  val dcl = CL.mkDecl(ty, lhs, NONE)
                  val trAssign = trAssign env
                  fun trArg (i, arg, stms) =
                        trAssign (CL.mkSubscript(CL.mkVar lhs, mkInt i), arg, stms)
                  in
                    List.foldli trArg (dcl :: stms) args
                  end
              | _ => CL.mkDeclInit(ty, lhs, trExp (env, rhs)) :: stms
            (* end case *)
          end

    fun trMultiAssign (env, lhs, IR.E_Op(rator, args)) = (case (lhs, rator, args)
           of ([vals, vecs], Op.EigenVecs2x2, [exp]) =>
                CL.mkCall("eigenvecs", [trExp (env, exp), vals, vecs])
            | ([vals, vecs], Op.EigenVecs3x3, [exp]) =>
                CL.mkCall("eigenvecs", [trExp (env, exp), vals, vecs])
            | _ => raise Fail "bogus multi-assignment"
          (* end case *))
      | trMultiAssign (env, lhs, rhs) = raise Fail "bogus multi-assignment"

    fun trPrintStm (env, tys, args) = let
          fun mkExp (lhs, [], []) = CL.mkBinOp(lhs, CL.#<<, CL.mkVar "std::flush")
            | mkExp (lhs, ty::tys, e::es) = let
              (* if necessary, wrap the argument so that the correct "<<" instance is used *)
                val e = (case ty
                       of Ty.TensorTy shape => CL.mkApply(RN.tensorRefStruct shape, [e])
                        | _ => e
                      (* end case *))
                in
                  mkExp (CL.mkBinOp(lhs, CL.#<<, e), tys, es)
                end
            | mkExp _ = raise Fail "trPrintStm: arity mismatch"
          val wrld = worldVar env
          val outS = CL.mkIndirectDispatch(wrld, "print", [])
          val stm = CL.mkExpStm (mkExp (outS, tys, args))
          in
            if TargetSpec.isParallel(Env.target env)
              then let
                val lock = CL.mkAddrOf(CL.mkIndirect(CL.mkIndirect(wrld, "_sched"), "_prLock"))
                in [
                (* NOTE: statements will be reversed! *)
                  CL.mkCall("pthread_mutex_unlock", [lock]),
                  stm,
                  CL.mkCall("pthread_mutex_lock", [lock])
                ] end
              else [stm]
          end

    fun trStms (env, stms : TreeIR.stm list) = let
          fun trStm (stm, (env, stms : CL.stm list)) = (case stm
                 of IR.S_Comment text => (env, CL.mkComment text :: stms)
                  | IR.S_Assign(true, x, exp) => let
                      val ty = trType (env, V.ty x)
                      val x' = V.name x
                      val env = Env.insert (env, x, x')
                      in
                        (env, trDecl (env, ty, x', exp, stms))
                      end
                  | IR.S_Assign(false, x, exp) =>
                      (env, trAssign env (lvalueVar (env, x), exp, stms))
                  | IR.S_MAssign(xs, exp) =>
                      (env, trMultiAssign (env, List.map (fn x => lvalueVar (env, x)) xs, exp) :: stms)
                  | IR.S_GAssign(x, exp) =>
                      (env, trAssign env (lvalueGlobalVar (env, x), exp, stms))
                  | IR.S_IfThen(cond, thenBlk) =>
                      (env, CL.mkIfThen(trExp(env, cond), trBlock(env, thenBlk)) :: stms)
                  | IR.S_IfThenElse(cond, thenBlk, elseBlk) => let
                      val stm = CL.mkIfThenElse(trExp(env, cond),
                            trBlock(env, thenBlk),
                            trBlock(env, elseBlk))
                      in
                        (env, stm :: stms)
                      end
                  | IR.S_For(x, lo, hi, blk) => let
                      val x' = V.name x
                      val env' = Env.insert (env, x, x')
                      val (hi', hiInit) = if CodeGenUtil.isSimple hi
                            then (trExp(env, hi), [])
                            else let
                              val hi' = freshVar "hi"
                              in
                                (CL.mkVar hi', [CL.mkDeclInit(CL.int32, hi', trExp(env, hi))])
                              end
                      val loop = CL.mkFor(
                            CL.int32, [( x', trExp(env, lo))],
                            CL.mkBinOp(CL.mkVar x', CL.#<=, hi'),
                            [CL.mkUnOp(CL.%++, CL.mkVar x')],
                            trBlock (env', blk))
                      in
                        (env, hiInit @ loop :: stms)
                      end
                  | IR.S_Foreach(x, e, blk) => let
                      val seq = trExp(env, e)
                      val x' = V.name x
                      val env' = Env.insert (env, x, x')
                      val blk' = trBlock (env', blk)
                      val loop = (case TreeTypeOf.exp e
                             of Ty.SeqTy(_, SOME n) => let
                                  val ix = freshVar "ix"
                                  val defx = CL.mkDeclInit(CL.T_Named "auto", x',
                                        CL.mkSubscript(seq, CL.mkVar ix))
                                  in
                                    CL.mkFor(
                                      CL.int32, [(ix, CL.mkInt 0)],
                                      CL.mkBinOp(CL.mkVar ix, CL.#<, mkInt n),
                                      [CL.mkUnOp(CL.%++, CL.mkVar ix)],
                                      CL.prependStm(defx, blk'))
                                  end
                              | Ty.SeqTy(_, NONE) => let
                                  val ix = freshVar "it"
                                  val defx = CL.mkDeclInit(CL.T_Named "auto", x',
                                        CL.mkUnOp(CL.%*, CL.mkVar ix))
                                  in
                                    CL.mkFor(
                                      CL.T_Named "auto", [(ix, CL.mkDispatch(seq, "cbegin", []))],
                                      CL.mkBinOp(CL.mkVar ix, CL.#!=, CL.mkDispatch(seq, "cend", [])),
                                      [CL.mkUnOp(CL.%++, CL.mkVar ix)],
                                      CL.prependStm(defx, blk'))
                                  end
                              | _ => raise Fail "impossible"
                            (* end case *))
                      in
                        (env, loop :: stms)
                      end
                  | IR.S_MapReduce(mrs, src) => Env.mapReduceCB (env, mrs, src, stms)
                  | IR.S_New(strand, args) => let
                      val args = worldVar env :: List.map (fn e => trExp(env, e)) args
                      val args = if TargetSpec.isParallel(Env.target env)
                            then CL.mkVar RN.workerVar :: args
                            else args
                      val stm = CL.mkCall(Atom.toString strand ^ "_new", args)
                      in
                        (env, stm :: stms)
                      end
                  | IR.S_Save(x, exp) =>
                    (env, trAssign env (lvalueStateVar(env, x), exp, stms))
                  | IR.S_LoadNrrd(lhs, ty, nrrd, proxy) => let
                      val stm = loadNrrd (lvalueVar (env, lhs), CL.mkStr nrrd, proxy)
                      in
                        (env, stm :: stms)
                      end
                  | IR.S_Input(_, _, _, NONE) => (env, stms)
                  | IR.S_Input(gv, name, _, SOME dflt) =>
                      (env, CL.mkAssign(lvalueGlobalVar (env, gv), trExp(env, dflt)) :: stms)
                  | IR.S_InputNrrd _ => (env, stms)
                  | IR.S_StabilizeAll => let
                      val stm = CL.mkExpStm(CL.mkIndirectDispatch(worldVar env, "stabilize_all", []))
                      in
                        (env, stm::stms)
                      end
                  | IR.S_KillAll => let
                      val stm = CL.mkExpStm(CL.mkIndirectDispatch(worldVar env, "kill_all", []))
                      in
                        (env, stm::stms)
                      end
                  | IR.S_Print(tys, args) => let
                      val args = List.map (fn e => trExp(env, e)) args
                      in
                        (env, trPrintStm(env, tys, args) @ stms)
                      end
                  | IR.S_Return NONE => (env, CL.mkReturn NONE :: stms)
                  | IR.S_Return(SOME e) => (env, CL.mkReturn(SOME(trExp(env, e))) :: stms)
                  | IR.S_Active => (env, CL.mkReturn(SOME(CL.mkVar "diderot::kActive")) :: stms)
                  | IR.S_Stabilize => (env, CL.mkReturn(SOME(CL.mkVar "diderot::kStabilize")) :: stms)
                  | IR.S_Die => (env, CL.mkReturn(SOME(CL.mkVar "diderot::kDie")) :: stms)
                (* end case *))
          val (env, stms) = List.foldl trStm (env, []) stms
          in
            (env, List.rev stms)
          end

    and trBlock (env, IR.Block{locals, body}) = let
          fun trLocal (x, (env, dcls)) = let
                val x' = V.name x
                val dcl = CL.mkDecl(trType(env, V.ty x), x', NONE)
                in
                  (Env.insert(env, x, x'), dcl :: dcls)
                end
          val (env, dcls) = List.foldl trLocal (env, []) (!locals)
          val (_, stms) = trStms (env, body)
          in
            CL.mkBlock (dcls @ stms)
          end

    and trWithLocals (env, locals, trBody) = let
          fun trLocal (x, (env, dcls)) = let
                val x' = V.name x
                val dcl = CL.mkDecl(trType(env, V.ty x), x', NONE)
                in
                  (Env.insert(env, x, x'), dcl :: dcls)
                end
          val (env, dcls) = List.foldl trLocal (env, []) locals
          in
            CL.mkBlock (dcls @ trBody env)
          end

    fun errorMsgAdd (env, msg) =
          CL.mkCall("biffMsgAdd", [CL.mkIndirect(worldVar env, "_errors"), msg])

    fun trParam (env, x)= let
          val x' = V.name x
          in
            (Env.insert (env, x, x'), CL.PARAM([], trType(env, V.ty x), x'))
          end

  end
