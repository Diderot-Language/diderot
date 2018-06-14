(* low-to-tree.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure LowToTree : sig

    val translate : LowIR.program * TreeIR.target_info -> TreeIR.program

  end = struct

    structure IR = LowIR
    structure V = LowIR.Var
    structure Ty = LowTypes
    structure Op = LowOps
    structure GV = IR.GlobalVar
    structure SV = IR.StateVar
    structure T = TreeIR
    structure TTy = TreeTypes
    structure TOp = TreeOps
    structure TV = TreeVar
    structure TGV = TreeGlobalVar
    structure TSV = TreeStateVar
    structure U = Util

  (* associate Tree IL globals variables with Low IL variables using properties *)
    local
      val {setFn, peekFn, ...} =
            GV.newProp (fn x => raise Fail(concat["getGlobalVar(", GV.uniqueName x, ")"]))
    in
      fun mkGlobalVar x = (case peekFn x
             of NONE => let
                    val x' = TGV.new {
                            name = GV.name x,
                            ty = U.trType (GV.ty x),
                            input = GV.isInput x,
                            output = false, (* FIXME: change once we support output globals *)
                            varying = GV.isVarying x,
                            apiTy = if GV.isInput x
                              then SOME(U.toAPIType (GV.ty x))
                              else NONE
                          }
                    in
                      setFn (x, x');
                      x'
                    end
              | SOME x' => x'
            (* end case *))
    end

  (* associate Tree IL state variables with Low IL variables using properties *)
    local
      fun mkStateVar x = TSV.new {
              name = SV.name x,
              ty = U.trType (SV.ty x),
              output = SV.isOutput x,
              varying = SV.isVarying x,
              shared = SV.isShared x,
              apiTy = U.toAPIType (SV.ty x)
            }
    in
    val {getFn = getStateVar, ...} = SV.newProp mkStateVar
    end

  (* associate Tree IL function variables with Low IL variables using properties *)
    local
      val {setFn, getFn : IR.func -> TreeFunc.t, ...} =
            IR.Func.newProp (fn f => raise Fail("no binding for function " ^ IR.Func.toString f))
    in
    val getFuncVar = getFn
    fun mkFuncVar (f, needsWrld, hasGlobs) = let
          val (resTy, paramTys) = IR.Func.ty f
(* QUESTION: what about vector result/arguments? *)
          val f' = TreeFunc.new (
                IR.Func.name f, U.trType resTy, List.map U.trParamType paramTys,
                needsWrld, hasGlobs, Flatten.isMapFunc f)
          in
            setFn (f, f'); f'
          end
    end

  (* for variables that are in an equivalence class (see UnifyVars), we use a single
   * TreeIR variable (or vector of variables) to represent them.
   *)
    datatype eq_var_rep = NOEQ | VAR of TV.t | VEC of TV.t list

    fun eqToString NOEQ = "NOEQ"
      | eqToString (VAR x) = concat["VAR(", TV.toString x, ")"]
      | eqToString (VEC xs) = concat["VEC[", String.concatWithMap "," TV.toString xs, "]"]

    local
      val {peekFn : IR.var -> eq_var_rep option, setFn, ...} =
            V.newProp (fn x => raise Fail(V.toString x))

      fun repOf (env, x) = (case peekFn x
             of SOME b => b
              | NONE => let
                  val rep = (case V.ty x
                         of Ty.TensorTy[d] => VEC(U.newVectorVars(Env.layoutVec env d))
                          | ty => if AssignTypes.isMemoryVar x
                              then VAR(U.newMemVar x)
                              else VAR(U.newLocalVar x)
                        (* end case *))
                  in
                    setFn (x, rep);
                    rep
                  end
            (* end case *))
    in

    fun eqClassRepOf (env, x) = (case UnifyVars.eqClassOf x
           of SOME x' => repOf (env, x')
            | NONE => NOEQ
          (* end case *))

    fun useVar env = let
          val useV = Env.useVar env
          in
            fn x => (case UnifyVars.eqClassOf x
                  of SOME x' => (case repOf (env, x')
                       of VAR x => Env.TREE(T.E_Var x)
                        | VEC xs => let
                            val Ty.TensorTy[d] = V.ty x
                            in
                              Env.VEC(Env.layoutVec env d, List.map T.E_Var xs)
                            end
                        | NOEQ => raise Fail "impossible"
                      (* end case *))
                   | NONE => useV x
                 (* end case *))
          end
    end

    fun mkBlock stms = T.Block{locals = ref [], body = stms}
    fun mkIf (x, stms, []) = T.S_IfThen(x, mkBlock stms)
      | mkIf (x, stms1, stms2) = T.S_IfThenElse(x, mkBlock stms1, mkBlock stms2)
    fun mkAssign (x, e) = T.S_Assign(false, x, e)
    fun mkDefn (x, e) = T.S_Assign(true, x, e)
    val zero = T.E_Lit(Literal.Real(RealLit.zero false))

  (* turn an expression of type TensorTy to one of TensorTyRef *)
    fun mkRef e = (case TreeTypeOf.exp e
           of TTy.TensorTy(shp as _::_) => T.E_Op(TOp.TensorRef shp, [e])
            | _ => e
         (* end case *))

  (* turn an expression of type TensorRefTy to one of TensorTy *)
    fun mkDeref e = (case TreeTypeOf.exp e
           of TTy.TensorRefTy(shp as _::_) => T.E_Op(TOp.TensorCopy shp, [e])
            | _ => e
         (* end case *))

    fun cvtScalarTy Ty.BoolTy = TTy.BoolTy
      | cvtScalarTy Ty.IntTy = TTy.IntTy
      | cvtScalarTy Ty.StringTy = TTy.StringTy
      | cvtScalarTy (Ty.TensorTy[]) = TTy.realTy
      | cvtScalarTy ty = raise Fail(concat["cvtScalarTy(", Ty.toString ty, ")"])

  (* define a new local variable and bind x to it in the environment. *)
    fun newLocal (env, x) = let
          val x' = if AssignTypes.isMemoryVar x then U.newMemVar x else U.newLocalVar x
          in
            Env.bindSimple (env, x, T.E_Var x');
            x'
          end

  (* define a new local variable and bind x to it in the environment. *)
    fun newMemLocal (env, x) = let
          val x' = U.newMemVar x
          in
            Env.bindSimple (env, x, T.E_Var x');
            x'
          end

  (* get a variable's binding as a single argument expression.  This means that
   * if x is bound to a vector of expressions, then we need to pack it.  Since
   * Pack gets translated to an array, we need to create a new temp.
   *)
    fun singleArg env (x, stms) = (case useVar env x
           of Env.RHS(ty, e) => let
                val tmp = U.newTempVar ("t", ty)
                in
                  (T.E_Var tmp, mkDefn(tmp, e) :: stms)
                end
            | Env.TREE e => (e, stms)
            | Env.VEC(layout, es) => let
                val tmp = U.newTempVar("_arg", TTy.TensorTy[#wid layout])
                in
                  (T.E_Var tmp, mkDefn(tmp, T.E_Pack(layout, es)) :: stms)
                end
          (* end case *))

    fun singleArgs (env, args) = let
          val singleArg = singleArg env
          fun doArg (arg, (es, stms)) = let
                val (e', stms') = singleArg (arg, stms)
                in
                  (e'::es, stms')
                end
          in
            List.foldr doArg ([], []) args
          end

    fun simpleArg env = let
          val useVar = useVar env
          in
            fn (x, stms) => (case useVar x
               of Env.RHS(ty, e) => let
                    val tmp = U.newTempVar ("t", ty)
                    in
                      (T.E_Var tmp, mkDefn(tmp, e)::stms)
                    end
                | Env.TREE e => (e, stms)
                | _ => raise Fail("expected simple binding for " ^ IR.Var.toString x)
              (* end case *))
          end

  (* translation for simple operations (e.g., scalar operations) *)
    fun simpleArgs (env, args) = let
          val simpleArg = simpleArg env
          fun doArg (arg, (es, stms)) = let
                val (e', stms') = simpleArg (arg, stms)
                in
                  (e'::es, stms')
                end
          in
            List.foldr doArg ([], []) args
          end

    fun vectorArg (env, x) = let
          fun expToArg (e, stms) = (case V.ty x
                 of Ty.TensorTy[d] => let
                      val layout = Env.layoutVec env d
                      val e = mkRef e
                      val es = List.tabulate (
                            List.length(#pieces layout),
                            fn i => T.E_VLoad(layout, e, i))
                      in
                        (layout, es, stms)
                      end
                  | ty => raise Fail(concat[
                        "expected ", V.toString x, " : TensorTy[_], but found " ^ Ty.toString ty
                      ])
                (* end case *))
          in
            case useVar env x
             of Env.RHS(ty, e) =>  let
                  val tmp = U.newTempVar ("t", ty)
                  in
                    expToArg (T.E_Var tmp, [mkDefn(tmp, e)])
                  end
              | Env.TREE e => expToArg(e, [])
              | Env.VEC(layout, es) => (layout, es, [])
            (* end case *)
          end

  (* convert a list of LowIR variables, each of which are mapped
   * to lists of of vector expressions, to a list of list of expressions
   *)
    fun vectorArgs (env, []) = raise Fail "unexpected empty argument list"
      | vectorArgs (env, x::xs) = let
          val (layout, exps, stms) = vectorArg (env, x)
          fun doArg (x, (argLists, stms)) = let
                val (_, exps, stms') = vectorArg (env, x)
                in
                  (ListPair.mapEq (op ::) (exps, argLists), stms'@stms)
                end
(* FIXME: this map is possibly just a bandaid for the real problem *)
          val es = List.map (fn e => [e]) exps
          val (argLists, stms) = List.foldl doArg (es, stms) xs
          in
            (layout, List.map List.rev argLists, List.rev stms)
          end

  (* translation for simple vector operations *)
    fun trVecOp (env, rator, args) = let
          val useVar = useVar env
          fun doArg (x, (es, stms)) = (case useVar x
                 of Env.RHS(ty, e) => let
                      val tmp = U.newTempVar ("t", ty)
                      in
                        (T.E_Var tmp :: es, mkDefn(tmp, e)::stms)
                      end
                  | Env.TREE e => (e::es, stms)
                  | _ => raise Fail("expected simple binding for " ^ IR.Var.toString x)
                (* end case *))
          val (args, stms) = List.foldr doArg ([], []) args
          in
            (Env.TREE(T.E_Op(rator, args)), stms)
          end

    fun trOp (env, srcRator, args) = let
          fun bindTREE rator = let
                val (args, stms) = simpleArgs (env, args)
                in
                  (Env.TREE(T.E_Op(rator, args)), stms)
                end
          fun bindTREE' rator = let
                val (args, stms) = singleArgs (env, args)
                in
                  (Env.TREE(T.E_Op(rator, args)), stms)
                end
          fun bindRHS (ty, rator) = let
                val (args, stms) = simpleArgs (env, args)
                in
                  (Env.RHS(ty, T.E_Op(rator, args)), stms)
                end
          fun bindVOp rator = let
                val (layout, argss, stms) = vectorArgs (env, args)
                fun mkArgs (w, [p], [args]) = [T.E_Op(rator(w, p), args)]
                  | mkArgs (w, p::ps, args::argss) =
                      T.E_Op(rator(p, p), args) :: mkArgs (w-p, ps, argss)
                  | mkArgs _ = raise Fail "bindVOp: arity mismatch"
                val exps = mkArgs (#wid layout, #pieces layout, argss)
                in
                  (Env.VEC(layout, exps), stms)
                end
          fun mkArgs f ({wid, pieces, ...} : TTy.vec_layout, args) = let
                fun mk (w, [p], [x]) = [T.E_Op(f(w, p, x))]
                  | mk (w, p::ps, x::xs) = T.E_Op(f(p, p, x)) :: mk (w-p, ps, xs)
                  | mk _ = raise Fail "mkArgs: arity mismatch"
                in
                  mk (wid, pieces, args)
                end
          in
            case srcRator
             of Op.IAdd => bindTREE TOp.IAdd
              | Op.ISub => bindTREE TOp.ISub
              | Op.IMul => bindTREE TOp.IMul
              | Op.IDiv => bindTREE TOp.IDiv
              | Op.IMod => bindTREE TOp.IMod
              | Op.INeg => bindTREE TOp.INeg
(* QUESTION: should we just use VAdd 1, etc ?*)
              | Op.RAdd => bindTREE TOp.RAdd
              | Op.RSub => bindTREE TOp.RSub
              | Op.RMul => bindTREE TOp.RMul
              | Op.RDiv => bindTREE TOp.RDiv
              | Op.RNeg => bindTREE TOp.RNeg
              | Op.LT ty => bindTREE (TOp.LT (cvtScalarTy ty))
              | Op.LTE ty => bindTREE (TOp.LTE (cvtScalarTy ty))
              | Op.EQ ty => bindTREE (TOp.EQ (cvtScalarTy ty))
              | Op.NEQ ty => bindTREE (TOp.NEQ (cvtScalarTy ty))
              | Op.GT ty => bindTREE (TOp.GT (cvtScalarTy ty))
              | Op.GTE ty => bindTREE (TOp.GTE (cvtScalarTy ty))
              | Op.BAnd => bindTREE TOp.BAnd
              | Op.BOr => bindTREE TOp.BOr
              | Op.BNot => bindTREE TOp.BNot
              | Op.Abs ty => bindTREE (TOp.Abs (cvtScalarTy ty))
              | Op.Max ty => bindTREE (TOp.Max (cvtScalarTy ty))
              | Op.Min ty => bindTREE (TOp.Min (cvtScalarTy ty))
              | Op.RClamp => bindTREE TOp.RClamp
              | Op.RLerp => bindTREE TOp.RLerp
              | Op.VAdd _ => bindVOp TOp.VAdd
              | Op.VSub _ => bindVOp TOp.VSub
              | Op.VScale _ => let
                  val [s, v] = args
                  val (s, stms) = simpleArg env (s, [])
                  val (layout, vs, stms') = vectorArg (env, v)
                  val exps = mkArgs (fn (w, p, x) => (TOp.VScale(w, p), [s, x])) (layout, vs)
                  in
                    (Env.VEC(layout, exps), stms@stms')
                  end
              | Op.VMul _ => bindVOp TOp.VMul
              | Op.VNeg _ => bindVOp TOp.VNeg
              | Op.VSum _ => let
                  val [v] = args
                  val (layout, pieces, stms) = vectorArg (env, v)
                  val e::es = mkArgs (fn (w, p, x) => (TOp.VSum(w, p), [x])) (layout, pieces)
                  in
                    (Env.TREE(List.foldr (fn (e, es) => T.E_Op(TOp.RAdd, [e, es])) e es), stms)
                  end
              | Op.VDot _ => let
                  val (layout, argss, stms) = vectorArgs (env, args)
                  fun mkArgs (w, [p], [args]) = [T.E_Op(TOp.VDot(w, p), args)]
                    | mkArgs (w, p::ps, args::argss) =
                        T.E_Op(TOp.VDot(p, p), args) :: mkArgs (w-p, ps, argss)
                    | mkArgs _ = raise Fail "VDot: arity mismatch"
                  val e::es = mkArgs (#wid layout, #pieces layout, argss)
                  in
                    (Env.TREE(List.foldr (fn (e, es) => T.E_Op(TOp.RAdd, [e, es])) e es), stms)
                  end
              | Op.VIndex(_, i) => let
                  val [v] = args
                  val ({wid, pieces, ...}, es, stms) = vectorArg (env, v)
                  fun select (_, wid, [w], [e]) = Env.TREE(T.E_Op(TOp.VIndex(wid, w, i), [e]))
                    | select (i, wid, w::ws, e::es) =
                        if (i < w)
                          then Env.TREE(T.E_Op(TOp.VIndex(w, w, i), [e]))
                          else select (i-w, wid-w, ws, es)
                    | select _ = raise Fail("bogus " ^ Op.toString srcRator)
                  in
                    (select (i, wid, pieces, es), stms)
                  end
              | Op.TensorIndex(ty as Ty.TensorTy[_], [i]) => let
                  val [arg] = args
                  fun mkOp e = Env.TREE(T.E_Op(TOp.TensorIndex(TreeTypeOf.exp e, [i]), [e]))
                  in
                    case useVar env arg
                     of Env.RHS(ty, e) =>  let
                          val tmp = U.newTempVar ("t", ty)
                          in
                            (mkOp(T.E_Var tmp), [mkDefn(tmp, e)])
                          end
                      | Env.TREE e => (mkOp e, [])
                      | Env.VEC(layout, es) => let
                          fun mkOp (w, pw, i, e) =
                                (Env.TREE(T.E_Op(TOp.VIndex(w, pw, i), [e])), [])
                          fun index (idx, w, [pw], [e]) = mkOp (w, pw, idx, e)
                            | index (idx, w, pw::ws, e::es) =
                                if (idx < pw)
                                  then mkOp (pw, pw, idx, e)
                                  else index (idx - pw, w - pw, ws, es)
                            | index _ = raise Fail "inconsistent"
                          in
                            index (i, #wid layout, #pieces layout, es)
                          end
                    (* end case *)
                  end
              | Op.TensorIndex(ty, idxs) => let
                  val ([arg], stms) = simpleArgs (env, args)
                  val ty = TreeTypeOf.exp arg
                  in
                    (Env.TREE(T.E_Op(TOp.TensorIndex(ty, idxs), [arg])), stms)
                  end
              | Op.ProjectLast(_, idxs) => let
                  val ([arg], stms) = simpleArgs (env, args)
                  val ty = TreeTypeOf.exp arg
                  in
                    (Env.TREE(T.E_Op(TOp.ProjectLast(ty, idxs), [arg])), stms)
                  end
              | Op.Select(ty, i) => bindTREE (TOp.Select(U.trType ty, i))
              | Op.Subscript ty => bindTREE (TOp.Subscript(U.trType ty))
              | Op.Append ty => let
                  val (args as [_, item], stms) = singleArgs (env, args)
                  in
                    (Env.TREE(T.E_Op(TOp.Append(U.trType ty, TreeTypeOf.exp item), args)), stms)
                  end
              | Op.Prepend ty => let
                  val (args as [item, _], stms) = singleArgs (env, args)
                  in
                    (Env.TREE(T.E_Op(TOp.Prepend(U.trType ty, TreeTypeOf.exp item), args)), stms)
                  end
              | Op.Concat ty => bindTREE (TOp.Concat(U.trType ty))
              | Op.Range => bindTREE TOp.Range
              | Op.Length ty => bindTREE (TOp.Length(U.trType ty))
              | Op.SphereQuery(dim, ty) => let
                  val ([pos, rad], stms) = singleArgs (env, args)
                  val ty' = U.trType ty
                  val rator = TOp.SphereQuery(dim, ty')
                  in
                    (Env.RHS(TTy.SeqTy(ty', NONE), T.E_Op(rator, [mkRef pos, rad])), stms)
                  end
              | Op.Sqrt => bindTREE TOp.Sqrt
              | Op.Cos => bindTREE TOp.Cos
              | Op.ArcCos => bindTREE TOp.ArcCos
              | Op.Sin => bindTREE TOp.Sin
              | Op.ArcSin => bindTREE TOp.ArcSin
              | Op.Tan => bindTREE TOp.Tan
              | Op.ArcTan => bindTREE TOp.ArcTan
              | Op.Exp  => bindTREE TOp.Exp
              | Op.Sign  => bindTREE TOp.Sign
              | Op.Ceiling 1 => bindTREE TOp.RCeiling
              | Op.Ceiling d => bindVOp TOp.VCeiling
              | Op.Floor 1 => bindTREE TOp.RFloor
              | Op.Floor d => bindVOp TOp.VFloor
              | Op.Round 1 => bindTREE TOp.RRound
              | Op.Round d => bindVOp TOp.VRound
              | Op.Trunc 1 => bindTREE TOp.RTrunc
              | Op.Trunc d => bindVOp TOp.VTrunc
              | Op.IntToReal => bindTREE TOp.IntToReal
              | Op.RealToInt 1 => bindTREE TOp.RealToInt
              | Op.RealToInt d => let
                  val [v] = args
                  val (layout, args, stms) = vectorArg (env, v)
                  in
                    (Env.TREE(T.E_Op(TOp.VToInt layout, args)), stms)
                  end
              | Op.NumStrands set => bindRHS (TTy.IntTy, TOp.NumStrands set)
              | Op.Transform info => bindTREE (TOp.Transform info)
              | Op.Translate info => bindTREE (TOp.Translate info)
              | Op.ControlIndex(info, ctl, d) => bindTREE (TOp.ControlIndex(info, ctl, d))
              | Op.LoadVoxel info => bindTREE (TOp.LoadVoxel info)
              | Op.Inside(info, s) => (case ImageInfo.dim info
                   of 1 => bindTREE (TOp.Inside(VectorLayout.realLayout, info, s))
                    | d => let
                        val [x, img] = args
                        val (layout, args, stms) = vectorArg (env, x)
                        val (img, stms) = simpleArg env (img, stms)
                        in
                          (Env.TREE(T.E_Op(TOp.Inside(layout, info, s), args@[img])), stms)
                        end
                  (* end case *))
              | Op.IndexInside(info, s) => bindTREE (TOp.IndexInside(info, s))
              | Op.ImageDim(info, d) => bindTREE(TOp.ImageDim(info, d))
              | Op.MathFn f => bindTREE (TOp.MathFn f)
              | Op.IfWrap => bindTREE(TOp.IfWrap )
              | rator => raise Fail("bogus operator " ^ Op.toString srcRator)
            (* end case *)
          end

  (* if required, add a TensorCopy operation to the rhs of an assignment *)
    fun mkAssignRHS (TTy.TensorTy _, rhs) = (case TreeTypeOf.exp rhs
           of TTy.TensorRefTy shp => T.E_Op(TOp.TensorCopy shp, [rhs])
            | _ => rhs
          (* end case *))
      | mkAssignRHS (_, rhs) = rhs

    fun mkDefn' (x, rhs) = mkDefn (x, mkAssignRHS(TV.ty x, rhs))
    fun mkAssign' (x, rhs) = mkAssign (x, mkAssignRHS(TV.ty x, rhs))

    local
      fun trLHS (env, x) = (case eqClassRepOf (env, x)
             of NOEQ => newMemLocal (env, x)
              | VAR y => y
              | _ => raise Fail "unexpected vector variable"
            (* end case *))
    in
    fun trEigenVecs (env, vals, vecs, rator, x, stms) = let
          val (e, stms) = simpleArg env (x, stms)
          val stm = T.S_MAssign([trLHS (env, vals), trLHS (env, vecs)], T.E_Op(rator, [mkRef e]))
          in
            stm :: stms
          end
    fun trEigenVals (env, vals, rator, x, stms) = let
          val (e, stms) = simpleArg env (x, stms)
          val (isDcl, lhs) = (case eqClassRepOf (env, vals)
                 of NOEQ => (true, newMemLocal (env, vals))
                  | VAR y => (false, y)
                  | _ => raise Fail "unexpected vector variable"
                (* end case *))
          val stm = T.S_Assign(isDcl, lhs, T.E_Op(rator, [mkRef e]))
          in
            stm :: stms
          end
    end (* local *)

(* cases:
        x in EqClass
                issue assignment; lhs is binding of representative (could be multiple vars)
        useCount(x) > 1 and rhs is not simple
        rhs is simple
        rhs is vector
*)
    fun trAssign (env, lhs, rhs, stms) = let
          fun getLHS () = (case UnifyVars.eqClassOf lhs of SOME x => x | _ => lhs)
        (* binding for the lhs variable, where the rhs is a simple expression.  We check to
         * see if it is part of an merged equivalence class, in which case we need to generate
         * assigment(s)
         *)
          fun bindSimple (rhs, stms) = (case eqClassRepOf(env, lhs)
                 of NOEQ => (Env.bindSimple (env, lhs, rhs); stms)
                  | VAR x' => mkAssign' (x', rhs) :: stms
                  | VEC xs' => (case V.ty lhs
                       of Ty.TensorTy[d] => let
                            val layout = Env.layoutVec env d
                            val rhs = mkRef rhs
                            in
                              List.foldli
                                (fn (i, x', stms) => mkAssign(x', T.E_VLoad(layout, rhs, i)) :: stms)
                                  stms
                                    xs'
                            end
                        | _ => raise Fail "inconsistent"
                      (* end case *))
                (* end case *))
          fun assignOp (rator, args) = let
                val (args, stms') = simpleArgs (env, args)
                in
                  case eqClassRepOf(env, lhs)
                   of NOEQ => (
                        Env.bindVar (env, lhs, Env.RHS(U.trTempType(V.ty lhs), T.E_Op(rator, args)));
                        stms' @ stms)
                    | VAR x' => stms' @ mkAssign' (x', T.E_Op(rator, args)) :: stms
                    | VEC _ => raise Fail ("unexpected VEC for lhs " ^ V.toString lhs)
                  (* end case *)
                end
        (* bind the lhs to a tensor cons expression (including Op.Zero) *)
          fun bindCons (args, Ty.TensorTy[d], stms) = let
                val layout = Env.layoutVec env d
                fun mkVecs (args, w::ws) = let
                    (* take arguments from args to build a vector value of width w; pad as
                     * necessary.
                     *)
                      fun take (0, args, es) = T.E_Vec(w, w, List.rev es) :: mkVecs (args, ws)
                        | take (i, [], es) = if #padded layout andalso null ws
                            then [T.E_Vec(w-i, w, List.rev es)]
                            else raise Fail "too few arguments for CONS"
                        | take (i, arg::args, es) = take (i-1, args, arg :: es)
                      in
                        take (w, args, [])
                      end
                  | mkVecs ([], []) = []
                  | mkVecs (_, []) = raise Fail "too many arguments for CONS"
                val es = mkVecs (args, #pieces layout)
                in
                  case eqClassRepOf(env, lhs)
                   of NOEQ => if (V.useCount lhs > 1)
                        then let
                          val vs = U.newVectorVars layout
                          in
                            Env.bindVar (env, lhs, Env.VEC(layout, List.map T.E_Var vs));
                            ListPair.foldl (fn (v, e, stms) => mkDefn(v, e)::stms) stms (vs, es)
                          end
                        else (Env.bindVar(env, lhs, Env.VEC(layout, es)); stms)
                    | VEC xs =>
                        ListPair.foldl (fn (x, e, stms) => mkAssign(x, e)::stms) stms (xs, es)
                    | _ => raise Fail "inconsistent"
                  (* end case *)
                end
            | bindCons (args, ty as Ty.TensorTy _, stms) = let
                val ty = U.trType ty
                val cons = T.E_Cons(args, ty)
                in
                  case eqClassRepOf(env, lhs)
                   of NOEQ => if (V.useCount lhs > 1)
                        then mkDefn (newMemLocal (env, lhs), cons) :: stms
                        else (
                          Env.bindVar (env, lhs, Env.RHS(ty, cons));
                          stms)
                    | VAR x => mkAssign (x, cons) :: stms
                    | VEC xs => raise Fail "inconsistent"
                  (* end case *)
                end
          in
            case rhs
             of IR.GLOBAL x => bindSimple (T.E_Global(mkGlobalVar x), stms)
              | IR.STATE(NONE, fld) =>
                  bindSimple (T.E_State(NONE, getStateVar fld), stms)
              | IR.STATE(SOME x, fld) => let
                  val (arg, stms) = simpleArg env (x, stms)
                  in
                    bindSimple (T.E_State(SOME arg, getStateVar fld), stms)
                  end
              | IR.VAR x => (case eqClassRepOf(env, lhs)
                   of NOEQ => (Env.bindVar(env, lhs, useVar env x); stms)
                    | VAR x' => (case useVar env x
                         of Env.RHS(_, e) => mkAssign' (x', e) :: stms
                          | Env.TREE e => mkAssign' (x', e) :: stms
                        (* end case *))
                    | VEC xs => let
                        val (_, es, stms') = vectorArg (env, x)
                        val stms = stms' @ stms
                        in
                          ListPair.foldl
                            (fn (x, e, stms) => mkAssign' (x, e) :: stms)
                              stms (xs, es)
                        end
                  (* end case *))
              | IR.LIT lit => bindSimple (T.E_Lit lit, stms)
              | IR.OP(Op.EigenVals2x2, [x]) => trEigenVals (env, lhs, TOp.EigenVals2x2, x, stms)
              | IR.OP(Op.EigenVals3x3, [x]) => trEigenVals (env, lhs, TOp.EigenVals3x3, x, stms)
              | IR.OP(Op.Zero(ty as Ty.TensorTy dd), []) => let
                  val z = T.E_Lit(Literal.Real(RealLit.zero false))
                  val sz = List.foldl Int.* 1 dd
                  in
                    bindCons (List.tabulate(sz, fn _ => z), ty, stms)
                  end
              | IR.OP(Op.MkDynamic(ty, n), args) => let
                  val lhs = newLocal (env, getLHS ())
                  val (args, stms') = simpleArgs (env, args)
                  in
            mkDefn (lhs, T.E_Op(TOp.MkDynamic(U.trType ty, n), args))
              :: stms' @ stms
                  end
              | IR.OP(Op.Strands _, _) => stms (* Op.Strands is translated by trMapReduce *)
              | IR.OP(Op.LoadSeq(ty, file), []) => let
                  val lhs = newLocal (env, getLHS ())
                  in
                    T.S_LoadNrrd(lhs, U.toAPIType ty, file, NONE) :: stms
                  end
              | IR.OP(Op.LoadImage(ty as Ty.ImageTy info, file), []) => let
                  val lhs = newLocal (env, getLHS ())
                  in
                    T.S_LoadNrrd(lhs, U.toAPIType ty, file, SOME info) :: stms
                  end
              | IR.OP(rator, args) => let
                  val (rhs, stms') = trOp (env, rator, args)
                  val stms = stms' @ stms
                  val emitBind = (V.useCount lhs > 1) orelse not(Env.isInlineOp env rator)
                  in
                    case (rhs, eqClassRepOf(env, lhs), emitBind)
                     of (_, NOEQ, false) => (Env.bindVar (env, lhs, rhs); stms)
                      | (Env.TREE e, NOEQ, true) => mkDefn'(newLocal(env, lhs), e) :: stms
                      | (Env.TREE e, VAR x', _) => mkAssign'(x', e) :: stms
                      | (Env.VEC(layout, es), NOEQ, true) => let
                          val vs = U.newVectorVars layout
                          in
                            Env.bindVar (env, lhs, Env.VEC(layout, List.map T.E_Var vs));
                            ListPair.foldl (fn (v, e, stms) => mkDefn(v, e)::stms) stms (vs, es)
                          end
                      | (Env.VEC(layout, es), VEC xs, _) =>
                          ListPair.foldl (fn (x, e, stms) => mkAssign(x, e)::stms) stms (xs, es)
                      | (Env.RHS(_, e), NOEQ, true) => mkDefn'(newLocal(env, lhs), e) :: stms
                      | (rhs, eqCls, emitBind) => (
                          print(concat[
                              "OP(", LowOps.toString rator, ", [",
                              String.concatWithMap "," V.toString args, "])\n",
                              "rhs = ", Env.bindingToString rhs, "\n",
                              "eqCls = ", eqToString eqCls, "\n",
                              "emitBind = ", Bool.toString emitBind, "\n"
                            ]);
                          raise Fail "inconsistent")
                    (* end case *)
                  end
              | IR.CONS(args, ty) => let
                  val (es, stms') = simpleArgs (env, args)
                  in
                    bindCons (es, ty, stms' @ stms)
                  end
              | IR.SEQ(args, ty) => let
                  val (es, stms') = singleArgs (env, args)
                  val stms = stms' @ stms
                  val ty = U.trType ty
                (* if we are dealing with a sequence of tensors, then we need to copy references *)
                  val es = (case ty
                         of TTy.SeqTy(TTy.TensorTy _, _) => List.map mkDeref es
                          | _ => es
                        (* end case *))
                  val seq = T.E_Seq(es, ty)
                  in
                    case eqClassRepOf(env, lhs)
                     of NOEQ => if (V.useCount lhs > 1)
                          then mkDefn (newMemLocal (env, lhs), seq) :: stms
                          else (
                            Env.bindVar (env, lhs, Env.RHS(ty, seq));
                            stms)
                      | VAR x => mkAssign (x, seq) :: stms
                      | VEC xs => raise Fail "inconsistent"
                    (* end case *)
                  end
              | IR.APPLY(f, args) => let
                  val (es, stms') = singleArgs (env, args)
                  in
                    Env.bindVar (env, lhs, Env.TREE(T.E_Apply(getFuncVar f, es)));
                    stms' @ stms
                  end
              | rhs => raise Fail(concat["unexpected ", IR.RHS.toString rhs, " in LowIR code"])
            (* end case *)
          end
handle ex => (
print(concat["trAssign: ", V.toString lhs, " = ", IR.RHS.toString rhs, "\n"]);
raise ex)

  (* In order to reconstruct the block-structure from the CFG, we keep a stack of open ifs.
   * the items on this stack distinguish between when we are processing the then and else
   * branches of the if.
   *)
    datatype open_if
    (* working on the "then" branch.  The fields are statments that preceed the if, the condition,
     * and the else-branch node.
     *)
      = THEN_BR of T.stm list * T.exp * IR.node
    (* working on the "else" branch.  The fields are statments that preceed the if, the condition,
     * the "then" branch statements, and the node kind that terminated the "then" branch (will be
     * a JOIN or EXIT(DIE, STABILIZE, or UNREACHABLE)).
     *)
      | ELSE_BR of T.stm list * T.exp * T.stm list * IR.node_kind

    fun trCFGWithEnv (env, cfg) = let
          fun useScalar x = (case useVar env x
                 of Env.RHS(_, e) => e
                  | Env.TREE e => e
                  | _ => raise Fail("expected scalar binding for " ^ V.toString x)
                (* end case *))
        (* analyze the CFG *)
          val _ = UnifyVars.analyze cfg
          val _ = AssignTypes.analyze cfg
        (* join (stk, stms, k): handle a control-flow join, where env is the
         * current environment, stk is the stack of open ifs (the top of stk specifies
         * which branch we are in), stms are the TreeIL statements preceding the join
         * on the current path, and k is the kind of the join node (either JOIN or EXIT).
         *)
          fun join ([], _, IR.JOIN _) = raise Fail "JOIN with no open if"
            | join ([], stms, _) = mkBlock (List.rev stms)
            | join (THEN_BR(stms1, cond, elseBr)::stk, thenBlk, k) = let
                val thenBlk = Env.flushPending (env, thenBlk)
                in
                  doNode (elseBr, ELSE_BR(stms1, cond, thenBlk, k)::stk, [])
                end
            | join (ELSE_BR(stms, cond, thenBlk, k1)::stk, elseBlk, k2) = let
                val elseBlk = Env.flushPending (env, elseBlk)
                in
                  case (k1, k2)
                   of (IR.JOIN{succ, ...}, IR.JOIN _) => let
                        val stm = mkIf(cond, List.rev thenBlk, List.rev elseBlk)
                        in
                          doNode (!succ, stk, stm::stms)
                        end
                    | (IR.JOIN{succ, ...}, _) => let
                        val stm = mkIf(cond, List.rev thenBlk, List.rev elseBlk)
                        in
                          doNode (!succ, stk, stm::stms)
                        end
                    | (_, IR.JOIN{succ, ...}) => let
                        val stm = mkIf(cond, List.rev thenBlk, List.rev elseBlk)
                        in
                          doNode (!succ, stk, stm::stms)
                        end
                    | (_, _) => let
                        val stm = mkIf(cond, List.rev thenBlk, List.rev elseBlk)
                        in
                          mkBlock (List.rev(stm::stms))
                        end
                  (* end case *)
                end
          and doNode (nd : IR.node, ifStk : open_if list, stms) = (case IR.Node.kind nd
                 of IR.NULL => raise Fail "unexpected NULL"
                  | IR.ENTRY{succ} => doNode (!succ, ifStk, stms)
                  | k as IR.JOIN _ => join (ifStk, stms, k)
                  | IR.COND{cond, trueBranch, falseBranch, ...} => let
                      val cond = useScalar (!cond)
                      val stms = Env.flushPending (env, stms)
                      in
                        doNode (!trueBranch, THEN_BR(stms, cond, !falseBranch)::ifStk, [])
                      end
                  | IR.FOREACH{var, src, bodyEntry, succ, ...} => let
                      val var' = U.newIterVar var
                      val _ = Env.bindSimple (env, var, T.E_Var var')
                    (* note that the flatten phase on LowIR should guarantee that each
                     * FOREACH node that takes a Range argument has its own instance of
                     * the Range expression.
                     * Note also that we resolve the src before processing the body
                     * of the loop so that any nested flushPendings do not cause Range
                     * bindings to be flushed.
                     *)
                      val (src', stms) = simpleArg env (!src, stms)
                      val stms = Env.flushPending (env, stms)
                      val mkStm = (case src'
                             of T.E_Op(TOp.Range, [lo, hi]) =>
                                  (fn body => T.S_For(var', lo, hi, body))
                              | e =>
                                  (fn body => T.S_Foreach(var', e, body))
                            (* end case *))
                      val stm = mkStm (doNode (!bodyEntry, [], []))
                      in
                        doNode (!succ, ifStk, stm::stms)
                      end
                  | IR.NEXT _ => mkBlock (List.rev stms)
                  | IR.COM {text, succ, ...} =>
                      doNode (!succ, ifStk, T.S_Comment text :: stms)
                  | IR.ASSIGN{stm=(lhs, rhs), succ, ...} =>
                      doNode (!succ, ifStk, trAssign (env, lhs, rhs, stms))
                  | IR.MASSIGN{stm=([], IR.OP(Op.KillAll, [])), succ, ...} =>
                      doNode (!succ, ifStk, T.S_KillAll :: stms)
                  | IR.MASSIGN{stm=([], IR.OP(Op.StabilizeAll, [])), succ, ...} =>
                      doNode (!succ, ifStk, T.S_StabilizeAll :: stms)
                  | IR.MASSIGN{stm=([], IR.OP(Op.Print tys, xs)), succ, ...} => let
                      val (es, stms') = singleArgs (env, xs)
                    (* translate TensorTy to TensorRefTy in the type list *)
                      fun trType (Ty.TensorTy(shp as _::_)) = TTy.TensorRefTy shp
                        | trType ty = U.trType ty
                      val tys = List.map trType tys
                      val stm = T.S_Print(tys, List.map mkRef es)
                      in
                        doNode (!succ, ifStk, stm :: List.revAppend (stms', stms))
                      end
                  | IR.MASSIGN{stm=([vals, vecs], IR.OP(Op.EigenVecs2x2, [x])), succ, ...} =>
                      doNode (
                        !succ, ifStk,
                        trEigenVecs (env, vals, vecs, TOp.EigenVecs2x2, x, stms))
                  | IR.MASSIGN{stm=([vals, vecs], IR.OP(Op.EigenVecs3x3, [x])), succ, ...} =>
                      doNode (
                        !succ, ifStk,
                        trEigenVecs (env, vals, vecs, TOp.EigenVecs3x3, x, stms))
                  | IR.MASSIGN{stm=(ys, IR.MAPREDUCE mrs), succ, ...} =>
                      doNode (!succ, ifStk, trMapReduce (env, ys, mrs, stms))
                  | IR.MASSIGN{stm=(ys, rhs), succ, ...} => raise Fail(concat[
                        "unexepected rhs ", IR.RHS.toString rhs, " for MASSIGN"
                      ])
                  | IR.GASSIGN{lhs, rhs, succ, ...} => let
                      val gv = mkGlobalVar lhs
                      fun mkGAssign (gv, e) = T.S_GAssign(gv, mkAssignRHS(TGV.ty gv, e))
                      val stm = (case useVar env rhs
                             of Env.RHS(_, e) => mkGAssign(gv, e)
                              | Env.TREE e => mkGAssign(gv, e)
                              | Env.VEC(layout, es) => let
                                  val tmp = U.newTempVar("_arg", TTy.TensorTy[#wid layout])
                                  in
                                    T.S_GAssign(gv, T.E_Pack(layout, es))
                                  end
                            (* end case *))
                      in
                        doNode (!succ, ifStk, stm :: stms)
                      end
                  | IR.NEW{strand, args, succ, ...} => let
                      val (es, stms') = singleArgs (env, args)
                      val stm = T.S_New(strand, es)
                      in
                        doNode (!succ, ifStk, stm :: List.revAppend (stms', stms))
                      end
                  | IR.SAVE{lhs, rhs, succ, ...} => let
                      val sv = getStateVar lhs
                      fun mkSAssign (sv, e) = T.S_Save(sv, mkAssignRHS(TSV.ty sv, e))
                      val stm = (case useVar env rhs
                             of Env.RHS(_, e) => mkSAssign(sv, e)
                              | Env.TREE e => mkSAssign(sv, e)
                              | Env.VEC(layout, es) => let
                                  val tmp = U.newTempVar("_arg", TTy.TensorTy[#wid layout])
                                  in
                                    T.S_Save(sv, T.E_Pack(layout, es))
                                  end
                            (* end case *))
                      in
                        doNode (!succ, ifStk, stm :: stms)
                      end
                  | k as IR.EXIT{kind, succ, ...} => (case (!succ, kind)
                       of (NONE, ExitKind.RETURN NONE) => join (ifStk, stms, k)
                        | (NONE, ExitKind.RETURN(SOME x)) => let
                            val (e', stms) = singleArg env (x, stms)
                            in
                              join (ifStk, T.S_Return(SOME e') :: stms, k)
                            end
                        | (NONE, ExitKind.ACTIVE) => join (ifStk, T.S_Active :: stms, k)
                        | (NONE, ExitKind.STABILIZE) => let
                            val stms = T.S_Stabilize :: stms
                            in
                              join (ifStk, stms, k)
                            end
                        | (NONE, ExitKind.DIE) => join (ifStk, T.S_Die :: stms, k)
                        | (NONE, ExitKind.NEXTSTEP) => join (ifStk, T.S_Return NONE :: stms, k)
                        | (NONE, ExitKind.UNREACHABLE) => join (ifStk, stms, k)
                        | (SOME nd, ExitKind.RETURN NONE) => doNode (nd, ifStk, stms)
                        | (SOME nd, ExitKind.RETURN(SOME x)) => let
                            val (e', stms) = singleArg env (x, stms)
                            in
                              doNode (nd, ifStk, T.S_Return(SOME e') :: stms)
                            end
                        | (SOME nd, ExitKind.ACTIVE) => doNode (nd, ifStk, T.S_Active :: stms)
                        | (SOME nd, ExitKind.STABILIZE) => doNode (nd, ifStk, T.S_Stabilize :: stms)
                        | (SOME nd, ExitKind.DIE) => doNode (nd, ifStk, T.S_Die :: stms)
                        | (SOME nd, ExitKind.NEXTSTEP) => doNode (nd, ifStk, T.S_Return NONE :: stms)
                        | (SOME nd, ExitKind.UNREACHABLE) => doNode (nd, ifStk, stms)
                      (* end case *))
                (* end case *))
          in
            doNode (IR.CFG.entry cfg, [], [])
          end

    and trMapReduce (env, ys, mrs as (_, _, x::_)::rest, stms) = let
          val stms = Env.flushPending (env, stms)
        (* get the strand type of the first map-reduce *)
          val IR.OP(Op.Strands(Ty.StrandTy strand, _), _) = IR.Var.getDef x
          val srcVar = U.newTempVar("ix", TTy.StrandIdTy strand)
          fun mkMR (y, (r, f, src::args), (mrs, stms)) = let
                val lhs = (case eqClassRepOf (env, y)
                       of NOEQ => newMemLocal (env, y)
                        | VAR y => y
                        | _ => raise Fail "unexpected vector variable"
                      (* end case *))
                val srcSet = (case IR.Var.getDef src
                       of IR.OP(Op.Strands(_, set), _) => set
                        | rhs => raise Fail("expected Strands, but found " ^ IR.RHS.toString rhs)
                      (* end case *))
                val (es, stms') = singleArgs (env, args)
                val mr = T.MapReduce(lhs, r, getFuncVar f, T.E_Var srcVar :: es, srcSet)
                in
                  (mr :: mrs, stms' @ stms)
                end
          val (mrs, stms) = ListPair.foldl mkMR ([], stms) (ys, mrs)
          val mrStm = T.S_MapReduce(List.rev mrs, srcVar)
          in
            mrStm :: stms
          end
      | trMapReduce _ = raise Fail "unexpected ill-formed map-reduce"

    fun trCFG info cfg = ScopeVars.assignScopes ([], trCFGWithEnv (Env.new info, cfg))

  (* conversion for global start and update blocks where we have to handle map-reduce
   * statements.
   *)
    fun trGlobalCFG (info, funcs) cfg =
          ScopeVars.assignScopes ([], trCFGWithEnv (Env.newWithFuncs (info, funcs), cfg))

  (* Convert a user-defined function.  We need to check for language features
   * that require the world pointer (e.g., printing) and for references to global variables.
   *)
    fun trFunc info (IR.Func{name, params, body}) = let
          val params' = List.map U.newParamVar params
          val env = Env.new info
          val _ = ListPair.appEq
                    (fn (x, x') => Env.bindSimple (env, x, T.E_Var x'))
                      (params, params')
          val body' = ScopeVars.assignScopes (params', trCFGWithEnv (env, body))
          val {needsWorld, usesGlobals} = Util.analyzeBlock body'
          val name' = mkFuncVar (name, needsWorld, usesGlobals)
          in
            T.Func{name = name', params = params', body = body'}
          end

  (* Build a strand method from a TreeIR block.  We need to check for language features
   * that require the world pointer (e.g., printing) and for references to global variables.
   *)
    fun mkMethod body = let
          val {needsWorld, usesGlobals} = Util.analyzeBlock body
          in
            T.Method{needsW = needsWorld, hasG = usesGlobals, body = body}
          end

    fun trStrand info strand = let
          val IR.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          val trMethod = mkMethod o trCFG info
          val params' = List.map U.newParamVar params
          val state' = List.map getStateVar state
          val stateInit' = let
                val env = Env.new info
                in
                  ListPair.appEq
                    (fn (x, x') => Env.bindSimple (env, x, T.E_Var x'))
                      (params, params');
                  mkMethod (ScopeVars.assignScopes (params', trCFGWithEnv (env, stateInit)))
                end
          in
            T.Strand{
                name = name,
                params = params',
                spatialDim = spatialDim,
                state = state',
                stateInit = stateInit',
                startM = Option.map trMethod startM,
                updateM = trMethod updateM,
                stabilizeM = Option.map trMethod stabilizeM
              }
          end

    fun translate (prog, info) = let
        (* first step is to flatten any nested CONS nodes *)
          val prog = Flatten.transform prog
          val LowIR.Program{
                  props, consts, inputs, constInit, globals,
                  funcs, globInit, strand, create, start, update
                } = prog
          val trCFG = trCFG info
          val trGlobalCFG = trGlobalCFG (info, funcs)
          in
            TreeIR.Program{
                props = props,
                target = info,
                consts = List.map mkGlobalVar consts,
                inputs = List.map (Inputs.map mkGlobalVar) inputs,
                constInit = trCFG constInit,
                globals = List.map mkGlobalVar globals,
                funcs = List.map (trFunc info) funcs,
                globInit = trCFG globInit,
                strand = trStrand info strand,
                create = Create.map trCFG create,
                start = Option.map trGlobalCFG start,
                update = Option.map trGlobalCFG update
              }
          end

  end
