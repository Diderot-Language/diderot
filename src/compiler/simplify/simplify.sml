(* simplify.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Simplify the AST representation.  This phase involves the following transformations:
 *
 *      - types are simplified by removing meta variables (which will have been resolved)
 *
 *      - expressions are simplified to involve a single operation on variables
 *
 *      - global reductions are converted to MapReduce statements
 *
 *      - other comprehensions and reductions are converted to foreach loops
 *
 *      - unreachable code is pruned
 *
 *      - negation of literal integers and reals are constant folded
 *)

structure Simplify : sig

    val transform : Error.err_stream * AST.program * GlobalEnv.t -> Simple.program

  end = struct

    structure TU = TypeUtil
    structure S = Simple
    structure STy = SimpleTypes
    structure Ty = Types
    structure VMap = Var.Map
    structure II = ImageInfo
    structure BV = BasisVars

  (* environment for mapping small global constants to AST expressions *)
    type const_env = AST.expr VMap.map

  (* context for simplification *)
    datatype context = Cxt of {
        errStrm : Error.err_stream,
        gEnv : GlobalEnv.t,
        cEnv : const_env
      }

    fun getNrrdInfo (Cxt{errStrm, ...}, nrrd) = NrrdInfo.getInfo (errStrm, nrrd)

    fun findStrand (Cxt{gEnv, ...}, s) = GlobalEnv.findStrand(gEnv, s)

    fun insertConst (Cxt{errStrm, gEnv, cEnv}, x, e) = Cxt{
            errStrm = errStrm, gEnv = gEnv, cEnv = VMap.insert(cEnv, x, e)
          }

    fun findConst (Cxt{cEnv, ...}, x) = VMap.find(cEnv, x)

    fun error (Cxt{errStrm, ...}, msg) = Error.error (errStrm, msg)
    fun warning (Cxt{errStrm, ...}, msg) = Error.warning (errStrm, msg)

  (* error message for when a nrrd image file is incompatible with the declared image type *)
    fun badImageNrrd (cxt, nrrdFile, nrrdInfo, expectedDim, expectedShp) = let
          val NrrdInfo.NrrdInfo{dim, nElems, ...} = nrrdInfo
          val expectedNumElems = List.foldl (op * ) 1 expectedShp
          val prefix = String.concat[
                  "image file \"", nrrdFile, "\"  is incompatible with expected type image(",
                  Int.toString expectedDim, ")[",
                  String.concatWithMap "," Int.toString expectedShp, "]"
                ]
          in
            case (dim = expectedDim, nElems = expectedNumElems)
             of (false, true) => error (cxt, [
                    prefix, "; its dimension is ", Int.toString dim
                  ])
              | (true, false) => error (cxt, [
                    prefix, "; it has ", Int.toString nElems, " values per voxel"
                  ])
              | _ =>  error (cxt, [
                    prefix, ";  its dimension is ", Int.toString dim, " and it has ",
                    Int.toString nElems, " values per voxel"
                  ])
            (* end case *)
          end

  (* convert a Types.ty to a SimpleTypes.ty *)
    fun cvtTy ty = (case ty
           of Ty.T_Var(Ty.TV{bind, ...}) => (case !bind
                 of NONE => raise Fail "unresolved type variable"
                  | SOME ty => cvtTy ty
                (* end case *))
            | Ty.T_Bool => STy.T_Bool
            | Ty.T_Int => STy.T_Int
            | Ty.T_String => STy.T_String
            | Ty.T_Sequence(ty, NONE) => STy.T_Sequence(cvtTy ty, NONE)
            | Ty.T_Sequence(ty, SOME dim) => STy.T_Sequence(cvtTy ty, SOME(TU.monoDim dim))
            | Ty.T_Strand id => STy.T_Strand id
            | Ty.T_Kernel _ => STy.T_Kernel
            | Ty.T_Tensor shape => STy.T_Tensor(TU.monoShape shape)
            | Ty.T_Image{dim, shape} =>
                STy.T_Image(II.mkInfo(TU.monoDim dim, TU.monoShape shape))
            | Ty.T_Field{diff, dim, shape} => STy.T_Field{
                  diff = TU.monoDiff diff,
                  dim = TU.monoDim dim,
                  shape = TU.monoShape shape
                }
            | Ty.T_Fun(tys1, ty2) => raise Fail "unexpected T_Fun in Simplify"
            | Ty.T_Error => raise Fail "unexpected T_Error in Simplify"
          (* end case *))

    fun apiTypeOf x = let
          fun cvtTy STy.T_Bool = APITypes.BoolTy
            | cvtTy STy.T_Int = APITypes.IntTy
            | cvtTy STy.T_String = APITypes.StringTy
            | cvtTy (STy.T_Sequence(ty, len)) = APITypes.SeqTy(cvtTy ty, len)
            | cvtTy (STy.T_Tensor shape) = APITypes.TensorTy shape
            | cvtTy (STy.T_Image info) =
                APITypes.ImageTy(II.dim info, II.voxelShape info)
            | cvtTy ty = raise Fail "bogus API type"
          in
            cvtTy (SimpleVar.typeOf x)
          end

    fun newTemp (ty as STy.T_Image _) = SimpleVar.new ("img", SimpleVar.LocalVar, ty)
      | newTemp ty = SimpleVar.new ("_t", SimpleVar.LocalVar, ty)

  (* a property to map AST function variables to SimpleAST functions *)
    local
      fun cvt f = (case Var.monoTypeOf f
             of Ty.T_Fun(paramTys, resTy) =>
                  SimpleFunc.new (Var.nameOf f, cvtTy resTy, List.map cvtTy paramTys)
              | ty as Ty.T_Field _ => let
                  val ty' as STy.T_Field{dim, shape, ...} = cvtTy ty
                  in
                    SimpleFunc.newDiff (Var.nameOf f, STy.T_Tensor shape, [STy.T_Tensor[dim]])
                  end
              | ty => raise Fail "expected function or field type"
            (* end case *))
    in
    val {getFn = cvtFunc, ...} = Var.newProp cvt
    end

  (* a property to map AST variables to SimpleAST variables *)
    local
      fun cvt x = SimpleVar.new (Var.nameOf x, Var.kindOf x, cvtTy(Var.monoTypeOf x))
      val {getFn, setFn, ...} = Var.newProp cvt
    in
    val cvtVar = getFn
    fun newVarWithType (x, ty) = let
          val x' = SimpleVar.new (Var.nameOf x, Var.kindOf x, ty)
          in
            setFn (x, x');
            x'
          end
    end

    fun cvtVars xs = List.map cvtVar xs

  (* make a block out of a list of statements that are in reverse order *)
    fun mkBlock stms = S.Block{props = PropList.newHolder(), code = List.rev stms}

  (* make a variable definition *)
    fun mkDef (x, e) = S.S_Var(x, SOME e)

    fun mkRDiv (res, a, b) = mkDef (res, S.E_Prim(BV.div_rr, [], [a, b], STy.realTy))
    fun mkToReal (res, a) =
          mkDef (res, S.E_Coerce{srcTy = STy.T_Int, dstTy = STy.realTy, x = a})
    fun mkLength (res, elemTy, xs) =
          mkDef (res, S.E_Prim(BV.fn_length, [STy.TY elemTy], [xs], STy.T_Int))

  (* simplify a statement into a single statement (i.e., a block if it expands
   * into more than one new statement).
   *)
    fun simplifyBlock (cxt, stm) = mkBlock (simplifyStmt (cxt, stm, []))

  (* convert the lhs variable of a var decl or assignment; if the rhs is a LoadImage,
   * then we use the info from the proxy image to determine the type of the lhs
   * variable.
   *)
    and cvtLHS (lhs, S.E_LoadImage(_, _, info)) = newVarWithType(lhs, STy.T_Image info)
      | cvtLHS (lhs, _) = cvtVar lhs

  (* simplify the statement stm where stms is a reverse-order list of preceeding simplified
   * statements.  This function returns a reverse-order list of simplified statements.
   * Note that error reporting is done in the typechecker, but it does not prune unreachable
   * code.
   *)
    and simplifyStmt (cxt : context, stm, stms) : S.stmt list = (case stm
           of AST.S_Block body => let
                fun simplify ([], stms) = stms
                  | simplify (stm::r, stms) = simplify (r, simplifyStmt (cxt, stm, stms))
                in
                  simplify (body, stms)
                end
            | AST.S_Decl(x, NONE) => let
                val x' = cvtVar x
                in
                  S.S_Var(x', NONE) :: stms
                end
            | AST.S_Decl(x, SOME e) => let
                val (stms, e') = simplifyExp (cxt, e, stms)
                val x' = cvtLHS (x, e')
                in
                  S.S_Var(x', SOME e') :: stms
                end
(* FIXME: we should also define a "boolean negate" operation on AST expressions so that we can
 * handle both cases!
 *)
            | AST.S_IfThenElse(AST.E_Orelse(e1, e2), s1 as AST.S_Block[], s2) =>
                simplifyStmt (cxt, AST.S_IfThenElse(e1, s1, AST.S_IfThenElse(e2, s1, s2)), stms)
            | AST.S_IfThenElse(AST.E_Andalso(e1, e2), s1, s2 as AST.S_Block[]) =>
                simplifyStmt (cxt, AST.S_IfThenElse(e1, AST.S_IfThenElse(e2, s1, s2), s2), stms)
            | AST.S_IfThenElse(e, s1, s2) => let
                val (stms, x) = simplifyExpToVar (cxt, e, stms)
                val s1 = simplifyBlock (cxt, s1)
                val s2 = simplifyBlock (cxt, s2)
                in
                  S.S_IfThenElse(x, s1, s2) :: stms
                end
            | AST.S_Foreach((x, e), body) => let
                val (stms, xs') = simplifyExpToVar (cxt, e, stms)
                val body' = simplifyBlock (cxt, body)
                in
                  S.S_Foreach(cvtVar x, xs', body') :: stms
                end
            | AST.S_Assign((x, _), e) => let
                val (stms, e') = simplifyExp (cxt, e, stms)
                val x' = cvtLHS (x, e')
                in
                  S.S_Assign(x', e') :: stms
                end
            | AST.S_New(name, args) => let
                val (stms, xs) = simplifyExpsToVars (cxt, args, stms)
                in
                  S.S_New(name, xs) :: stms
                end
            | AST.S_KillAll => S.S_KillAll :: stms
            | AST.S_StabilizeAll => S.S_StabilizeAll :: stms
            | AST.S_Continue => S.S_Continue :: stms
            | AST.S_Die => S.S_Die :: stms
            | AST.S_Stabilize => S.S_Stabilize :: stms
            | AST.S_Return e => let
                val (stms, x) = simplifyExpToVar (cxt, e, stms)
                in
                  S.S_Return x :: stms
                end
            | AST.S_Print args => let
                val (stms, xs) = simplifyExpsToVars (cxt, args, stms)
                in
                  S.S_Print xs :: stms
                end
          (* end case *))

    and simplifyExp (cxt, exp, stms) = let
          fun doBorderCtl (f, args) = let
                val (ctl, arg) = if Var.same(BV.image_border, f)
                        then (BorderCtl.Default(hd args), hd(tl args))
                      else if Var.same(BV.image_clamp, f)
                        then (BorderCtl.Clamp, hd args)
                      else if Var.same(BV.image_mirror, f)
                        then (BorderCtl.Mirror, hd args)
                      else if Var.same(BV.image_wrap, f)
                        then (BorderCtl.Wrap, hd args)
                        else raise Fail "impossible"
                in
                  S.E_BorderCtl(ctl, arg)
                end
          fun doPrimApply (f, tyArgs, args, ty) = let
                val (stms, xs) = simplifyExpsToVars (cxt, args, stms)
                fun cvtTyArg (Types.TYPE tv) = S.TY(cvtTy(TU.resolve tv))
                  | cvtTyArg (Types.DIFF dv) = S.DIFF(TU.monoDiff(TU.resolveDiff dv))
                  | cvtTyArg (Types.SHAPE sv) = S.SHAPE(TU.monoShape(TU.resolveShape sv))
                  | cvtTyArg (Types.DIM dv) = S.DIM(TU.monoDim(TU.resolveDim dv))
                in
                  if Basis.isBorderCtl f
                    then (stms, doBorderCtl (f, xs))
                  else if Var.same(f, BV.fn_sphere_im)
                    then let
                    (* get the strand type for the query *)
                      val tyArgs as [S.TY(STy.T_Strand strand)] = List.map cvtTyArg tyArgs
                    (* get the strand environment for the strand *)
                      val SOME sEnv = findStrand(cxt, strand)
                      fun result (query, pos) =
                            (stms, S.E_Prim(query, tyArgs, cvtVar pos::xs, cvtTy ty))
                      in
                      (* extract the position variable and spatial dimension *)
                        case (StrandEnv.findPosVar sEnv, StrandEnv.getSpaceDim sEnv)
                         of (SOME pos, SOME 1) => result (BV.fn_sphere1_r, pos)
                          | (SOME pos, SOME 2) => result (BV.fn_sphere2_t, pos)
                          | (SOME pos, SOME 3) => result (BV.fn_sphere3_t, pos)
                          | _ => raise Fail "impossible"
                        (* end case *)
                      end
                    else (case Var.kindOf f
                       of Var.BasisVar => let
                            val tyArgs = List.map cvtTyArg tyArgs
                            in
                              (stms, S.E_Prim(f, tyArgs, xs, cvtTy ty))
                            end
                        | _ => raise Fail "bogus prim application"
                      (* end case *))
                end
          fun doCoerce (srcTy, dstTy, e, stms) = let
                val (stms, x) = simplifyExpToVar (cxt, e, stms)
                val dstTy = cvtTy dstTy
                val result = newTemp dstTy
                val rhs = S.E_Coerce{srcTy = cvtTy srcTy, dstTy = dstTy, x = x}
                in
                  (S.S_Var(result, SOME rhs)::stms, S.E_Var result)
                end
          in
            case exp
             of AST.E_Var(x, _) => (case Var.kindOf x
                   of Var.BasisVar => let
                        val ty = cvtTy(Var.monoTypeOf x)
                        val x' = newTemp ty
                        val stm = S.S_Var(x', SOME(S.E_Prim(x, [], [], ty)))
                        in
                          (stm::stms, S.E_Var x')
                        end
                    | Var.ConstVar => (case findConst(cxt, x)
                         of SOME e => let
                              val (stms, x') = simplifyExpToVar (cxt, e, stms)
                              in
                                (stms, S.E_Var x')
                              end
                          | NONE => (stms, S.E_Var(cvtVar x))
                        (* end case *))
                    | _ => (stms, S.E_Var(cvtVar x))
                  (* end case *))
              | AST.E_Lit lit => (stms, S.E_Lit lit)
              | AST.E_Kernel h => (stms, S.E_Kernel h)
              | AST.E_Select(e, (fld, _)) => let
                  val (stms, x) = simplifyExpToVar (cxt, e, stms)
                  in
                    (stms, S.E_Select(x, cvtVar fld))
                  end
              | AST.E_Prim(rator, tyArgs, args as [e], ty) => (case e
                   of AST.E_Lit(Literal.Int n) => if Var.same(BV.neg_i, rator)
                        then (stms, S.E_Lit(Literal.Int(~n))) (* constant-fold negation of integer literals *)
                        else doPrimApply (rator, tyArgs, args, ty)
                    | AST.E_Lit(Literal.Real f) =>
                        if Var.same(BV.neg_t, rator)
                          then (stms, S.E_Lit(Literal.Real(RealLit.negate f))) (* constant-fold negation of real literals *)
                          else doPrimApply (rator, tyArgs, args, ty)
(* QUESTION: is there common code in handling a reduction over a sequence of strands vs. over a strand set? *)
                    | AST.E_Comprehension(e', (x, e''), seqTy) => if Basis.isReductionOp rator
                        then let
                          val (stms, xs) = simplifyExpToVar (cxt, e'', stms)
                          val (bodyStms, bodyResult) = simplifyExpToVar (cxt, e', [])
                          val seqTy' as STy.T_Sequence(elemTy, NONE) = cvtTy seqTy
                          fun mkReductionLoop (redOp, bodyStms, bodyResult, stms) = let
                                val {rator, init, mvs} = Util.reductionInfo redOp
                                val acc = SimpleVar.new ("accum", Var.LocalVar, cvtTy ty)
                                val initStm = S.S_Var(acc, SOME(S.E_Lit init))
                                val updateStm = S.S_Assign(acc,
                                      S.E_Prim(rator, mvs, [acc, bodyResult], seqTy'))
                                val foreachStm = S.S_Foreach(cvtVar x, xs,
                                      mkBlock(updateStm :: bodyStms))
                                in
                                  (foreachStm :: initStm :: stms, S.E_Var acc)
                                end
                          in
                            case Util.identifyReduction rator
                             of Util.MEAN => let
                                  val (stms, S.E_Var resultV) = mkReductionLoop (
                                        Reductions.RSUM, bodyStms, bodyResult, stms)
                                  val num = SimpleVar.new ("num", Var.LocalVar, STy.T_Int)
                                  val rNum = SimpleVar.new ("rNum", Var.LocalVar, STy.realTy)
                                  val mean = SimpleVar.new ("mean", Var.LocalVar, STy.realTy)
                                  val stms =
                                        mkRDiv (mean, resultV, rNum) ::
                                        mkToReal (rNum, num) ::
                                        mkLength (num, elemTy, xs) ::
                                        stms
                                  in
                                    (stms, S.E_Var mean)
                                  end
                              | Util.VARIANCE => raise Fail "FIXME: VARIANCE"
                              | Util.RED red => mkReductionLoop (red, bodyStms, bodyResult, stms)
                            (* end case *)
                          end
                        else doPrimApply (rator, tyArgs, args, ty)
                    | AST.E_ParallelMap(e', x, xs, _) =>
                        if Basis.isReductionOp rator
                          then let
                            val (result, stms) = simplifyReduction (cxt, rator, e', x, xs, ty, stms)
                            in
                              (stms, S.E_Var result)
                            end
                          else raise Fail "unsupported operation on parallel map"
                    | _ => doPrimApply (rator, tyArgs, args, ty)
                  (* end case *))
              | AST.E_Prim(f, tyArgs, args, ty) => doPrimApply (f, tyArgs, args, ty)
              | AST.E_Apply((f, _), args, ty) => let
                  val (stms, xs) = simplifyExpsToVars (cxt, args, stms)
                  in
                    case Var.kindOf f
                     of Var.FunVar => (stms, S.E_Apply(SimpleFunc.use(cvtFunc f), xs))
                      | _ => raise Fail "bogus application"
                    (* end case *)
                  end
              | AST.E_Comprehension(e, (x, e'), seqTy) => let
                (* convert a comprehension to a foreach loop over the sequence defined by e' *)
                  val (stms, xs) = simplifyExpToVar (cxt, e', stms)
                  val (bodyStms, bodyResult) = simplifyExpToVar (cxt, e, [])
                  val seqTy' as STy.T_Sequence(elemTy, NONE) = cvtTy seqTy
                  val acc = SimpleVar.new ("accum", Var.LocalVar, seqTy')
                  val initStm = S.S_Var(acc, SOME(S.E_Seq([], seqTy')))
                  val updateStm = S.S_Assign(acc,
                        S.E_Prim(BV.at_dT, [S.TY elemTy], [acc, bodyResult], seqTy'))
                  val foreachStm = S.S_Foreach(cvtVar x, xs, mkBlock(updateStm :: bodyStms))
                  in
                    (foreachStm :: initStm :: stms, S.E_Var acc)
                  end
              | AST.E_ParallelMap(e, x, xs, ty) =>
                (* a map over a strand set without a reduction should be converted to a
                 * foreach loop.
                 *)
                  raise Fail "FIXME: unexpected ParallelMap without reduction"
              | AST.E_Tensor(es, ty) => let
                  val (stms, xs) = simplifyExpsToVars (cxt, es, stms)
                  in
                    (stms, S.E_Tensor(xs, cvtTy ty))
                  end
              | AST.E_Field(es, ty) => let
                  val (stms, xs) = simplifyExpsToVars (cxt, es, stms)
                  in
                    (stms, S.E_Field(xs, cvtTy ty))
                  end
              | AST.E_Seq(es, ty) => let
                  val (stms, xs) = simplifyExpsToVars (cxt, es, stms)
                  in
                    (stms, S.E_Seq(xs, cvtTy ty))
                  end
              | AST.E_Slice(e, indices, ty) => let (* tensor slicing *)
                  val (stms, x) = simplifyExpToVar (cxt, e, stms)
                  fun f NONE = NONE
                    | f (SOME(AST.E_Lit(Literal.Int i))) = SOME(Int.fromLarge i)
                    | f _ = raise Fail "expected integer literal in slice"
                  val indices = List.map f indices
                  in
                    (stms, S.E_Slice(x, indices, cvtTy ty))
                  end
              | AST.E_Cond(e1, e2, e3, ty) => let
                (* a conditional expression gets turned into an if-then-else statememt *)
                  val result = newTemp(cvtTy ty)
                  val (stms, x) = simplifyExpToVar (cxt, e1, S.S_Var(result, NONE) :: stms)
                  fun simplifyBranch e = let
                        val (stms, e) = simplifyExp (cxt, e, [])
                        in
                          mkBlock (S.S_Assign(result, e)::stms)
                        end
                  val s1 = simplifyBranch e2
                  val s2 = simplifyBranch e3
                  in
                    (S.S_IfThenElse(x, s1, s2) :: stms, S.E_Var result)
                  end
              | AST.E_Orelse(e1, e2) => simplifyExp (
                  cxt,
                  AST.E_Cond(e1, AST.E_Lit(Literal.Bool true), e2, Ty.T_Bool),
                  stms)
              | AST.E_Andalso(e1, e2) => simplifyExp (
                  cxt,
                  AST.E_Cond(e1, e2, AST.E_Lit(Literal.Bool false), Ty.T_Bool),
                  stms)
              | AST.E_LoadNrrd(_, nrrd, ty) => (case cvtTy ty
                   of ty as STy.T_Sequence(_, NONE) => (stms, S.E_LoadSeq(ty, nrrd))
                    | ty as STy.T_Image info => let
                        val dim = II.dim info
                        val shape = II.voxelShape info
                        in
                          case getNrrdInfo (cxt, nrrd)
                           of SOME nrrdInfo => (case II.fromNrrd(nrrdInfo, dim, shape)
                                 of NONE => (
                                      badImageNrrd (cxt, nrrd, nrrdInfo, dim, shape);
                                      (stms, S.E_LoadImage(ty, nrrd, II.mkInfo(dim, shape))))
                                  | SOME imgInfo =>
                                      (stms, S.E_LoadImage(STy.T_Image imgInfo, nrrd, imgInfo))
                                (* end case *))
                            | NONE => (
                                error (cxt, [
                                    "proxy-image file \"", nrrd, "\" does not exist"
                                  ]);
                                (stms, S.E_LoadImage(ty, nrrd, II.mkInfo(dim, shape))))
                          (* end case *)
                        end
                    | _ => raise Fail "bogus type for E_LoadNrrd"
                  (* end case *))
              | AST.E_Coerce{dstTy, e=AST.E_Lit(Literal.Int n), ...} => (case cvtTy dstTy
                   of SimpleTypes.T_Tensor[] => (stms, S.E_Lit(Literal.Real(RealLit.fromInt n)))
                    | _ => raise Fail "impossible: bad coercion"
                  (* end case *))
              | AST.E_Coerce{dstTy, srcTy, e as AST.E_Seq(es, ty)} => let
                  val Ty.T_Sequence(dstTy', dstBnd) = TU.prune dstTy
                  val Ty.T_Sequence(srcTy', srcBnd) = TU.prune srcTy
                  in
                    if STy.same(cvtTy dstTy', cvtTy srcTy')
                      then (* static-size to dynamic coercion *)
                        doCoerce (srcTy, dstTy, e, stms)
                      else let
                      (* distribute the coercion over the sequence elements *)
                        val es = List.map
                              (fn e => AST.E_Coerce{dstTy=dstTy', srcTy=srcTy', e=e})
                                es
                        val eTy = Ty.T_Sequence(dstTy', srcBnd)
                        val e = AST.E_Seq(es, eTy)
                        in
                          if Option.isSome dstBnd
                            then simplifyExp (cxt, e, stms)
                            else simplifyExp (cxt, AST.E_Coerce{dstTy=dstTy, srcTy=eTy, e=e}, stms)
                        end
                  end
              | AST.E_Coerce{srcTy, dstTy, e} => doCoerce (srcTy, dstTy, e, stms)
            (* end case *)
          end

    and simplifyExpToVar (cxt, exp, stms) = let
          val (stms, e) = simplifyExp (cxt, exp, stms)
          in
            case e
             of S.E_Var x => (stms, x)
              | _ => let
                  val x = newTemp (S.typeOf e)
                  in
                    (S.S_Var(x, SOME e)::stms, x)
                  end
            (* end case *)
          end

    and simplifyExpsToVars (cxt, exps, stms) = let
          fun f ([], xs, stms) = (stms, List.rev xs)
            | f (e::es, xs, stms) = let
                val (stms, x) = simplifyExpToVar (cxt, e, stms)
                in
                  f (es, x::xs, stms)
                end
          in
            f (exps, [], stms)
          end

  (* `simplifyReduction (cxt, rator, e, x, xs, resTy, stms)`
   * simplify a parallel map-reduce, where `e` is the body of the map, `x` is the
   * strand variable, and `xs` is the source of strands.
   *)
    and simplifyReduction (cxt, rator, e, x, xs, resTy, stms) = let
            val result = SimpleVar.new ("res", Var.LocalVar, cvtTy resTy)
            val x' = cvtVar x
            val (bodyStms, bodyResult) = simplifyExpToVar (cxt, e, [])
          (* convert the domain from a variable to a StrandSets.t value *)
            val domain = if Var.same(BV.set_active, xs) then StrandSets.ACTIVE
                  else if Var.same(BV.set_all, xs) then StrandSets.ALL
                  else if Var.same(BV.set_stable, xs) then StrandSets.STABLE
                    else raise Fail "impossible: not a strand set"
            val (func, args) = Util.makeFunction(
                  Var.nameOf rator, x', mkBlock(S.S_Return bodyResult :: bodyStms),
                  SimpleVar.typeOf bodyResult)
            in
              case Util.identifyReduction rator
               of Util.MEAN => let
                    val mapReduceStm = S.S_MapReduce[
                            S.MapReduce{
                                result = result, reduction = Reductions.RSUM, mapf = func,
                                args = args, source = x', domain = domain
                              }]
                    val num = SimpleVar.new ("num", Var.LocalVar, STy.T_Int)
                    val rNum = SimpleVar.new ("rNum", Var.LocalVar, STy.realTy)
                    val mean = SimpleVar.new ("mean", Var.LocalVar, STy.realTy)
                    val numStrandsOp = (case domain
                           of StrandSets.ACTIVE => BV.fn_numActive
                            | StrandSets.ALL => BV.fn_numStrands
                            | StrandSets.STABLE => BV.fn_numStable
                          (* end case *))
                    val stms =
                          mkRDiv (mean, result, rNum) ::
                          mkToReal (rNum, num) ::
                          mkDef (num, S.E_Prim(numStrandsOp, [], [], STy.T_Int)) ::
                          mapReduceStm ::
                          stms
                    in
                      (mean, stms)
                    end
                | Util.VARIANCE => raise Fail "FIXME: variance reduction"
                | Util.RED rator' => let
                    val mapReduceStm = S.S_MapReduce[
                            S.MapReduce{
                                result = result, reduction = rator', mapf = func, args = args,
                                source = x', domain = domain
                              }]
                    in
                      (result, mapReduceStm :: stms)
                    end
              (* end case *)
            end

  (* simplify a block and then prune unreachable and dead code *)
    fun simplifyAndPruneBlock cxt blk =
          DeadCode.eliminate (simplifyBlock (cxt, blk))

    fun simplifyStrand (cxt, strand) = let
          val AST.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          val params' = cvtVars params
          fun simplifyState ([], xs, stms) = (List.rev xs, stms)
            | simplifyState ((x, optE) :: r, xs, stms) = let
                val x' = cvtVar x
                in
                  case optE
                   of NONE => simplifyState (r, x'::xs, stms)
                    | SOME e => let
                        val (stms, e') = simplifyExp (cxt, e, stms)
                        in
                          simplifyState (r, x'::xs, S.S_Assign(x', e') :: stms)
                        end
                  (* end case *)
                end
          val (xs, stateInit) = let
              (* simplify the state-variable initializations *)
                val (xs, stms) = simplifyState (state, [], [])
              (* simplify optional "initialize" block *)
                val blk = (case stateInit
                       of SOME stm => mkBlock (simplifyStmt (cxt, stm, stms))
                        | NONE => mkBlock stms
                      (* end case *))
                in
                  (xs, blk)
                end
          in
            S.Strand{
                name = name,
                params = params',
                spatialDim = spatialDim,
                state = xs,
                stateInit = stateInit,
                startM = Option.map (simplifyAndPruneBlock cxt) startM,
                updateM = simplifyAndPruneBlock cxt updateM,
                stabilizeM = Option.map (simplifyAndPruneBlock cxt) stabilizeM
              }
          end

    fun transform (errStrm, prog, gEnv) = let
          val AST.Program{
                  props, const_dcls, input_dcls, globals, globInit, strand, create, start, update
                } = prog
          val consts' = ref[]
          val constInit = ref[]
          val inputs' = ref[]
          val globals' = ref[]
          val globalInit = ref[]
          val funcs = ref[]
        (* simplify the constant dcls: the small constants will be added to the context
         * while the large constants will be added to the const' list.
         *)
          val cxt = let
                val cxt = Cxt{errStrm = errStrm, gEnv = gEnv, cEnv = VMap.empty}
                fun simplifyConstDcl ((x, SOME e), cxt) = if Util.isSmallExp e
                      then insertConst (cxt, x, e)
                      else let
                        val (stms, e') = simplifyExp (cxt, e, [])
                        val x' = cvtVar x
                        in
                          consts' := x' :: !consts';
                          constInit := S.S_Assign(x', e') :: (stms @ !constInit);
                          cxt
                        end
                  | simplifyConstDcl _ = raise Fail "impossble"
                in
                  List.foldl simplifyConstDcl cxt const_dcls
                end
          fun simplifyInputDcl ((x, NONE), desc) = let
                val x' = cvtVar x
                val init = (case SimpleVar.typeOf x'
                       of STy.T_Image info => (
                            warning(cxt, [
                                "assuming a sample type of ", RawTypes.toString(II.sampleTy info),
                                " for '", SimpleVar.nameOf x',
                                "'; specify a proxy-image file to override the default sample type"
                              ]);
                            S.Image info)
                        | _ => S.NoDefault
                      (* end case *))
                val inp = S.INP{
                        var = x',
                        name = Var.nameOf x,
                        ty =  apiTypeOf x',
                        desc = desc,
                        init = init
                      }
                in
                  inputs' := inp :: !inputs'
                end
            | simplifyInputDcl ((x, SOME(AST.E_LoadNrrd(tvs, nrrd, ty))), desc) = let
                val (x', init) = (case Var.monoTypeOf x
                       of Ty.T_Sequence(_, NONE) => (cvtVar x, S.LoadSeq nrrd)
                        | Ty.T_Image{dim, shape} => let
                            val dim = TU.monoDim dim
                            val shape = TU.monoShape shape
                            in
                              case getNrrdInfo (cxt, nrrd)
                               of SOME nrrdInfo => (case II.fromNrrd(nrrdInfo, dim, shape)
                                     of NONE => (
                                          badImageNrrd (cxt, nrrd, nrrdInfo, dim, shape);
                                          (cvtVar x, S.Image(II.mkInfo(dim, shape))))
                                      | SOME info =>
                                          (newVarWithType(x, STy.T_Image info), S.Proxy(nrrd, info))
                                    (* end case *))
                                | NONE => (
                                    error (cxt, [
                                        "proxy-image file \"", nrrd, "\" does not exist"
                                      ]);
                                    (cvtVar x, S.Image(II.mkInfo(dim, shape))))
                              (* end case *)
                            end
                        | _ => raise Fail "impossible"
                      (* end case *))
                val inp = S.INP{
                        var = x',
                        name = Var.nameOf x,
                        ty = apiTypeOf x',
                        desc = desc,
                        init = init
                      }
                in
                  inputs' := inp :: !inputs'
                end
            | simplifyInputDcl ((x, SOME e), desc) = let
                val x' = cvtVar x
                val (stms, e') = simplifyExp (cxt, e, [])
                val inp = S.INP{
                        var = x',
                        name = Var.nameOf x,
                        ty = apiTypeOf x',
                        desc = desc,
                        init = S.ConstExpr
                      }
                in
                  inputs' := inp :: !inputs';
                  constInit := S.S_Assign(x', e') :: (stms @ !constInit)
                end
        (* simplify a global declaration *)
          fun simplifyGlobalDcl (AST.D_Var(x, NONE)) = globals' := cvtVar x :: !globals'
            | simplifyGlobalDcl (AST.D_Var(x, SOME e)) = let
                val (stms, e') = simplifyExp (cxt, e, [])
                val x' = cvtLHS (x, e')
                in
                  globals' := x' :: !globals';
                  globalInit := S.S_Assign(x', e') :: (stms @ !globalInit)
                end
            | simplifyGlobalDcl (AST.D_Func(f, params, body)) = let
                val f' = cvtFunc f
                val params' = cvtVars params
                val body' = simplifyAndPruneBlock cxt body
                in
                  funcs := S.Func{f=f', params=params', body=body'} :: !funcs
                end
            | simplifyGlobalDcl (AST.D_DiffFunc(f, params, body)) = let
              (* differentiable field function: we map it to both a function definition and
               * a field variable.
               *)
                val vf = cvtVar f
                val f' = SimpleFunc.use(cvtFunc f)
                val params' = cvtVars params
                val body' = simplifyAndPruneBlock cxt (AST.S_Return body)
                in
                  funcs := S.Func{f=f', params=params', body=body'} :: !funcs;
                  globals' := vf :: !globals';
                  globalInit := S.S_Assign(vf, S.E_FieldFn f') :: !globalInit
                end
          val () = (
                List.app simplifyInputDcl input_dcls;
                List.app simplifyGlobalDcl globals)
        (* check if there no remaining constants *)
          val props = if List.null(!consts')
                then Properties.clearProp Properties.HasConsts props
                else props
        (* make the global-initialization block *)
          val globInit = (case globInit
                 of SOME stm => mkBlock (simplifyStmt (cxt, stm, !globalInit))
                  | NONE => mkBlock (!globalInit)
                (* end case *))
        (* if the globInit block is non-empty, record the fact in the property list *)
          val props = (case globInit
                 of S.Block{code=[], ...} => props
                  | _ => Properties.GlobalInit :: props
                (* end case *))
          in
            S.Program{
                props = props,
                consts = List.rev(!consts'),
                inputs = List.rev(!inputs'),
                constInit = mkBlock (!constInit),
                globals = List.rev(!globals'),
                globInit = globInit,
                funcs = List.rev(!funcs),
                strand = simplifyStrand (cxt, strand),
                create = Create.map (simplifyAndPruneBlock cxt) create,
                start = Option.map (simplifyAndPruneBlock cxt) start,
                update = Option.map (simplifyAndPruneBlock cxt) update
              }
          end

  end
