(* high-to-mid.sml
 *
 * Translation from HighIR to MidIR representations.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure HighToMid : sig

    val translate : HighIR.program -> MidIR.program

  end = struct

    structure SrcIR = HighIR
    structure SrcTy = HighTypes
    structure SrcOp = HighOps
    structure SrcSV = SrcIR.StateVar
    structure VTbl = SrcIR.Var.Tbl
    structure DstIR = MidIR
    structure DstTy = MidTypes
    structure DstOp = MidOps
    structure InP = Inputs
    structure BCtl = BorderCtl

    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt

    fun getRHS x = (case SrcIR.Var.getDef x
           of SrcIR.GLOBAL gv => raise Fail(concat[
                  "expected rhs binding for ", SrcIR.Var.toString x,
                  " but found mutable global ", SrcIR.GlobalVar.toString gv
                ])
            | rhs => rhs
          (* end case *))

    fun getRHSOp x = (case SrcIR.Var.getDef x
           of SrcIR.OP(rator, args) => (rator, args)
            | rhs => raise Fail(concat[
                  "expected rhs operator for ", SrcIR.Var.toString x,
                  " but found ", SrcIR.RHS.toString rhs
                ])
          (* end case *))

    fun cvtTy SrcTy.BoolTy = DstTy.BoolTy
      | cvtTy SrcTy.StringTy = DstTy.StringTy
      | cvtTy SrcTy.IntTy = DstTy.intTy
      | cvtTy (SrcTy.TensorTy dd) = DstTy.tensorTy dd
      | cvtTy (SrcTy.TupleTy tys) = DstTy.TupleTy(List.map cvtTy tys)
      | cvtTy (SrcTy.SeqTy(ty, n)) = DstTy.SeqTy(cvtTy ty, n)
      | cvtTy (SrcTy.ImageTy info) = DstTy.ImageTy info
      | cvtTy (SrcTy.StrandTy n) = DstTy.StrandTy n
      | cvtTy SrcTy.KernelTy = DstTy.KernelTy
    (* we replace Field operations by 0, so the types are mapped to int *)
      | cvtTy SrcTy.FieldTy = DstTy.intTy

  (* instantiate the translation environment *)
    structure Env = TranslateEnvFn (
      struct
        structure SrcIR = SrcIR
        structure DstIR = DstIR
        val cvtTy = cvtTy
      end)

  (* expand raising a real to an integer power.  When we know the exponent, we can inline
   * multiplications.
   *)
    fun expandPower (env, y, [x, n]) = let
          fun getConst x = (case SrcIR.Var.getDef x
                 of SrcIR.LIT(Literal.Int n) => SOME n
                  | _ => NONE
                (* end case *))
          val x = Env.rename(env, x)
          fun pow () = let
                val t = DstIR.Var.new("n", DstTy.realTy)
                in [
                  (t, DstIR.OP(DstOp.IntToReal, [Env.rename(env, n)])),
                  (y, DstIR.OP(DstOp.MathFn MathFns.POW, [x, t]))
                ] end
          in
            case getConst n
             of SOME 0 => [(y, DstIR.LIT(Literal.Real(RealLit.one)))]
              | SOME 1 => [(y, DstIR.VAR x)]
              | SOME ~1 => let
                  val t = DstIR.Var.new("one", DstTy.realTy)
                  in [
                    (t, DstIR.LIT(Literal.Real(RealLit.one))),
                    (y, DstIR.EINAPP(MkOperators.divRR, [t, x]))
                  ] end
              | SOME 2 => [(y, DstIR.EINAPP(MkOperators.mulRR, [x, x]))]
(* FIXME: expand into multiplications
              | SOME n =>
*) | SOME _ => pow()
              | NONE => pow()
            (* end case *)
          end

  (* expand the High IR Inside operator into an image-space test *)
    fun expandInside (env, result, pos, img, info, s) = let
          val pos = Env.rename (env, pos)
          val img = Env.rename (env, img)
        (* map pos to index space point ix *)
          val (x, code) = let
                val avail = AvailRHS.new()
                val (_, x) = CoordSpaceTransform.worldToImage {
                        avail = avail, info = info, img = img, pos = pos
                      }
                in
                  (x, AvailRHS.getAssignments avail)
                end
          val code = (result, DstIR.OP(DstOp.Inside(info, s), [x, img])) :: code
          in
            List.rev code
          end

    fun arity (SrcTy.TensorTy[]) = 1
      | arity (SrcTy.TensorTy[d]) = d
      | arity _ = raise Fail "arity"

    fun expandOp (env, y, rator, args) = let
          fun assign rator' =
                [(y, DstIR.OP(rator', Env.renameList(env, args)))]
          fun cvtToInt rator' = let
                val t = DstIR.Var.new ("t", DstTy.realTy)
                in [
                  (t, DstIR.OP(rator', Env.renameList(env, args))),
                  (y, DstIR.OP(DstOp.RealToInt 1, [t]))
                ] end
          fun dummy () = [(y, DstIR.LIT(Literal.Int 0))]
          fun copy () = let val [x] = args
                in
                  [(y, DstIR.VAR(Env.rename(env, x)))]
                end
          in
            case rator
             of SrcOp.IAdd => assign DstOp.IAdd
              | SrcOp.ISub => assign DstOp.ISub
              | SrcOp.IMul => assign DstOp.IMul
              | SrcOp.IDiv => assign DstOp.IDiv
              | SrcOp.IMod => assign DstOp.IMod
              | SrcOp.INeg => assign DstOp.INeg
              | SrcOp.IAbs => assign DstOp.IAbs
              | SrcOp.LT ty => assign (DstOp.LT(cvtTy ty))
              | SrcOp.LTE ty => assign (DstOp.LTE(cvtTy ty))
              | SrcOp.EQ ty => assign (DstOp.EQ(cvtTy ty))
              | SrcOp.NEQ ty => assign (DstOp.NEQ(cvtTy ty))
              | SrcOp.GT ty => assign (DstOp.GT(cvtTy ty))
              | SrcOp.GTE ty => assign (DstOp.GTE(cvtTy ty))
              | SrcOp.Power => expandPower(env, y, args)
              | SrcOp.BAnd => assign DstOp.BAnd
              | SrcOp.BOr => assign DstOp.BOr
              | SrcOp.BNot => assign DstOp.BNot
              | SrcOp.Max ty => assign (DstOp.Max(cvtTy ty))
              | SrcOp.Min ty => assign (DstOp.Min(cvtTy ty))
              | SrcOp.Zero ty => assign (DstOp.Zero(cvtTy ty))
              | SrcOp.TensorIndex(ty, shp) => assign (DstOp.TensorIndex(cvtTy ty, shp))
              | SrcOp.Select(ty, i) => assign (DstOp.Select(cvtTy ty, i))
              | SrcOp.Subscript(ty as SrcTy.SeqTy _) => assign (DstOp.Subscript(cvtTy ty))
              | SrcOp.MkDynamic(ty, n) => assign (DstOp.MkDynamic(cvtTy ty, n))
              | SrcOp.Append ty => assign (DstOp.Append(cvtTy ty))
              | SrcOp.Prepend ty => assign (DstOp.Prepend(cvtTy ty))
              | SrcOp.Concat ty => assign (DstOp.Concat(cvtTy ty))
              | SrcOp.Range => assign DstOp.Range
              | SrcOp.Length ty => assign (DstOp.Length(cvtTy ty))
              | SrcOp.SphereQuery(dim, ty) => assign (DstOp.SphereQuery(dim, cvtTy ty))
              | SrcOp.IntToReal => assign DstOp.IntToReal
              | SrcOp.TruncToInt => cvtToInt (DstOp.Trunc 1)
              | SrcOp.RoundToInt => cvtToInt (DstOp.Round 1)
              | SrcOp.CeilToInt => cvtToInt (DstOp.Ceiling 1)
              | SrcOp.FloorToInt => cvtToInt (DstOp.Floor 1)
              | SrcOp.NumStrands set => assign (DstOp.NumStrands set)
              | SrcOp.Strands(ty, set) => assign (DstOp.Strands(cvtTy ty, set))
              | SrcOp.Kernel h => assign (DstOp.Kernel h)
              | SrcOp.Inside(info, s) => (case args
                   of [pos, img] => expandInside(env, y, pos, img, info, s)
                  (* end case *))
              | SrcOp.ImageDim(info, i) => assign (DstOp.ImageDim(info, i))
              | SrcOp.BorderCtlDefault info => assign (DstOp.BorderCtlDefault info)
              | SrcOp.BorderCtlClamp info => assign (DstOp.BorderCtlClamp info)
              | SrcOp.BorderCtlMirror info => assign (DstOp.BorderCtlMirror info)
              | SrcOp.BorderCtlWrap info => assign (DstOp.BorderCtlWrap  info)
              | SrcOp.LoadSeq(ty, file) => assign (DstOp.LoadSeq(cvtTy ty, file))
              | SrcOp.LoadImage(ty, file) => assign (DstOp.LoadImage(cvtTy ty, file))
              | SrcOp.MathFn e => assign (DstOp.MathFn e)
              | rator => raise Fail("bogus operator " ^ SrcOp.toString rator)
            (* end case *)
          end
handle ex => (print(concat["HighToMid.expandOp: error converting ", SrcOp.toString rator, "\n"]); raise ex)

  (* expandEINAPP: env* midil.var*EIN*mid-ilvar->DstIR.ASSGN list
   * Field operators are changed to zero
   *)
    fun expandEINAPP (env, srcy, y, rator, args) = (case SrcIR.Var.ty srcy
           of SrcTy.FieldTy => [DstIR.ASSGN(y, DstIR.LIT(Literal.Int 0))]
            | _ => if (useCount srcy > 0)
                then HandleEin.expand (y, rator, Env.renameList(env, args))
                else []
          (* end case *))
handle ex => (print(concat["HighToMid.expandEINAPP: error converting ", MidIR.Var.toString y, " = ",
EinPP.toString rator, " ", "(", String.concatWithMap ", " HighIR.Var.toString args, ")\n"]);
raise ex)

  (* expand a SrcIR assignment to a list of DstIR assignments *)
    fun expand (env, (y, rhs)) = let
          fun assign rhs = [DstIR.ASSGN(Env.rename (env, y), rhs)]
          in
            case rhs
             of SrcIR.GLOBAL x => assign (DstIR.GLOBAL(Env.renameGV(env, x)))
              | SrcIR.STATE(NONE, fld) => assign (DstIR.STATE(NONE, Env.renameSV(env, fld)))
              | SrcIR.STATE(SOME x, fld) =>
                  assign (DstIR.STATE(SOME(Env.rename(env, x)), Env.renameSV(env, fld)))
              | SrcIR.VAR x => assign (DstIR.VAR(Env.rename(env, x)))
              | SrcIR.LIT lit => assign (DstIR.LIT lit)
              | SrcIR.OP(rator, args) =>
                  List.map DstIR.ASSGN (expandOp (env, Env.rename (env, y), rator, args))
              | SrcIR.CONS(args, ty) => assign (DstIR.CONS(Env.renameList(env, args), cvtTy ty))
              | SrcIR.SEQ(args, ty) => assign (DstIR.SEQ(Env.renameList(env, args), cvtTy ty))
              | SrcIR.EINAPP(rator, args) =>
                  expandEINAPP (env, y, Env.rename (env, y), rator, args)
              | SrcIR.APPLY(f, args) =>
                  assign (DstIR.APPLY(Env.renameFV(env, f), Env.renameList(env, args)))
              | _ => raise Fail("bogus rhs for ASSIGN: " ^ SrcIR.RHS.toString rhs)
            (* end case *)
          end

  (* expand a SrcIR multi-assignment to a DstIR CFG *)
    fun mexpand (env, (ys, rhs)) = let
          fun massign rhs = let
                val nd = DstIR.Node.mkMASSIGN(Env.renameList(env, ys), rhs)
                in
                  DstIR.CFG{entry=nd, exit=nd}
                end
          fun mkOP (rator, xs) = massign(DstIR.OP(rator, Env.renameList(env, xs)))
          in
            case rhs
             of SrcIR.OP(SrcOp.Eigen2x2, xs) => mkOP (DstOp.EigenVecs2x2, xs)
              | SrcIR.OP(SrcOp.Eigen3x3, xs) => mkOP (DstOp.EigenVecs3x3, xs)
              | SrcIR.OP(SrcOp.KillAll, []) => mkOP (DstOp.KillAll, [])
              | SrcIR.OP(SrcOp.StabilizeAll, []) => mkOP (DstOp.StabilizeAll, [])
              | SrcIR.OP(SrcOp.Print tys, xs) => mkOP (DstOp.Print(List.map cvtTy tys), xs)
              | SrcIR.MAPREDUCE mrs => let
                  val mrs = List.map
                        (fn (r, f, xs) => (r, Env.renameFV(env, f), Env.renameList(env, xs)))
                          mrs
                  in
                    massign (DstIR.MAPREDUCE mrs)
                  end
              | _ => raise Fail("bogus rhs for MASSIGN: " ^ SrcIR.RHS.toString rhs)
            (* end case *)
          end

    structure Trans =  TranslateFn (
      struct
        open Env
        val expand = DstIR.CFG.mkBlock o expand
        val mexpand = mexpand
      end)

    structure Promote = PromoteFn (DstIR)

    fun translate prog = let
          val prog = Trans.translate prog
          in
            MidCensus.init prog;
            Promote.transform prog
          end

  end
