(* mid-to-low.sml
 *
 * COPYRIGHT (c) 2017 The Diderot Project (http://diderot-language.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translation from MidIR to LowIR representations.
 *)

structure MidToLow : sig

    val translate : MidIR.program -> LowIR.program

  end = struct

    structure SrcIR = MidIR
    structure SrcOp = MidOps
    structure SrcGV = SrcIR.GlobalVar
    structure SrcSV = SrcIR.StateVar
    structure SrcTy = MidTypes
    structure VTbl = SrcIR.Var.Tbl
    structure DstIR = LowIR
    structure DstTy = LowTypes
    structure DstOp = LowOps

    fun cvtTy ty = (case ty
           of SrcTy.BoolTy => DstTy.BoolTy
            | SrcTy.StringTy => DstTy.StringTy
            | SrcTy.IntTy => DstTy.IntTy
            | SrcTy.TensorTy sh => DstTy.TensorTy sh
            | SrcTy.TupleTy tys => DstTy.TupleTy(List.map cvtTy tys)
            | SrcTy.SeqTy(ty, d) => DstTy.SeqTy(cvtTy ty, d)
            | SrcTy.ImageTy info => DstTy.ImageTy info
            | SrcTy.StrandTy n => DstTy.StrandTy n
            | SrcTy.KernelTy => DstTy.IntTy
          (* end case *))

  (* instantiate the translation environment *)
    structure Env = TranslateEnvFn (
      struct
        structure SrcIR = SrcIR
        structure DstIR = DstIR
        val cvtTy = cvtTy
      end)

     fun expandBuildPos (y, s, [pos]) = let
        (* create positions from 1-s.. to s *)
          val range = 2*s
          fun f (0, xs, stms) = (xs, List.rev stms)
            | f (i, xs, stms) = (case IntInf.fromInt(i - s)
                 of 0 => f(i-1, pos::xs, stms)
                  | j => let
                      val r = DstIR.Var.new ("rlit", DstTy.realTy)
                      val x = DstIR.Var.new ("idx", DstTy.realTy)
                      val stms = if (j < 0)
                            then (r, DstIR.LIT(Literal.Real(RealLit.fromInt(~j)))) ::
                              (x, DstIR.OP(DstOp.RAdd, [pos, r])) :: stms
                            else (r, DstIR.LIT(Literal.Real(RealLit.fromInt j))) ::
                              (x, DstIR.OP(DstOp.RSub, [pos, r])) :: stms
                      in
                        f(i-1, x::xs, stms)
                      end
                (* end case *))
          val (xs, stms) = f(range, [], [])
          in
            List.rev ((y, DstIR.CONS(xs, DstTy.TensorTy[range])) :: stms)
          end
      | expandBuildPos _ = raise Fail "expected one argument for BuildPos"

    fun expandOp (env, y, rator, args) = let
          val args' = Env.renameList (env, args)
          fun assign rator' = [(y, DstIR.OP(rator', args'))]
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
              | SrcOp.IAbs => assign (DstOp.Abs DstTy.intTy)
              | SrcOp.LT ty => assign (DstOp.LT(cvtTy ty))
              | SrcOp.LTE ty => assign (DstOp.LTE(cvtTy ty))
              | SrcOp.EQ ty => assign (DstOp.EQ(cvtTy ty))
              | SrcOp.NEQ ty => assign (DstOp.NEQ(cvtTy ty))
              | SrcOp.GT ty => assign (DstOp.GT(cvtTy ty))
              | SrcOp.GTE ty => assign (DstOp.GTE(cvtTy ty))
              | SrcOp.BAnd => assign DstOp.BAnd
              | SrcOp.BOr => assign DstOp.BOr
              | SrcOp.BNot => assign DstOp.BNot
              | SrcOp.Max ty => assign (DstOp.Max(cvtTy ty))
              | SrcOp.Min ty => assign (DstOp.Min(cvtTy ty))
              | SrcOp.EigenVecs2x2 => assign (DstOp.EigenVecs2x2)
              | SrcOp.EigenVecs3x3 => assign (DstOp.EigenVecs3x3)
              | SrcOp.EigenVals2x2 => assign (DstOp.EigenVals2x2)
              | SrcOp.EigenVals3x3 => assign (DstOp.EigenVals3x3)
              | SrcOp.Zero ty => assign (DstOp.Zero(cvtTy ty))
              | SrcOp.TensorIndex(SrcTy.TensorTy[d], [i]) => assign (DstOp.VIndex(d,i))
              | SrcOp.TensorIndex(ty, shp) => assign (DstOp.TensorIndex(cvtTy ty, shp))
              | SrcOp.Select(ty, i) => assign (DstOp.Select(cvtTy ty, i))
              | SrcOp.Subscript ty => assign (DstOp.Subscript(cvtTy ty))
              | SrcOp.MkDynamic(ty, n) => assign (DstOp.MkDynamic(cvtTy ty, n))
              | SrcOp.Append ty => assign (DstOp.Append(cvtTy ty))
              | SrcOp.Prepend ty => assign (DstOp.Prepend(cvtTy ty))
              | SrcOp.Concat ty => assign (DstOp.Concat(cvtTy ty))
              | SrcOp.Range => assign DstOp.Range
              | SrcOp.Length ty => assign (DstOp.Length(cvtTy ty))
              | SrcOp.SphereQuery(dim, ty) => assign (DstOp.SphereQuery(dim, cvtTy ty))
              | SrcOp.Ceiling d => assign (DstOp.Ceiling d)
              | SrcOp.Floor d => assign (DstOp.Floor d)
              | SrcOp.Round d => assign (DstOp.Round d)
              | SrcOp.Trunc d => assign (DstOp.Trunc d)
              | SrcOp.IntToReal => assign (DstOp.IntToReal)
              | SrcOp.RealToInt d => assign (DstOp.RealToInt d)
              | SrcOp.NumStrands set => assign (DstOp.NumStrands set)
              | SrcOp.Strands(ty, set) => assign (DstOp.Strands(cvtTy ty, set))
              | SrcOp.BuildPos s => expandBuildPos (y, s, args')
              | SrcOp.EvalKernel(d, h, k) => EvalKern.expand(y, d, h, k, args')
              | SrcOp.Kernel _ => dummy()
              | SrcOp.Transform img => assign (DstOp.Transform img)
              | SrcOp.Translate img => assign (DstOp.Translate img)
              | SrcOp.LoadVoxels(info, s) => (case args'
                   of [img, idx] => LoadVoxels.expand {
                          lhs = y, info = info, s = s, img = img, idx = idx
                        }
                    | _ => raise Fail("bogus operator " ^ SrcOp.toString rator)
                  (* end case *))
              | SrcOp.LoadVoxelsWithCtl(info, s, ctl) => (case args'
                   of [img, idx] => LoadVoxels.expandWithCtl {
                          lhs = y, info = info, s = s, ctl = ctl, img = img, idx = idx
                        }
                    | _ => raise Fail("bogus operator " ^ SrcOp.toString rator)
                  (* end case *))
              | SrcOp.Inside arg => assign (DstOp.Inside arg)
              | SrcOp.IndexInside arg => assign (DstOp.IndexInside arg)
              | SrcOp.ImageDim arg => assign (DstOp.ImageDim arg)
(* QUESTION: will we still have BorderCtl* operators at this point? *)
              | SrcOp.BorderCtlDefault info => (case args'
                   of [x] => [(y, DstIR.VAR x)]
                    | _ => raise Fail("bogus operator " ^ SrcOp.toString rator)
                  (* end case *))
              | SrcOp.BorderCtlClamp info => copy()
              | SrcOp.BorderCtlMirror info => copy()
              | SrcOp.BorderCtlWrap info => copy()
              | SrcOp.LoadSeq(ty, nrrd) => assign (DstOp.LoadSeq(cvtTy ty, nrrd))
              | SrcOp.LoadImage(ty, nrrd) => assign (DstOp.LoadImage(cvtTy ty, nrrd))
              | SrcOp.MathFn f => assign (DstOp.MathFn f)
              | rator => raise Fail("bogus operator " ^ SrcOp.toString rator)
            (* end case *)
          end

  (* expand a SrcIR assignment to a DstIR CFG *)
    fun expand (env, (y, rhs)) = let
          val y' = Env.rename (env, y)
          fun assign rhs = [DstIR.ASSGN(y', rhs)]
          in
            case rhs
             of SrcIR.GLOBAL x => assign (DstIR.GLOBAL(Env.renameGV(env, x)))
              | SrcIR.STATE(NONE, fld) => assign (DstIR.STATE(NONE, Env.renameSV(env, fld)))
              | SrcIR.STATE(SOME x, fld) =>
                  assign (DstIR.STATE(SOME(Env.rename(env, x)), Env.renameSV(env, fld)))
              | SrcIR.VAR x => assign (DstIR.VAR(Env.rename(env, x)))
              | SrcIR.LIT lit => assign (DstIR.LIT lit)
              | SrcIR.OP(rator, args) => List.map DstIR.ASSGN (expandOp (env, y', rator, args))
              | SrcIR.CONS(args, ty) => assign (DstIR.CONS(Env.renameList(env, args), cvtTy ty))
              | SrcIR.SEQ(args, ty) => assign (DstIR.SEQ(Env.renameList(env, args), cvtTy ty))
              | SrcIR.EINAPP(rator, args) => (
                  EinToLow.expand (y', rator, Env.renameList(env, args))
                    handle ex => (
                      print (concat [
                          "MidToLow.expand: error converting ", EinPP.toString rator, "(",
                          String.concatWithMap "," SrcIR.Var.name args, ")\n"
                        ]);
                      raise ex))
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
             of SrcIR.OP(SrcOp.EigenVecs2x2, xs) => mkOP (DstOp.EigenVecs2x2, xs)
              | SrcIR.OP(SrcOp.EigenVecs3x3, xs) => mkOP (DstOp.EigenVecs3x3, xs)
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

    fun translate prog = let
          val prog = Trans.translate prog
          in
            LowCensus.init prog;
            prog
          end

  end
