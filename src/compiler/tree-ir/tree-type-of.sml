(* tree-type-of.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TreeTypeOf : sig

    val exp : TreeIR.exp -> TreeTypes.t

  end = struct

    structure IR = TreeIR
    structure Ty = TreeTypes
    structure Op = TreeOps

    fun operator rator = (case rator
           of Op.IAdd => Ty.IntTy
            | Op.ISub => Ty.IntTy
            | Op.IMul => Ty.IntTy
            | Op.IDiv => Ty.IntTy
            | Op.IMod => Ty.IntTy
            | Op.INeg => Ty.IntTy
            | Op.RAdd => Ty.realTy
            | Op.RSub => Ty.realTy
            | Op.RMul => Ty.realTy
            | Op.RDiv => Ty.realTy
            | Op.RNeg => Ty.realTy
            | Op.RClamp => Ty.realTy
            | Op.RLerp => Ty.realTy
            | Op.RCeiling => Ty.realTy
            | Op.RFloor => Ty.realTy
            | Op.RRound => Ty.realTy
            | Op.RTrunc => Ty.realTy
            | Op.RealToInt => Ty.IntTy
            | Op.LT ty => Ty.BoolTy
            | Op.LTE ty => Ty.BoolTy
            | Op.EQ ty => Ty.BoolTy
            | Op.NEQ ty => Ty.BoolTy
            | Op.GT ty => Ty.BoolTy
            | Op.GTE ty => Ty.BoolTy
            | Op.BAnd => Ty.BoolTy
            | Op.BOr => Ty.BoolTy
            | Op.BNot => Ty.BoolTy
            | Op.Abs ty => ty
            | Op.Max ty => ty
            | Op.Min ty => ty
            | Op.VAdd d => Ty.VecTy d
            | Op.VSub d => Ty.VecTy d
            | Op.VScale d => Ty.VecTy d
            | Op.VMul d => Ty.VecTy d
            | Op.VNeg d => Ty.VecTy d
            | Op.VSum d => Ty.realTy
            | Op.VDot d => Ty.realTy
            | Op.VIndex(d, pw, _) => Ty.realTy
            | Op.VCeiling d => Ty.VecTy d
            | Op.VFloor d => Ty.VecTy d
            | Op.VRound d => Ty.VecTy d
            | Op.VTrunc d => Ty.VecTy d
            | Op.VToInt{wid, ...} => Ty.SeqTy(Ty.IntTy, SOME wid)
            | Op.TensorIndex(ty, _) => Ty.realTy
            | Op.ProjectLast(ty as Ty.TensorTy dd, _) => Ty.TensorRefTy[List.last dd]
            | Op.ProjectLast(ty as Ty.TensorRefTy dd, _) => Ty.TensorRefTy[List.last dd]
            | Op.TensorCopy shp => Ty.TensorTy shp
            | Op.TensorRef shp => Ty.TensorRefTy shp
            | Op.EigenVecs2x2 => Ty.TupleTy[
                  Ty.SeqTy(Ty.realTy, SOME 2),
                  Ty.SeqTy(Ty.TensorTy[2], SOME 2)
                ]
            | Op.EigenVecs3x3 => Ty.TupleTy[
                  Ty.SeqTy(Ty.realTy, SOME 3),
                  Ty.SeqTy(Ty.TensorTy[3], SOME 3)
                ]
            | Op.EigenVals2x2 => Ty.SeqTy(Ty.realTy, SOME 2)
            | Op.EigenVals3x3 => Ty.SeqTy(Ty.realTy, SOME 3)
            | Op.Select(ty as Ty.TupleTy tys, i) => List.nth(tys, i-1)
            | Op.Subscript(Ty.SeqTy(Ty.TensorTy(shp as _::_), _)) => Ty.TensorRefTy shp
            | Op.Subscript(Ty.SeqTy(elemTy, _)) => elemTy
            | Op.MkDynamic(ty, n) => Ty.SeqTy(ty, NONE)
            | Op.Prepend(ty, _) => Ty.SeqTy(ty, NONE)
            | Op.Append(ty, _) => Ty.SeqTy(ty, NONE)
            | Op.Concat ty => Ty.SeqTy(ty, NONE)
            | Op.Range => Ty.SeqTy(Ty.intTy, NONE)
            | Op.Length ty => Ty.intTy
            | Op.SphereQuery(_, strandTy) => Ty.SeqTy(strandTy, NONE)
            | Op.Sqrt => Ty.realTy
            | Op.Cos => Ty.realTy
            | Op.ArcCos => Ty.realTy
            | Op.Sin => Ty.realTy
            | Op.ArcSin => Ty.realTy
            | Op.Tan => Ty.realTy
            | Op.ArcTan => Ty.realTy
            | Op.Exp => Ty.realTy
            | Op.Sign => Ty.realTy
            | Op.IntToReal => Ty.realTy
            | Op.NumStrands _ => Ty.IntTy
            | Op.Transform info => let
                val dim = ImageInfo.dim info
                in
                  if (dim = 1)
                    then Ty.realTy
                    else Ty.TensorRefTy[dim, dim]
                end
            | Op.Translate info => let
                val dim = ImageInfo.dim info
                in
                  if (dim = 1)
                    then Ty.realTy
                    else Ty.TensorRefTy[dim]
                end
            | Op.ControlIndex(info, _, _) => Ty.IntTy
            | Op.LoadVoxel info => Ty.realTy
            | Op.Inside _ => Ty.BoolTy
            | Op.ImageDim _ => Ty.IntTy
            | Op.MathFn f => Ty.realTy
            | _ => raise Fail("invalid operator " ^ Op.toString rator)
          (* end case *))

    fun exp e = (case e
           of IR.E_Global gv => TreeGlobalVar.ty gv
            | IR.E_State(_, sv) => TreeStateVar.ty sv
            | IR.E_Var x => TreeVar.ty x
            | IR.E_Lit(Literal.Int _) => Ty.IntTy
            | IR.E_Lit(Literal.Real _) => Ty.realTy
            | IR.E_Lit(Literal.String _) => Ty.StringTy
            | IR.E_Lit(Literal.Bool _) => Ty.BoolTy
            | IR.E_Op(rator, _) => operator rator
            | IR.E_Apply(f, _) => #1(TreeFunc.ty f)
            | IR.E_Vec(w, pw, _) => Ty.VecTy(w, pw)
            | IR.E_Cons(_, ty) => ty
            | IR.E_Seq(_, ty) => ty
            | IR.E_Pack(layout, _) => Ty.TensorTy[#wid layout]
            | IR.E_VLoad(layout, _, i) => Ty.nthVec(layout, i)
          (* end case *))

  end
