(* check-high.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure CheckOps : OPERATOR_TY = struct

    structure Op = HighOps
    structure Ty = HighTypes

    type rator = Op.rator
    type ty = Ty.ty

    fun chkIndex (idx, bnd) = ((0 <= idx) andalso (idx < bnd))

  (* Return the signature of a HighIL operator. *)
    fun sigOf rator = (case rator
           of Op.IAdd => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.ISub => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.IMul => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.IDiv => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.IMod => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.INeg => (Ty.IntTy, [Ty.IntTy])
            | Op.IAbs => (Ty.IntTy, [Ty.IntTy])
            | Op.LT ty => (Ty.BoolTy, [ty, ty])
            | Op.LTE ty => (Ty.BoolTy, [ty, ty])
            | Op.EQ ty => (Ty.BoolTy, [ty, ty])
            | Op.NEQ ty => (Ty.BoolTy, [ty, ty])
            | Op.GT ty => (Ty.BoolTy, [ty, ty])
            | Op.GTE ty => (Ty.BoolTy, [ty, ty])
            | Op.Power => (Ty.realTy, [Ty.realTy, Ty.IntTy])
            | Op.BAnd => (Ty.BoolTy, [Ty.BoolTy, Ty.BoolTy])
            | Op.BOr => (Ty.BoolTy, [Ty.BoolTy, Ty.BoolTy])
            | Op.BNot => (Ty.BoolTy, [Ty.BoolTy])
            | Op.Max ty => (ty, [ty, ty])
            | Op.Min ty => (ty, [ty, ty])
            | Op.Zero ty => (ty, [])
            | Op.TensorIndex(ty as Ty.TensorTy shp, idxs) =>
                if ListPair.allEq chkIndex (idxs, shp)
                  then (Ty.realTy, [ty])
                  else raise Fail("sigOf: invalid index in operator " ^ Op.toString rator)
            | Op.Select(ty as Ty.TupleTy tys, i) =>
                if (1 <= i) andalso (i <= length tys)
                  then (List.nth(tys, i-1), [ty])
                  else raise Fail("sigOf: invalid operator " ^ Op.toString rator)
            | Op.Subscript(ty as Ty.SeqTy(elemTy, _)) => (elemTy, [ty, Ty.IntTy])
            | Op.MkDynamic(ty, n) => (Ty.SeqTy(ty, NONE), [Ty.SeqTy(ty, SOME n)])
            | Op.Prepend ty => (Ty.SeqTy(ty, NONE), [ty, Ty.SeqTy(ty, NONE)])
            | Op.Append ty => (Ty.SeqTy(ty, NONE), [Ty.SeqTy(ty, NONE), ty])
            | Op.Concat ty => (Ty.SeqTy(ty, NONE), [Ty.SeqTy(ty, NONE), Ty.SeqTy(ty, NONE)])
            | Op.Range => (Ty.SeqTy(Ty.intTy, NONE), [Ty.IntTy, Ty.IntTy])
            | Op.Length ty => (Ty.intTy, [Ty.SeqTy(ty, NONE)])
            | Op.SphereQuery(1, strandTy) =>
                (Ty.SeqTy(strandTy, NONE), [Ty.realTy, Ty.realTy])
            | Op.SphereQuery(dim, strandTy) =>
                (Ty.SeqTy(strandTy, NONE), [Ty.TensorTy[dim], Ty.realTy])
            | Op.IntToReal => (Ty.realTy, [Ty.IntTy])
            | Op.TruncToInt => (Ty.IntTy, [Ty.realTy])
            | Op.RoundToInt => (Ty.IntTy, [Ty.realTy])
            | Op.CeilToInt => (Ty.IntTy, [Ty.realTy])
            | Op.FloorToInt => (Ty.IntTy, [Ty.realTy])
            | Op.NumStrands _ => (Ty.IntTy, [])
            | Op.Strands(strandTy, _) => (Ty.SeqTy(strandTy, NONE), [])
            | Op.Kernel _ => (Ty.KernelTy, [])
            | Op.Inside(info, _) => (Ty.BoolTy, [Ty.vecTy(ImageInfo.dim info), Ty.ImageTy info])
            | Op.ImageDim(info, _) => (Ty.IntTy, [Ty.ImageTy info])
            | Op.BorderCtlDefault info =>
                (Ty.ImageTy info, [Ty.ImageTy info, Ty.TensorTy(ImageInfo.voxelShape info)])
            | Op.BorderCtlClamp info => (Ty.ImageTy info, [Ty.ImageTy info])
            | Op.BorderCtlMirror info => (Ty.ImageTy info, [Ty.ImageTy info])
            | Op.BorderCtlWrap info => (Ty.ImageTy info, [Ty.ImageTy info])
            | Op.LoadSeq(ty, _) => (ty, [])
            | Op.LoadImage(ty, _) => (ty, [])
            | Op.MathFn f => MathFns.sigOf (Ty.realTy, f)
            | _ => raise Fail("sigOf: invalid operator " ^ Op.toString rator)
          (* end case *))

  (* utility function for synthesizing eigenvector/eigenvalue signature *)
    fun eigenSig dim = let
          val resTy = [
                  Ty.SeqTy(Ty.realTy, SOME dim),
                  Ty.SeqTy(Ty.vecTy dim, SOME dim)
                ]
          in
            (resTy, [Ty.TensorTy[dim, dim]])
          end

    fun msigOf rator = (case rator
           of Op.Eigen2x2 => eigenSig 2
            | Op.Eigen3x3 => eigenSig 3
            | Op.KillAll => ([], [])
            | Op.StabilizeAll => ([], [])
            | Op.Print tys => ([], tys)
            | _ => raise Fail("msigOf: invalid operator " ^ Op.toString rator)
          (* end case *))

    fun typeOfCons (Ty.TensorTy dd', (ty1 as Ty.TensorTy dd)::r) =
          if List.all (fn ty => Ty.same(ty1, ty)) r
            then (dd' = (List.length r + 1)::dd)
            else false
      | typeOfCons _ = false

    fun typeOfSeq (Ty.SeqTy(ty, NONE), tys) = List.all (fn ty' => Ty.same(ty, ty')) tys
      | typeOfSeq (Ty.SeqTy(ty, SOME n), tys) =
          List.all (fn ty' => Ty.same(ty, ty')) tys andalso (List.length tys = n)
      | typeOfSeq _ = false

    fun isStrandTy (Ty.StrandTy _) = true
      | isStrandTy _ = false

    fun isBoolTy Ty.BoolTy = true
      | isBoolTy _ = false

  end

structure CheckHigh = CheckIRFn (
    structure IR = HighIR
    structure OpTy = CheckOps)

structure HighPP = SSAPPFn (HighIR)
