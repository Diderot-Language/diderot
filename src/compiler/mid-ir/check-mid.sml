(* check-mid.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure CheckOps : OPERATOR_TY = struct

    structure Op = MidOps
    structure Ty = MidTypes

    type rator = Op.rator
    type ty = Ty.ty

    val vec3Ty = Ty.vecTy 3

  (* the type of the indices for an image; note that 1D images are indexed by 1-element
   * sequences.
   *)
    fun indexType info = (case ImageInfo.dim info
           of 1 => Ty.IntTy
            | d => Ty.SeqTy(Ty.IntTy, SOME d)
          (* end case *))

  (* utility function for determining the type of LoadVoxels and LoadVoxelsWithCtl *)
    fun loadVoxelsSig (info, support) = let
          val shp = ImageInfo.voxelShape info
          val dim = ImageInfo.dim info
          val resTy = Ty.TensorTy(shp @ List.tabulate(dim, fn _ => support))
          in
            (resTy, [Ty.ImageTy info, indexType info])
          end

    fun chkIndex (idx, bnd) = ((0 <= idx) andalso (idx < bnd))

  (* Return the signature of a MidIR operator. *)
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
            | Op.BAnd => (Ty.BoolTy, [Ty.BoolTy, Ty.BoolTy])
            | Op.BOr => (Ty.BoolTy, [Ty.BoolTy, Ty.BoolTy])
            | Op.BNot => (Ty.BoolTy, [Ty.BoolTy])
            | Op.Max ty => (ty, [ty, ty])
            | Op.Min ty => (ty, [ty, ty])
            | Op.EigenVals2x2 => (Ty.SeqTy(Ty.realTy, SOME 2), [Ty.TensorTy[2,2]])
            | Op.EigenVals3x3 => (Ty.SeqTy(Ty.realTy, SOME 3), [Ty.TensorTy[3,3]])
            | Op.Zero ty => (ty, [])
            | Op.TensorIndex(ty as Ty.TensorTy shp, idxs) =>
                if ListPair.allEq chkIndex (idxs, shp)
                  then (Ty.realTy, [ty])
                  else raise Fail("sigOf: invalid index in operator " ^ Op.toString rator)
            | Op.Select(ty as Ty.TupleTy tys, i) => (List.nth(tys, i-1), [ty])
            | Op.Subscript(ty as Ty.SeqTy(elemTy, _)) => (elemTy, [ty, Ty.intTy])
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
            | Op.Ceiling d => (Ty.vecTy d, [Ty.vecTy d])
            | Op.Floor d => (Ty.vecTy d, [Ty.vecTy d])
            | Op.Round d => (Ty.vecTy d, [Ty.vecTy d])
            | Op.Trunc d => (Ty.vecTy d, [Ty.vecTy d])
            | Op.IntToReal => (Ty.realTy, [Ty.intTy])
            | Op.RealToInt 1 => (Ty.IntTy, [Ty.realTy])
            | Op.RealToInt d => (Ty.SeqTy(Ty.IntTy, SOME d), [Ty.TensorTy[d]])
            | Op.NumStrands _ => (Ty.IntTy, [])
            | Op.Strands(strandTy, _) => (Ty.SeqTy(strandTy, NONE), [])
            | Op.BuildPos s => (Ty.TensorTy[2*s], [Ty.realTy])
            | Op.EvalKernel(d, _, _) => (Ty.TensorTy[d], [Ty.TensorTy[d]])
            | Op.Kernel _ => (Ty.KernelTy, [])
            | Op.Transform info => let
                val dim = ImageInfo.dim info
                in
                  if (dim = 1)
                    then (Ty.TensorTy[], [Ty.ImageTy info])
                    else (Ty.TensorTy[dim, dim], [Ty.ImageTy info])
                end
            | Op.Translate info => let
                val dim = ImageInfo.dim info
                in
                  if (dim = 1)
                    then (Ty.TensorTy[], [Ty.ImageTy info])
                    else (Ty.TensorTy[dim], [Ty.ImageTy info])
                end
            | Op.LoadVoxels(info, s) => loadVoxelsSig (info, s)
            | Op.LoadVoxelsWithCtl(info, s, _) => loadVoxelsSig (info, s)
            | Op.Inside(info, _) => (Ty.BoolTy, [Ty.vecTy(ImageInfo.dim info), Ty.ImageTy info])
            | Op.IndexInside(info, _) => (Ty.BoolTy, [indexType info, Ty.ImageTy info])
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
           of Op.EigenVecs2x2 => eigenSig 2
            | Op.EigenVecs3x3 => eigenSig 3
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

structure CheckMid = CheckIRFn (
    structure IR = MidIR
    structure OpTy = CheckOps)

structure MidPP = SSAPPFn (MidIR)
