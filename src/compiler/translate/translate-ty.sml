(* translate-ty.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure TranslateTy : sig

    val tr : SimpleTypes.ty -> HighTypes.ty

  end = struct

    structure Ty = SimpleTypes
    structure DstTy = HighTypes

    fun tr ty = (case ty
           of Ty.T_Bool => DstTy.BoolTy
            | Ty.T_Int => DstTy.IntTy
            | Ty.T_String => DstTy.StringTy
            | Ty.T_Tensor dd => let
                fun cvtDim 1 = NONE
                  | cvtDim d = SOME d
                in
                  DstTy.TensorTy(List.mapPartial cvtDim dd)
                end
            | Ty.T_Sequence(ty, optDim) => DstTy.SeqTy(tr ty, optDim)
            | Ty.T_Tuple tys => DstTy.TupleTy(List.map tr tys)
            | Ty.T_Strand n => DstTy.StrandTy n
            | Ty.T_Image info => DstTy.ImageTy info
            | Ty.T_Field fld => DstTy.FieldTy
            | Ty.T_Kernel => DstTy.KernelTy
          (* end case *))

  end
