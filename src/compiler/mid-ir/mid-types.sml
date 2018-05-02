(* mid-types.sml
 *
 * Types for the MidIR.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure MidTypes =
  struct

    datatype ty
      = BoolTy | StringTy | IntTy
      | TensorTy of int list            (* tensor types, which include reals, vectors, etc. *)
      | TupleTy of ty list              (* tuples; used for multiple return values *)
      | SeqTy of ty * int option
      | ImageTy of ImageInfo.t
      | StrandTy of Atom.atom
      | KernelTy

    val intTy = IntTy
    val realTy = TensorTy[]
    fun vecTy 1 = realTy
      | vecTy n = TensorTy[n]
    fun iVecTy 1 = IntTy
      | iVecTy n = SeqTy(IntTy, SOME n)

  (* smart constructor for tensor type that prunes out dimensions with size 1 *)
    fun tensorTy dd = TensorTy(List.mapPartial (fn 1 => NONE | d => SOME d) dd)

    fun same (BoolTy, BoolTy) = true
      | same (StringTy, StringTy) = true
      | same (IntTy, IntTy) = true
      | same (TensorTy dd1, TensorTy dd2) = (dd1 = dd2)
      | same (TupleTy tys1, TupleTy tys2) = ListPair.allEq same (tys1, tys2)
      | same (SeqTy(ty1, NONE), SeqTy(ty2, NONE)) = same(ty1, ty2)
      | same (SeqTy(ty1, SOME n1), SeqTy(ty2, SOME n2)) = (n1 = n2) andalso same(ty1, ty2)
      | same (ImageTy info1, ImageTy info2) = ImageInfo.sameShape(info1, info2)
      | same (StrandTy n1, StrandTy n2) = Atom.same(n1, n2)
      | same (KernelTy, KernelTy) = true
      | same _ = false

    fun hash BoolTy = 0w1
      | hash StringTy = 0w2
      | hash IntTy = 0w3
      | hash (TensorTy dd) = List.foldl (fn (d, s) => 0w11 * Word.fromInt d + s) 0w5 dd
      | hash (TupleTy tys) = List.foldl (fn (ty, s) => hash ty + s) 0w7 tys
      | hash (SeqTy(ty, NONE)) = hash ty + 0w11
      | hash (SeqTy(ty, SOME n)) = Word.fromInt n * hash ty + 0w13
      | hash (ImageTy info) = 0w13 * ImageInfo.hash info + 0w6
      | hash (StrandTy n) = Atom.hash n
      | hash KernelTy = 0w17

    fun toString BoolTy = "bool"
      | toString StringTy = "string"
      | toString IntTy = "int"
      | toString (TensorTy[]) = "real"
      | toString (TensorTy dd) = String.concat[
            "tensor[", String.concatWithMap "," Int.toString dd, "]"
          ]
      | toString (TupleTy tys) = String.concat[
            "(", String.concatWithMap " * " toString tys, ")"
          ]
      | toString (SeqTy(ty, NONE)) = toString ty ^ "[]"
      | toString (SeqTy(ty, SOME n)) = concat[toString ty, "[", Int.toString n, "]"]
      | toString (ImageTy info) = concat["image(", ImageInfo.toString info, ")"]
      | toString (StrandTy n) = Atom.toString n
      | toString KernelTy = "kernel"

  end
