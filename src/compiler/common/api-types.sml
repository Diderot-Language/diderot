(* api-types.sml
 *
 * A representation of the types of values that can be communicated to and from a
 * Diderot program.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure APITypes =
  struct

    datatype t
      = IntTy
      | BoolTy
      | TensorTy of int list
      | StringTy
      | ImageTy of int * int list
      | SeqTy of t * int option

    val realTy = TensorTy[]

    fun toString IntTy = "int"
      | toString BoolTy = "bool"
      | toString (TensorTy[]) = "real"
      | toString (TensorTy[2]) = "vec2"
      | toString (TensorTy[3]) = "vec3"
      | toString (TensorTy[4]) = "vec4"
      | toString (TensorTy dd) = concat["tensor[", String.concatWithMap "," Int.toString dd, "]"]
      | toString StringTy = "string"
      | toString (ImageTy(d, dd)) = concat[
            "image(", Int.toString d, ")[", String.concatWithMap "," Int.toString dd, "]"
          ]
      | toString (SeqTy(ty, NONE)) = toString ty ^ "[]"
      | toString (SeqTy(ty, SOME d)) = concat[toString ty, "[", Int.toString d, "]"]

  (* does a type have a non-static size? *)
    fun hasDynamicSize StringTy = true
      | hasDynamicSize (ImageTy _) = true
      | hasDynamicSize (SeqTy(_, NONE)) = true
      | hasDynamicSize _ = false

  end
