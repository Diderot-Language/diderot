(* field-def.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure FieldDef =
  struct

  (* the static definition of a field value *)
    datatype field_def
      = CONV of int * ImageInfo.t * Kernel.t (* CONV(k, V, h) represents D^k(V*h) *)
      | NEG of field_def
      | SUM of field_def * field_def
(* scaling too? *)

  (* base convolution case *)
    fun convolve (v, h) = CONV(0, v, h)

  (* smart constructor for DIFF *)
    fun diff (CONV(k, img, h)) = CONV(k+1, img, h)
      | diff (NEG fld) = NEG(diff fld)
      | diff (SUM(fld1, fld2)) = SUM(diff fld1, diff fld2)

  (* slightly smart constructor for NEG *)
    fun neg (NEG fld) = fld
      | neg fld = (NEG fld)

  (* equality test for field definitions *)
    fun same (CONV(k1, img1, kern1), CONV(k2, img2, kern2)) =
          (k1 = k2) andalso ImageInfo.same(img1, img2) andalso Kernel.same(kern1, kern2)
      | same (NEG fld1, NEG fld2) = same(fld1, fld2)
      | same (SUM(fld11, fld12), SUM(fld21, fld22)) = 
          same(fld11, fld21) andalso same(fld12, fld22)

  (* hash value *)
    fun hash (CONV(k, img, kern)) =
          Word.fromInt k * 0w17 + 0w3 * ImageInfo.hash img + Kernel.hash kern
      | hash (NEG fld) = 0w3 * hash fld + 0w11
      | hash (SUM(fld1, fld2)) = 0w7 * hash fld1 + hash fld2

    fun toString (CONV(0, img, kern)) =
          concat["<", ImageInfo.toString img, "*", Kernel.name kern, ">"]
      | toString (CONV(k, img, kern)) =
          concat["(D", Int.toString k, " <", ImageInfo.toString img, "*", Kernel.name kern, ">)"]
      | toString (NEG fld) = "-" ^ toString fld
      | toString (SUM(fld1, NEG fld2)) =
          concat["(", toString fld1, "-", toString fld2, ")"]
      | toString (SUM(fld1, fld2)) =
          concat["(", toString fld1, "+", toString fld2, ")"]

  end
