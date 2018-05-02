(* index-ctl.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Border controls that can be implemented in terms of remapping image indices.
 *)

structure IndexCtl =
  struct

    datatype t
      = Clamp           (* clamp indices to image dimensions *)
      | Mirror          (* reflect indices at border *)
      | Wrap            (* wrap indices at border *)

  (* NOTE: these strings should match the names of the member functions in image.cxx *)
    fun toString Clamp = "clamp"
      | toString Mirror = "mirror"
      | toString Wrap = "wrap"

    fun same (c1 : t, c2) = (c1 = c2)

    fun hash Clamp = 0w17
      | hash Mirror = 0w23
      | hash Wrap = 0w37

  end
