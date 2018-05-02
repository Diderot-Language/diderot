(* global-var-kind.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure GlobalVarKind : sig

    datatype t = ConstVar | InputVar | OutputVar | GlobalVar

    val toString : t -> string
    val same : t * t -> bool

  end = struct

    datatype t = ConstVar | InputVar | OutputVar | GlobalVar

    fun toString ConstVar = "const"
      | toString InputVar = "input"
      | toString OutputVar = "output"
      | toString GlobalVar = "global"

    fun same (k1 : t, k2) = (k1 = k2)

  end
