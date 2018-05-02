(* tensor-shape.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TensorShape =
  struct

    type t = int list

    fun same (shp1 : t, shp2) = ListPair.allEq (op =) (shp1, shp2)

    fun hash shp =
          List.foldl (fn (i, w) => Word.xorb(Word.<<(w, 0w1), Word.fromInt i))
            (Word.fromInt(List.length shp)) shp

    fun toString shp = String.concat ["[", String.concatWithMap "," Int.toString shp, "]"]

  end
