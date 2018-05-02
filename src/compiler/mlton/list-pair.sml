(* list-pair.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

signature LIST_PAIR_EXT =
  sig

    include LIST_PAIR

    val appi : (int * 'a * 'b -> unit) -> 'a list * 'b list -> unit

  end

structure ListPairExt : LIST_PAIR_EXT =
  struct

    open ListPair

    fun appi f (xs, ys) = let
          fun appf (i, x::xs, y::ys) = (f(i, x, y); appf(i+1, xs, ys))
            | appf _ = ()
          in
            appf (0, xs, ys)
          end

  end
