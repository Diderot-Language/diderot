(* string.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

signature STRING_EXT =
  sig

    include STRING

    val concatWithMap : string -> ('a -> string) -> 'a list -> string

    val implodeRev : char list -> string

  end

structure StringExt : STRING_EXT =
  struct

    open String

    fun concatWithMap sep fmt xs = let
          fun lp ([], strs) = String.concat(List.rev strs)
            | lp (x::xs, strs) = lp (xs, fmt x :: sep :: strs)
          in
            case xs
             of [] => ""
              | [x] => fmt x
              | x::xr => lp (xr, [fmt x])
            (* end case *)
          end

    fun implodeRev chrs = implode(List.rev chrs)

  end
