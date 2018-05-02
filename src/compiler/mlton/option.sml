(* option.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

signature OPTION_EXT =
  sig

    include OPTION

    val isNone : 'a option -> bool

    val fold : ('a * 'b -> 'b) -> 'b -> 'a option -> 'b

  end

structure OptionExt : OPTION_EXT =
  struct

    open Option

    fun isNone NONE = true
      | isNone (SOME _) = false

    fun fold f x NONE = x
      | fold f x (SOME y) = f(y, x)

  end
