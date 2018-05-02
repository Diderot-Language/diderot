(* check-simple.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckSimple : sig

    val check : string * Simple.program -> bool

  end = struct

    structure S = Simple
    structure SV = SimpleVar

    fun check (phase, prog) = let
          val S.Program{
                  props, consts, inputs, constInit, globals, funcs,
                  globInit, strand, create, start, update
                } = prog
          in
(* FIXME *)false
          end

  end
