(* map-reduce-opt.sml
 *
 * Optimize global map-reduce computations that can occur in the global "start" and
 * "update" blocks.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure MapReduceOpt : sig

    val transform : Simple.program -> Simple.program

  end = struct

    structure S = Simple
    structure ST = Stats

  (* optimize global reductions; return NONE if there was no change *)
    fun doGlobalPhase NONE = NONE
      | doGlobalPhase (SOME blk) = NONE (* FIXME *)

    fun transform prog = let
          val S.Program{
                  props, consts, inputs, constInit, globals, globInit, funcs,
                  strand, create, start, update
                } = prog
          fun result (start', update') = S.Program{
                  props = props, consts = consts, inputs = inputs,
                  constInit = constInit, globals = globals, globInit = globInit,
                  funcs = funcs, strand = strand, create = create,
                  start = start', update = update'
                }
          in
            case (doGlobalPhase start, doGlobalPhase update)
             of (NONE, NONE) => prog
              | (SOME start', NONE) => result (start', update)
              | (NONE, SOME update') => result (start, update')
              | (SOME start', SOME update') => result (start', update')
            (* end case *)
          end

  end
