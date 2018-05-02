(* exit-kind.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * The CFG representation has different kinds of exits, depending on the
 * context and operation.
 *)

structure ExitKind =
  struct

    datatype 'var kind
      = RETURN of 'var option   (* return from function/method *)
      | ACTIVE                  (* normal return from update method *)
      | STABILIZE               (* stabilize in update method *)
      | DIE                     (* die in update method *)
      | NEXTSTEP                (* continue in global update *)
      | UNREACHABLE             (* dummy exit node that is unreachable *)

    fun map f exit = (case exit
           of RETURN NONE => RETURN NONE
            | RETURN(SOME x) => RETURN(SOME(f x))
            | ACTIVE => ACTIVE
            | STABILIZE => STABILIZE
            | DIE => DIE
            | NEXTSTEP => NEXTSTEP
            | UNREACHABLE => UNREACHABLE
          (* end case *))

    fun toString v2s exit = (case exit
           of RETURN NONE => "RETURN"
            | RETURN(SOME x) => concat["RETURN(", v2s x, ")"]
            | ACTIVE => "ACTIVE"
            | STABILIZE => "STABILIZE"
            | DIE => "DIE"
            | NEXTSTEP => "NEXTSTEP"
            | UNREACHABLE => "UNREACHABLE"
          (* end case *))

  end
