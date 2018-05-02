(* create.sml
 *
 * Generic representation of the initial-strand creation code in a Diderot program.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure Create =
  struct

    datatype 'a t = Create of {
        dim : int option,
        code : 'a
      }

    fun array (d, code) = Create{dim = SOME d, code = code}

    fun collection code = Create{dim = NONE, code = code}

  (* are the strands in an array? *)
    fun isArray (Create{dim=SOME _, ...}) = true
      | isArray _ = false

  (* get the grid dimension *)
    fun arrayDim (Create{dim, ...}) = dim

  (* get the creation code *)
    fun createCode (Create{code, ...}) = code

    fun map f (Create{dim, code}) = Create{dim = dim, code = f code}
    fun app f (Create{dim, code}) = f code
    fun fold f init (Create{dim, code}) = f (code, init)

  end
