(* keywords.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *
 * Diderot keywords for the current version of the syntax.
 *)

structure Keywords : sig

    val idToken : string -> DiderotTokens.token

  end = struct

    structure T = DiderotTokens

    val keywords = [
            ("bool",            T.KW_bool),
            ("const",           T.KW_const),
            ("continue",        T.KW_continue),
            ("create_array",    T.KW_create_array),
            ("create_collection", T.KW_create_collection),
            ("die",             T.KW_die),
            ("else",            T.KW_else),
            ("false",           T.KW_false),
            ("field",           T.KW_field),
            ("foreach",         T.KW_foreach),
            ("function",        T.KW_function),
            ("identity",        T.KW_identity),
            ("if",              T.KW_if),
            ("image",           T.KW_image),
            ("in",              T.KW_in),
            ("inf",             T.REAL RealLit.posInf),
            ("initialize",      T.KW_initialize),
            ("input",           T.KW_input),
            ("int",             T.KW_int),
            ("kernel",          T.KW_kernel),
            ("load_image",      T.KW_load_image),
            ("load_sequence",   T.KW_load_sequence),
            ("mat2",            T.KW_mat2),
            ("mat3",            T.KW_mat3),
            ("mat4",            T.KW_mat4),
            ("nan",             T.KW_nan),
            ("new",             T.KW_new),
            ("output",          T.KW_output),
            ("print",           T.KW_print),
            ("real",            T.KW_real),
            ("return",          T.KW_return),
            ("stabilize",       T.KW_stabilize),
            ("start",           T.KW_start),
            ("strand",          T.KW_strand),
            ("string",          T.KW_string),
            ("tensor",          T.KW_tensor),
            ("true",            T.KW_true),
            ("update",          T.KW_update),
            ("vec2",            T.KW_vec2),
            ("vec3",            T.KW_vec3),
            ("vec4",            T.KW_vec4),
            ("zeros",           T.KW_zeros)
          ]

  (* create a keyword lookup table *)
    local
      fun mkFind kws = let
            val tbl = AtomTable.mkTable (64, Fail "keywords")
            fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
            val find = AtomTable.find tbl
            fun idToken id = let
                  val id = Atom.atom id
                  in
                    case find id
                     of NONE => T.ID id
                      | SOME kw => kw
                    (* end case *)
                  end
            in
              List.app ins kws;
              idToken
            end
    in
  (* return either a keyword token or an ID token *)
      val idToken = mkFind keywords
    end

  end
