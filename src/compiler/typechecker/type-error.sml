(* type-error.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure TypeError : sig

    datatype token
      = S of string             (* literal string *)
      | A of Atom.atom          (* atom; will print in single quotes ('...') *)
      | V of AST.var            (* AST variable; will print name in single quotes ('...') *)
      | TY of Types.ty          (* type expression *)
      | TYS of Types.ty list    (* list of types *)
      | LN of Error.location    (* source-code location; prints as a line number *)

  (* format an error message *)
    val format : token list -> string list

  (* record a warning message *)
    val warning : (Error.err_stream * Error.span) * token list -> unit

  (* record an error message *)
    val error  : (Error.err_stream * Error.span) * token list -> unit

  end = struct

    structure TU = TypeUtil

    datatype token
      = S of string | A of Atom.atom
      | V of AST.var | TY of Types.ty | TYS of Types.ty list
      | LN of Error.location

      fun tysToString tys = String.concat[
              "(", String.concatWith " * " (List.map TU.toString tys), ")"
            ]
      fun tok2str (S s) = s
        | tok2str (A a) = concat["'", Atom.toString a, "'"]
        | tok2str (V x) = concat["'", Var.nameOf x, "'"]
        | tok2str (TY ty) = TU.toString ty
        | tok2str (TYS []) = "()"
        | tok2str (TYS[ty]) = TU.toString ty
        | tok2str (TYS tys) = tysToString tys
        | tok2str (LN loc) = Error.fmt ("line %l", "<unknown>") loc

    fun format toks = List.map tok2str toks

    fun warning ((errStrm, span), toks) = Error.warningAt(errStrm, span, List.map tok2str toks)
    fun error ((errStrm, span), toks) = Error.errorAt(errStrm, span, List.map tok2str toks)

  end
