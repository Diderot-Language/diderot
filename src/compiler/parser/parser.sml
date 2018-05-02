(* parser.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Parser glue.
 *)

structure Parser : sig

  (* parse a file; return NONE if there are syntax errors *)
    val parseFile : (Error.err_stream * TextIO.instream)
          -> (ParseTree.version * ParseTree.program) option

  end = struct

  (* default version of syntax *)
    val defaultVersion = [1, 0]
    val defaultName = String.concatWithMap "." Int.toString defaultVersion

  (* get the "#version" tag from the first line.  If not present, then return the default
   * version.
   * We do this using functional I/O, so the parser will also have to accept the version
   * tag (if present).
   *)
    fun getVersion (errStrm, inStrm) = (
          case TextIO.StreamIO.inputLine(TextIO.getInstream inStrm)
           of SOME(ln, _) => if (String.isPrefix "#version" ln)
                then let
                (* rest of line after "#version" prefix *)
                  val rest = Substring.extract(ln, 8, NONE)
                (* strip leading and trailing whitespace *)
                  val vers = Substring.dropl Char.isSpace (Substring.dropr Char.isSpace rest)
                (* convert a field to an integer *)
                  fun cvt ss = Option.map #1 (Int.scan StringCvt.DEC Substring.getc ss)
                  in
                    case List.map cvt (Substring.fields (fn #"." => true | _ => false) vers)
                     of [SOME m] => SOME[m, 0]
                      | [SOME m, SOME n] => SOME[m, n]
                      | _ => (
                          Error.error (errStrm, ["invalid '#version' directive"]);
                          NONE)
                    (* end case *)
                  end
                else (
                  Error.warning (errStrm, [
                      "no language version specified; assuming '#version ",
                      defaultName, "'"
                    ]);
                  SOME defaultVersion)
            | NONE => (
                Error.error (errStrm, ["empty source file"]);
                NONE)
          (* end case *))

    datatype add_or_delete = datatype AntlrRepair.add_or_delete

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (***** version 1.0 parser *****)
    local
    (* error function for parsers *)
      fun parseErr errStrm (pos, _) = Error.errorAt (errStrm, (pos, pos), ["syntax error"])
    (* glue together the lexer and parser *)
      structure DiderotParser = Diderot100ParseFn(Diderot100Lex)
    in
    fun parser100 (errStrm, file) = let
          fun get () = TextIO.input file
          val lexer = Diderot100Lex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          val (res, _, errs) = DiderotParser.parse lexer (Diderot100Lex.streamify get)
          in
            List.app (parseErr errStrm) errs;
            res
          end
    end (* local *)

  (***** version 2.0 parser *****)
    local
    (* map tokens to strings; when adding a token, we use a generic name where it makes sense *)
      fun tokToString ADD (DiderotTokens.ID _) = "<identifier>"
        | tokToString DEL (DiderotTokens.ID x) = Atom.toString x
        | tokToString _ (DiderotTokens.STRING s) = concat["\"", String.toCString s, "\""]
        | tokToString ADD (DiderotTokens.REAL f) = "<number>"
        | tokToString DEL (DiderotTokens.REAL f) = RealLit.toString f
        | tokToString ADD (DiderotTokens.INT i) = "<integer>"
        | tokToString DEL (DiderotTokens.INT i) = IntLit.toString i
        | tokToString ADD DiderotTokens.KW_vec2 = "<type>"
        | tokToString ADD DiderotTokens.KW_vec3 = "<type>"
        | tokToString ADD DiderotTokens.KW_vec4 = "<type>"
        | tokToString ADD DiderotTokens.KW_mat2 = "<type>"
        | tokToString ADD DiderotTokens.KW_mat3 = "<type>"
        | tokToString ADD DiderotTokens.KW_mat4 = "<type>"
        | tokToString ADD DiderotTokens.KW_real = "<type>"
        | tokToString ADD DiderotTokens.KW_int = "<type>"
        | tokToString ADD DiderotTokens.KW_bool = "<type>"
        | tokToString _ tok = DiderotTokens.toString tok

    (* error function for parsers *)
      fun parseErr errStrm (pos, _) = Error.errorAt (errStrm, (pos, pos), ["syntax error"])
(* FIXME: need better support for error-message tuning in ML-Antlr
      val parseErr = Error.parseError tokToString
*)

    (* glue together the lexer and parser *)
      structure DiderotParser = DiderotParseFn(DiderotLex)
    in
    fun parser200 (errStrm, file) = let
          fun get () = TextIO.input file
          val lexer = DiderotLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
          val (res, _, errs) = DiderotParser.parse lexer (DiderotLex.streamify get)
          in
            List.app (parseErr errStrm) errs;
            res
          end
    end (* local *)

  (* select a parser based on the syntax version *)
    local
      val tbl = [
              ([1, 0], parser100),
              ([2, 0], parser200)
            ]
      fun lookup vers =
            Option.map #2 (List.find (fn (v, _) => ListPair.allEq (op =) (v, vers)) tbl)
    in
    fun selectParser (errStrm, vers) = (case lookup vers
           of NONE => (
                Error.error (errStrm, [
                    "no support for Diderot version ",
                    String.concatWithMap "." Int.toString vers
                  ]);
                NONE)
            | someParser => someParser
          (* end case *))
    end (* local *)

  (* parse a file, returning a parse tree and the language version *)
    fun parseFile (errStrm, file) = (case getVersion (errStrm, file)
           of SOME vers => (case selectParser (errStrm, vers)
                 of SOME parser => (case parser (errStrm, file)
                       of SOME prog => SOME(vers, prog)
                        | NONE => NONE
                      (* end case *))
                  | NONE => NONE
                (* end case *))
            | NONE => NONE
          (* end case *))

  end
