(* diderot.lex
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

%name DiderotLex;

%arg (lexErr);

%defs(

    structure T = DiderotTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref will be used for keeping track of comment depth *)
    val depth = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* make a string from buf *)
    fun mkString () = let
          val s = String.concat(List.rev(!buf))
          in
            buf := [];
            T.STRING s
          end

  (* make a REAL token from a substring.  The argument should match the RE
   *
   *    {num}"."{num}([eE][+-]?{num})?
   *)
    fun mkReal ss = let
          val (isNeg, rest) = (case Substring.getc ss
                 of SOME(#"-", r) => (true, r)
                  | SOME(#"+", r) => (false, r)
                  | _ => (false, ss)
                (* end case *))
          val (whole, rest) = Substring.splitl Char.isDigit rest
          val rest = Substring.triml 1 rest (* remove "." *)
          val (frac, rest) = Substring.splitl Char.isDigit rest
          val exp = if Substring.isEmpty rest
                then 0
                else let
                  val rest = Substring.triml 1 rest (* remove "e" or "E" *)
                  in
                    #1(valOf(IntInf.scan StringCvt.DEC Substring.getc rest))
                  end
          in
            T.REAL(RealLit.real{
                isNeg = isNeg,
                whole = Substring.string whole,
                frac = Substring.string frac,
                exp = exp
              })
          end

  (* scan a number from a hexidecimal string *)
    val fromHexString = valOf o (StringCvt.scanString (IntInf.scan StringCvt.HEX))

  (* convert a "#version" directive to a int list *)
    fun cvtVersion s = let
        (* rest of string after "#version" prefix *)
          val rest = Substring.extract(s, 8, NONE)
        (* strip leading whitespace *)
          val vers = Substring.dropl Char.isSpace rest
        (* convert a field to an integer *)
          fun cvt ss = #1 (Option.valOf (Int.scan StringCvt.DEC Substring.getc ss))
          in
            List.map cvt (Substring.fields (fn #"." => true | _ => false) vers)
          end

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF
);

%states INITIAL STRING COM1 COM2;

%let dig = [0-9];
%let num = {dig}+;
%let hexdigit = [0-9a-fA-F];
%let hexnum = "0x"{hexdigit}+;
%let greek = [αβγζηθλμξπρστφψω];
%let letter = [a-zA-Z]|{greek};
%let idchar = {letter}|{dig}|"_"|"'";
%let id = {letter}{idchar}*;
%let ws = " "|[\t\n\v\f\r];
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let eol = "\n";
%let vers = {dig}+("."{dig}+)?;

(***** Keywords and operators *****)

<INITIAL> "#version"{ws}+{vers}
                        => (T.VERSION(cvtVersion yytext));

<INITIAL> "||"          => (T.OP_orelse);
<INITIAL> "&&"          => (T.OP_andalso);
<INITIAL> "<"           => (T.OP_lt);
<INITIAL> "<="          => (T.OP_lte);
<INITIAL> "=="          => (T.OP_eqeq);
<INITIAL> "!="          => (T.OP_neq);
<INITIAL> ">="          => (T.OP_gte);
<INITIAL> ">"           => (T.OP_gt);
<INITIAL> "+"           => (T.OP_plus);
<INITIAL> "-"           => (T.OP_minus);
<INITIAL> "*"           => (T.OP_star);
<INITIAL> "/"           => (T.OP_slash);
<INITIAL> "%"           => (T.OP_mod);
<INITIAL> "^"           => (T.OP_exp);
<INITIAL> "@"           => (T.OP_at);
<INITIAL> "⊛"           => (T.OP_convolve);     (* u229b *)
<INITIAL> "∇•"          => (T.OP_Ddot);         (* u2207, u2022 *)
<INITIAL> "∇⋅"          => (T.OP_Ddot);         (* u2207, u22c5 *)
<INITIAL> "∇⊗"          => (T.OP_Dotimes);      (* u2207, u2297 *)
<INITIAL> "∇×"          => (T.OP_curl);         (* u2207, u00d7 *)
<INITIAL> "∇"           => (T.OP_D);            (* u2207 *)
<INITIAL> "•"           => (T.OP_dot);          (* u2022 *)
<INITIAL> "⋅"           => (T.OP_dot);          (* u22c5 *)
<INITIAL> "×"           => (T.OP_cross);        (* u00d7 *)
<INITIAL> "⊗"           => (T.OP_outer);        (* u2297 *)
<INITIAL> "∘"           => (T.OP_compose);      (* u2218 *)
<INITIAL> "("           => (T.LP);
<INITIAL> ")"           => (T.RP);
<INITIAL> "["           => (T.LB);
<INITIAL> "]"           => (T.RB);
<INITIAL> "{"           => (T.LCB);
<INITIAL> "}"           => (T.RCB);
<INITIAL> ","           => (T.COMMA);
<INITIAL> ";"           => (T.SEMI);
<INITIAL> ":"           => (T.COLON);
<INITIAL> "#"           => (T.HASH);
<INITIAL> "!"           => (T.BANG);
<INITIAL> "="           => (T.OP_eq);
<INITIAL> "+="          => (T.OP_pluseq);
<INITIAL> "-="          => (T.OP_minuseq);
<INITIAL> "*="          => (T.OP_stareq);
<INITIAL> "/="          => (T.OP_slasheq);
<INITIAL> "%="          => (T.OP_modeq);
<INITIAL> "|"           => (T.BAR);
<INITIAL> "."           => (T.DOT);
<INITIAL> ".."          => (T.DOTDOT);

<INITIAL> "∞"           => (T.REAL RealLit.posInf);     (* u221e *)
<INITIAL> "π"           => (T.REAL RealLit.pi);         (* u03c0 *)

<INITIAL> {id}          => (Keywords.idToken yytext);

<INITIAL> {num}         => (T.INT(valOf (IntInf.fromString yytext)));
<INITIAL> {num}"."{num}([eE][+-]?{num})?
                        => (mkReal yysubstr);
<INITIAL> {ws}          => (skip ());

<INITIAL> "\""          => (YYBEGIN STRING; continue());

<INITIAL> .             => (lexErr(yypos, ["bad character `", String.toString yytext, "'"]);
                            continue());

(***** Strings *****)
<STRING>{esc}           => (addStr(valOf(String.fromString yytext)); continue());
<STRING>{sgood}+        => (addStr yytext; continue());
<STRING> "\""           => (YYBEGIN INITIAL; mkString());
<STRING> {eol}          => (YYBEGIN INITIAL;
                            lexErr(yypos, [
                                "unexpected end-of-line encountered in string literal"
                              ]);
                            continue());
<STRING> .              => (lexErr(yypos, [
                                "bad character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue());

(***** Comments *****)
<INITIAL> "//"          => (YYBEGIN COM1; skip());
<COM1> {eol}            => (YYBEGIN INITIAL; skip());
<COM1> .                => (skip());

<INITIAL> "/*"          => (YYBEGIN COM2; skip());
<COM2> "*/"             => (YYBEGIN INITIAL; skip());
<COM2> .                => (skip());
