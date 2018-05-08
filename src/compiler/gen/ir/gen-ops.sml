(* gen-ops.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * This module implements a program for generating the Operators for the instances
 * of the IL functor.  The program has the following usage
 *
 *      gen-ops <spec-file> <template-file> <out-file>
 *
 * The format of a specification is a line-oriented file, where each line (other than
 * comments) specifies an operator using five fields, which are separated by ":".  The
 * fields are
 *
 *      name
 *      argument type           (optional)
 *      result arity
 *      arity
 *      comment                 (optional)
 *
 *  Operations with effects are denoted by a "!" as the first character of the line.
 *
 * The template file must contain a line consisting of just the string "@BODY@" (i.e.,
 * without leading/trailing whitespace).  This line is replaced with the generated
 * definitions; all other lines are passed to the output file unchanged.
 *
 * We generate the following definitions:
 *
 *      datatype rator = ...
 *      val arity : rator -> int
 *      val resultArity : rator -> int
 *      val pure : rator -> bool
 *      val same : rator * rator -> bool
 *      val hash : rator -> word
 *      val toString : rator -> toString
 *
 *)

structure GenOps : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure SS = Substring

  (* Specification file input *)

  (* operator-specification fields *)
    type op_spec = {
        name : string,
        isPure : bool,
        ty : string list,
        resArity : int,
        arity : int,
        comment : string option
      }

    fun stripWS ss = SS.dropl Char.isSpace (SS.dropr Char.isSpace ss)

    fun optField ss = if (SS.isEmpty ss) then NONE else SOME(SS.string ss)

    fun doLine ss = let
          fun doFields (isPure, ss) = (case SS.fields (fn #":" => true | _ => false) ss
                 of [name, ty, resArity, arity, comment] => SOME{
                        name = SS.string (stripWS name),
                        isPure = isPure,
                        ty = let
                          val ty = stripWS ty
                          fun cvt ty = SS.string(stripWS ty)
                          in
                            if (SS.isEmpty ty)
                              then []
                              else List.map cvt (SS.fields (fn #"*" => true | _ => false) ty)
                          end,
                        resArity = #1(valOf (Int.scan StringCvt.DEC SS.getc resArity)),
                        arity = if SS.compare(stripWS arity, SS.full "*") = EQUAL
                            then ~1 (* variable arity *)
                            else #1(valOf (Int.scan StringCvt.DEC SS.getc arity)),
                        comment = optField (stripWS comment)
                      }
                  | [_] => NONE (* assume a blank line *)
                  | _ => raise Fail(concat[
                        "bogus input: \"", String.toString(Substring.string ss),
                        "\"\n"
                      ])
                (* end case *))
          in
            case SS.getc ss
             of NONE => NONE
              | SOME(#"#", _) => NONE
              | SOME(#"!", rest) => doFields (false, rest)
              | _ => doFields (true, ss)
            (* end case *)
          end

  (* read a specification file, returning a list of operator specifications *)
    fun readFile fname = let
          val inS = TextIO.openIn fname
          fun lp lns = (case TextIO.inputLine inS
                 of NONE => List.rev lns
                  | SOME ln => (case doLine (SS.full ln)
                       of SOME ln => lp(ln::lns)
                        | NONE => lp lns
                      (* end case *))
                (* end case *))
          in
            lp [] before TextIO.closeIn inS
          end

    fun usage sts = (
          TextIO.output(TextIO.stdErr, "usage: gen-ops <spec-file> <template-file> <out-file>\n");
          sts)

  (* extract the path part of a qualified name (if it exists) *)
    fun pathOf ty = (case String.fields (fn #"." => true | _ => false) ty
           of [path, _] => SOME path
            | _ => NONE
          (* end case *))

    fun genFun (outS, name, ops, genOp : op_spec -> unit) = let
          fun lp (_, []) = ()
            | lp (prefix, rator::ops) = (
                TextIO.output(outS, concat["    ", prefix, " ", name, " "]);
                genOp rator;
                lp ("  |", ops))
          in
            lp ("fun", ops)
          end

    fun genType (outS, ops) = let
          fun lp (_, []) = ()
            | lp (prefix, {name, isPure, ty, resArity, arity, comment}::r) = (
                case ty
                 of [] => TextIO.output(outS, concat["      ", prefix, " ", name, "\n"])
                  | [ty] => TextIO.output(outS,
                      concat["      ", prefix, " ", name, " of ", ty, "\n"])
                  | ty::tys => (
                      TextIO.output(outS,
                        concat["      ", prefix, " ", name, " of ", ty]);
                      List.app (fn ty => TextIO.output(outS, " * "^ty)) tys;
                      TextIO.output(outS, "\n"))
                (* end case *);
                lp ("|", r))
          in
            TextIO.output (outS, "    datatype rator\n");
            lp ("=", ops)
          end

(* FIXME: we should use a default case for the most common arity value *)
    fun genResArity (outS, ops) = let
          fun genOp {name, isPure, ty=[], resArity, arity, comment} =
                TextIO.output(outS, concat[name, " = ", Int.toString resArity, "\n"])
            | genOp {name, resArity, ...} =
                TextIO.output(outS, concat["(", name, " _) = ", Int.toString resArity, "\n"])
          in
            genFun (outS, "resultArity", ops, genOp)
          end

    fun genArity (outS, ops) = let
          fun genOp {name, isPure, ty=[], resArity, arity, comment} =
                TextIO.output(outS, concat[name, " = ", Int.toString arity, "\n"])
            | genOp {name, arity, ...} =
                TextIO.output(outS, concat["(", name, " _) = ", Int.toString arity, "\n"])
          in
            genFun (outS, "arity", ops, genOp)
          end

    fun genPurity (outS, ops) = let
          fun lp (prefix, []) =
                TextIO.output(outS, concat["    ", prefix, " isPure _ = true\n"])
            | lp (prefix, {name, isPure=false, ty, resArity, arity, comment}::ops) = (
                TextIO.output(outS, concat["    ", prefix, " isPure "]);
                case ty
                 of [] => TextIO.output(outS, concat[name, " = false\n"])
                  | _ => TextIO.output(outS, concat["(", name, " _) = false\n"])
                (* end case *);
                lp ("  |", ops))
            | lp (prefix, {isPure=true, ...}::ops) = lp (prefix, ops)
          in
            lp ("fun", ops)
          end

    fun genSame (outS, ops) = let
          fun prl l = TextIO.output(outS, concat l)
          fun genOp {name, isPure, ty=[], resArity, arity, comment} =
                prl ["(", name, ", ", name, ") = true\n"]
            | genOp {name, ty=ty::tys, arity, ...} = let
                fun test argTy = (case pathOf argTy
                       of SOME path => path ^ ".same"
                        | NONE => "same" ^ argTy
                      (* end case *))
                val nArgs = List.length tys + 1
                val arg1::args1 = List.tabulate(nArgs, fn i => "a"^Int.toString i)
                val arg2::args2 = List.tabulate(nArgs, fn i => "b"^Int.toString i)
                fun app3 f (x::xs, y::ys, z::zs) = (f(x, y, z); app3 f (xs, ys, zs))
                  | app3 f ([], [], []) = ()
                in
                  prl ["(", name, "(", arg1];
                  List.app (fn x => prl [",", x]) args1;
                  prl ["), ", name, "(", arg2];
                  List.app (fn x => prl [",", x]) args2;
                  prl [")) = ", test ty, "(a0, b0)"];
                  app3
                    (fn (ty, a, b) => prl[" andalso ", test ty, "(", a, ", ", b, ")"])
                      (tys, args1, args2);
                  TextIO.output(outS,"\n")
                end
          in
            genFun (outS, "same", ops, genOp);
            TextIO.output (outS, "      | same _ = false\n")
          end

  (* the first 200 primes *)
    val primes = Vector.fromList [
              2,     3,     5,     7,    11,    13,    17,    19,    23,    29,
             31,    37,    41,    43,    47,    53,    59,    61,    67,    71,
             73,    79,    83,    89,    97,   101,   103,   107,   109,   113,
            127,   131,   137,   139,   149,   151,   157,   163,   167,   173,
            179,   181,   191,   193,   197,   199,   211,   223,   227,   229,
            233,   239,   241,   251,   257,   263,   269,   271,   277,   281,
            283,   293,   307,   311,   313,   317,   331,   337,   347,   349,
            353,   359,   367,   373,   379,   383,   389,   397,   401,   409,
            419,   421,   431,   433,   439,   443,   449,   457,   461,   463,
            467,   479,   487,   491,   499,   503,   509,   521,   523,   541,
            547,   557,   563,   569,   571,   577,   587,   593,   599,   601,
            607,   613,   617,   619,   631,   641,   643,   647,   653,   659,
            661,   673,   677,   683,   691,   701,   709,   719,   727,   733,
            739,   743,   751,   757,   761,   769,   773,   787,   797,   809,
            811,   821,   823,   827,   829,   839,   853,   857,   859,   863,
            877,   881,   883,   887,   907,   911,   919,   929,   937,   941,
            947,   953,   967,   971,   977,   983,   991,   997,  1009,  1013,
           1019,  1021,  1031,  1033,  1039,  1049,  1051,  1061,  1063,  1069,
           1087,  1091,  1093,  1097,  1103,  1109,  1117,  1123,  1129,  1151,
           1153,  1163,  1171,  1181,  1187,  1193,  1201,  1213,  1217,  1223
          ]

    fun genHash (outS, ops) = let
          fun prl l = TextIO.output(outS, concat l)
          fun genOp (i, {name, isPure, ty=[], resArity, arity, comment}) =
                prl [name, " = 0w", Int.toString(Vector.sub(primes, i)), "\n"]
            | genOp (i, {name, ty=ty::tys, ...}) = let
                fun hash argTy = (case pathOf argTy
                       of SOME path => path ^ ".hash"
                        | NONE => "hash" ^ argTy
                      (* end case *))
                val arg::args = List.tabulate(List.length tys + 1, fn i => "a"^Int.toString i)
                in
                  prl ["(", name, "(", arg];
                  List.app (fn x => prl [",", x]) args;
                  prl [
                      ")) = 0w", Int.toString(Vector.sub(primes, i)),
                      " + ", hash ty, " ", arg
                    ];
                  ListPair.appEq
                    (fn (ty, x) => prl [" + ", hash ty, " ", x])
                      (tys, args);
                  TextIO.output(outS, "\n")
                end
          fun lp (_, _, []) = ()
            | lp (i, prefix, rator::r) = (
                prl ["    ", prefix, " hash "];
                genOp (i, rator);
                lp (i+1, "  |", r))
          in
            lp (1, "fun", ops)
          end

    fun genToString (outS, ops) = let
          fun prl l = TextIO.output(outS, concat l)
          fun genOp {name, isPure, ty=[], resArity, arity, comment} =
                prl [name, " = \"", name, "\"\n"]
            | genOp {name, ty=ty::tys, ...} = let
                fun toS argTy = (case pathOf argTy
                       of SOME path => path ^ ".toString"
                        | NONE => argTy ^ "ToString"
                      (* end case *))
                val arg::args = List.tabulate(List.length tys + 1, fn i => "a"^Int.toString i)
                in
                  prl ["(", name, "(", arg];
                  List.app (fn x => prl [",", x]) args;
                  prl [")) = concat[\"", name, "<\", ", toS ty, " ", arg];
                  ListPair.app
                    (fn (ty, x) => prl[", \",\", ", toS ty, " ", x])
                      (tys, args);
                  TextIO.output(outS, ", \">\"]\n")
                end
          in
            genFun (outS, "toString", ops, genOp)
          end

    fun gen (outS, ops : op_spec list) = (
          genType (outS, ops);
          TextIO.output(outS, "\n");
          genResArity (outS, ops);
          TextIO.output(outS, "\n");
          genArity (outS, ops);
          TextIO.output(outS, "\n");
          genPurity (outS, ops);
          TextIO.output(outS, "\n");
          genSame (outS, ops);
          TextIO.output(outS, "\n");
          genHash (outS, ops);
          TextIO.output(outS, "\n");
          genToString (outS, ops))

    fun main (cmd, [specFile, templateFile, outFile]) = let
          val ops = readFile specFile
          val inS = TextIO.openIn templateFile
          val outS = TextIO.openOut outFile
          fun copy () = (case TextIO.inputLine inS
                 of SOME "@BODY@\n" => (gen (outS, ops); copy())
                  | SOME ln => (TextIO.output(outS, ln); copy())
                  | NONE => ()
                (* end case *))
          in
            copy();
            TextIO.closeIn inS;
            TextIO.closeOut outS;
            OS.Process.success
          end
      | main (cmd, ["-h"]) = usage OS.Process.success
      | main (cmd, _) = usage OS.Process.failure

  end
