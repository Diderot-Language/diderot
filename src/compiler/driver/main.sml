(* main.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

  (* messages in verbose mode *)
(* QUESTION: perhaps we should include timing info? *)
    fun verbosePrint msg = if Controls.get Ctl.verbose
          then TextIO.output(TextIO.stdErr, concat msg)
          else ()

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm =
          if Error.anyErrors errStrm
            then raise Error.ERROR
            else ()

  (* conditional printing of intermediate forms *)
    fun dump ctl output phase prog = if Controls.get ctl
          then output (Log.logFile(), "After "^phase, prog)
          else ()

  (* compiler front end (parsing, typechecking, and simplification *)
    fun frontEnd' (errStrm, defs, filename) = let
          val _ = if OS.FileSys.access(filename, [OS.FileSys.A_READ])
                then ()
                else (
                  err(concat["source file \"", filename, "\" does not exist or is not readable\n"]);
                  raise Error.ERROR)
        (***** PARSING *****)
          val _ = verbosePrint["parsing ... "]
          val (version, parseTree) = PhaseTimer.withTimer Timers.timeParser (fn () => let
                val inS = TextIO.openIn filename
                val pt = Parser.parseFile (errStrm, inS)
                in
                  TextIO.closeIn inS;
                  checkForErrors errStrm;
                  valOf pt
                end) ()
          val _ = verbosePrint["done\n"]
          val _ = dump Ctl.dumpPT (ParseTreePP.output errStrm) "parsing" parseTree
        (***** TYPECHECKING *****)
          val _ = verbosePrint["type checking ... "]
          val _ = PhaseTimer.start Timers.timeTypechecker
          val (ast, gEnv) = (Typechecker.check defs errStrm (version, parseTree))
          val _ = PhaseTimer.stop Timers.timeTypechecker
          val _ = verbosePrint["done\n"];
          val _ = checkForErrors errStrm
(* FIXME: check AST for consistency *)
          val _ = dump Ctl.dumpAST ASTPP.output "typechecking" ast
        (***** SIMPLIFY *****)
          val _ = verbosePrint["simplifying AST ... "]
          val simple = SimpleOpt.checkAfter ("simplify", Simplify.transform (errStrm, ast, gEnv))
          val _ = checkForErrors errStrm
          val simple = SimpleOpt.transform simple
          val _ = verbosePrint["done\n"]
          in
            simple
          end

  (* a wrapper around the front-end that handles the Error.ERROR exception and reports
   * the error messages.
   *)
    fun frontEnd (defs, filename) = let
          val errStrm = Error.mkErrStream filename
          fun finish () = (
                if Error.anyErrors errStrm orelse Error.anyWarnings errStrm
                  then Error.report (TextIO.stdErr, errStrm)
                  else ())
          in
            (frontEnd' (errStrm, defs, filename) handle exn => (finish (); raise exn))
            before finish()
          end

    fun doFile (target : TargetOptions.t, defs, filename) = let
          val {info, generate} = Targets.get target
          val baseName = (case OS.Path.splitBaseExt filename
                 of {base, ext=SOME "diderot"} => base
                  | {base, ext=SOME "ddro"} => base
                  | _ => (errnl "expected diderot file"; raise Error.ERROR)
                (* end case *))
          val simple = PhaseTimer.withTimer Timers.timeFront frontEnd (defs, filename)
        (***** TRANSLATION TO HIGH IR*****)
          val _ = verbosePrint["translating to HighIR ... "]
          val high = PhaseTimer.withTimer Timers.timeTranslate Translate.translate simple
          val _ = verbosePrint["done\n"]
          val _ = verbosePrint["checking HighIR ... "]
          val high = HighOptimizer.checkAfter ("simple-to-high translation", high)
          val _ = verbosePrint["done\n"]
        (***** HIGH-IR OPTIMIZATION *****)
          val _ = verbosePrint["optimizing HighIR ... "]
          val high = PhaseTimer.withTimer Timers.timeHigh HighOptimizer.optimize high
          val _ = verbosePrint["done\n"]
        (***** TRANSLATION TO MID IR *****)
          val _ = verbosePrint["translating to MidIR ... "]
          val mid = PhaseTimer.withTimer Timers.timeHighToMid HighToMid.translate high
          val _ = verbosePrint["done\n"]
          val _ = verbosePrint["checking MidIR ... "]
          val mid = MidOptimizer.checkAfter ("high-to-mid translation", mid)
          val _ = verbosePrint["done\n"]
        (***** MID-IR OPTIMIZATION *****)
          val _ = verbosePrint["optimizing MidIR ... "]
          val mid = PhaseTimer.withTimer Timers.timeMid MidOptimizer.optimize mid
          val _ = verbosePrint["done\n"];
        (***** TRANSLATION TO LOW IR *****)
          val _ = verbosePrint["translating to LowIR ... "]
          val low = PhaseTimer.withTimer Timers.timeMidToLow MidToLow.translate mid
          val _ = verbosePrint["done\n"]
          val _ = verbosePrint["checking LowIR ... "]
          val low = LowOptimizer.checkAfter ("mid-to-low translation", low)
          val _ = verbosePrint["done\n"]
        (***** LOW-IR OPTIMIZATION *****)
          val _ = verbosePrint["optimizing LowIR ... "]
          val low = PhaseTimer.withTimer Timers.timeLow LowOptimizer.optimize low
          val _ = verbosePrint["done\n"]
        (***** TRANSLATION TO TREE IR *****)
          val _ = verbosePrint["translating to TreeIR ... "]
          val tree = PhaseTimer.withTimer Timers.timeLowToTree LowToTree.translate (low, info)
          val _ = verbosePrint["done\n"]
          val _ = verbosePrint["checking TreeIR ... "]
          val tree = TreeOptimizer.checkAfter ("low-to-tree translation", tree)
          val _ = verbosePrint["done\n"]
        (***** TREE-IR OPTIMIZATION *****)
          val _ = verbosePrint["optimizing TreeIR ... "]
          val tree = PhaseTimer.withTimer Timers.timeTree TreeOptimizer.optimize tree
          val _ = verbosePrint["done\n"]
          in
          (***** CODE GENERATION *****)
            verbosePrint["generating code ... "];
            PhaseTimer.withTimer Timers.timeCodegen generate (defs, tree);
            verbosePrint["done\n"]
          end

    fun usage (cmd, long, about) = (
          TextIO.output(TextIO.stdErr, Options.usage (cmd, long));
          if long orelse about
            then (
              TextIO.output(TextIO.stdErr, "==========\n");
              TextIO.output(TextIO.stdErr, About.message);
              TextIO.output(TextIO.stdErr, "==========\n"))
            else ())

    fun handleExn Error.ERROR = OS.Process.failure
      | handleExn exn = (
          err (concat [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ]);
          List.app (fn s => err (concat ["  raised at ", s, "\n"])) (SMLofNJ.exnHistory exn);
          OS.Process.failure)

    fun main (name: string, args: string list) = let
          val _ = PhaseTimer.start Timers.timeCompiler
          val {help, version, about, dumpBasis, target, defs, file} =
                (Options.parseCmdLine args)
                  handle Options.Usage msg => (
                    err(concat["Error: ", msg, "\n"]);
                    usage (name, false, false);
                    OS.Process.exit OS.Process.failure)
          in
            Ctl.resolve();
            case (help, version, about, dumpBasis)
             of (SOME long, _, _, _) => (usage (name, long, about); OS.Process.success)
              | (NONE, _, _, true) => (
(* FIXME: how should we specify the basis version on the command line? *)
                  DumpBasis.dump("basis.tex", [2]);
                  OS.Process.success)
              | (NONE, true, false, _) => (print(Version.message ^ "\n"); OS.Process.success)
              | (NONE, false, true, _) => (print(About.message ^ "\n"); OS.Process.success)
              | (NONE, true, true, _) => (
                  print(concat["Version: ", Version.message, "\n"]);
                  print "==========\n";
                  print About.message;
                  print "==========\n";
                  OS.Process.success)
              | _ => (let
                  val {base, ...} = OS.Path.splitBaseExt file
                  val logging = Controls.get Ctl.enableLog
                  in
                    if logging
                      then Log.init(base ^ ".log")
                      else ();
                    doFile (target, defs, file);
                    PhaseTimer.stop Timers.timeCompiler;
                    if Controls.get Ctl.collectStats
                      then (
                        if (not logging)
                          then Log.init(base ^ ".stats")
                          else ();
                        Stats.report ())
                      else ();
                    Log.reportTiming Timers.timeCompiler;
                    OS.Process.success
                  end handle exn => handleExn exn)
            (* end case *)
          end

  end
