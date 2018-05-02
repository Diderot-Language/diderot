(* options.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure Options : sig

  (* raised if parsing command-line args hits an error (e.g., missing option, syntax, ...).
   * The string is an error message.
   *)
    exception Usage of string

  (* parse the command-line args *)
    val parseCmdLine : string list -> {
            help : bool option,         (* "-h" and "--help" ==> SOME false; "-H" ==> SOME true. *)
            version : bool,             (* "--version" specified? *)
            about : bool,               (* "--about" specified? *)
            dumpBasis : bool,           (* "--dump-basis" specified? *)
            defs : CmdLineConstants.t,  (* constant & preprocessor command-line definitions *)
            target : TargetOptions.t,   (* collected infromation about the target *)
            file : string               (* source file *)
          }

  (* return a usage message.  The boolean controls whether all options should be
   * included (true ==> long; false ==> short).
   *)
    val usage : string * bool -> string

  end = struct

    structure G = GetOpt
    structure P = OS.Path
    structure Tgt = TargetOptions

    exception Usage of string

  (* option flags that are set by getOpt *)
    val helpFlg = ref(NONE : bool option)       (* SOME false -- short help; SOME true -- long help *)
    val aboutFlg = ref false
    val dumpBasisFlg = ref false
    val longHelp = ref false
    val versionFlg = ref false
    val runtimeLogFlg = ref false
    val debugFlg = ref false
    val doubleFlg = ref false
    val longIntFlg = ref false
    val scalarFlg = ref false
    val outputOpt : string option ref = ref NONE
    val execFlg = ref false
    val staticFlg = ref false
    val jsonFlg = ref false
    val snapshotFlg = ref false
    val prefix : string option ref = ref NONE
    val platform = ref Tgt.SEQUENTIAL
    val bspFlg = ref false
    val noSpacePartFlg = ref false

    fun mkOpt ctl = let
          val name = if Controls.get ctl
                then "disable-" ^ Controls.name ctl
                else Controls.name ctl
          in
            Controls.mkOptionFlag {ctl = ctl, short = "", long = SOME name}
          end

  (* create the target option descriptor. *)
    local
      val desc = if Paths.cudaEnabled then ["  cuda       -- generate CUDA code"] else []
      val desc = if Paths.clEnabled then "  opencl     -- generate OpenCL code" :: desc else desc
      val desc = if Paths.debuggerEnabled
            then "  debugger   -- generate code that can be run under the debugger" :: desc
            else desc
      val desc = "  parallel   -- generate parallel code" :: desc
      val desc = "  sequential -- generate sequential code (default)" :: desc
      val desc = "specify the target platform:" :: desc
      fun parseTargetOpt "c" = (platform := Tgt.SEQUENTIAL)
        | parseTargetOpt "sequential" = (platform := Tgt.SEQUENTIAL)
        | parseTargetOpt "parallel" = (platform := Tgt.PARALLEL)
        | parseTargetOpt "pthread" = (platform := Tgt.PARALLEL)
        | parseTargetOpt "cl" = if Paths.clEnabled
            then (platform := Tgt.OPENCL)
            else raise Usage "cl target not supported by this version"
        | parseTargetOpt "opencl" = if Paths.clEnabled
            then (platform := Tgt.OPENCL)
            else raise Usage "cl target not supported by this version"
        | parseTargetOpt "cuda" = if Paths.cudaEnabled
            then (platform := Tgt.CUDA)
            else raise Usage "cuda target not supported by this version"
        | parseTargetOpt "debugger" = if Paths.debuggerEnabled
            then (platform := Tgt.DEBUGGER)
            else raise Usage "debugger target not supported by this version"
        | parseTargetOpt opt = raise Usage(concat["unrecognized target \"", opt, "\""])
    in
    val targetOptDesc = {
            short = "",
            long = ["target"],
            desc = G.ReqArg(parseTargetOpt, "target"),
            help = String.concatWith "\n" desc
          }
    end

    fun setFlag (flg, value) = G.NoArg(fn () => (flg := value))

  (* the short list of options, which does not include the compiler controls *)
    val optionList = [
            { short = "h", long = ["help"],
              desc = setFlag (helpFlg, SOME false),
              help = "print command-line options"
            },
            { short = "H", long = [],
              desc = setFlag (helpFlg, SOME true),
              help = "print all command-line options (including compiler controls)"
            },
            { short = "", long = ["version"],
              desc = setFlag (versionFlg, true),
              help = "show the compiler version"
            },
            { short = "", long = ["about"],
              desc = setFlag (aboutFlg, true),
              help = "information about the Diderot language and compiler"
            },
            { short = "", long = ["exec"],
              desc = setFlag (execFlg, true),
              help = "generate a standalone executable"
            },
            { short = "", long = ["static"],
              desc = setFlag (staticFlg, true),
              help = "generate a statically linked library"
            },
            { short = "", long = ["json"],
              desc = setFlag (jsonFlg, true),
              help = "generate a JSON description of library API (incompatible with --exec)"
            },
            { short = "o", long = ["output"],
              desc = G.ReqArg(fn s => outputOpt := SOME s, "file"),
              help = "specify the executable file name"
            },
            { short = "", long = ["namespace"],
              desc = G.ReqArg(fn s => prefix := SOME s, "prefix"),
              help = "specify namespace prefix for generated code"
            },
            { short = "", long = ["snapshot"],
              desc = setFlag (snapshotFlg, true),
              help = "generate code to get a snapshot of strand states"
            },
            { short = "g", long = ["debug"],
              desc = setFlag (debugFlg, true),
              help = "enable debugging information in executable"
            },
            { short = "", long = ["double"],
              desc = setFlag (doubleFlg, true),
              help = "use double-precision floats for reals"
            },
            { short = "", long = ["long-int"],
              desc = setFlag (longIntFlg, true),
              help = "use 64-bits for ints"
            },
(* QUESTION: perhaps --scalar should not be part of the short option list? *)
            { short = "", long = ["scalar"],
              desc = setFlag (scalarFlg, true),
              help = "do not generate vectorized code"
            },
(* QUESTION: perhaps --log should not be part of the short option list? *)
            Controls.mkOptionFlag {ctl = Ctl.enableLog, short = "", long = SOME "log"},
(* QUESTION: perhaps --stats should not be part of the short option list? *)
            Controls.mkOptionFlag {ctl = Ctl.collectStats, short = "", long = SOME "stats"},
(* QUESTION: perhaps --verbose should not be part of the short option list? *)
            Controls.mkOptionFlag {ctl = Ctl.verbose, short = "", long = SOME "verbose"},
            targetOptDesc
          ]

  (* create the list of options that control compiler internals *)
    val ctlOptions = let
          val optimizeFlags = List.map mkOpt Ctl.optimizeControls
          val dumpFlags = List.map mkOpt Ctl.dumpControls @ [
                  { short = "", long = ["dump-basis"],
                    desc = setFlag (dumpBasisFlg, true),
                    help = "dump the Diderot basis to 'basis.tex'"
                  },
                  { short = "", long = ["dump-cfg"],
                    desc = setFlag (Ctl.dumpCFG, true),
                    help = "dump out all CFG representations to the log file"
                  },
                  { short = "", long = ["dump-all"],
                    desc = setFlag (Ctl.dumpAll, true),
                    help = "dump out all intermediate representations to the log file"
                  },
                  { short = "", long = ["show-cfg-labels"],
                    desc = setFlag (Ctl.compactCFG, false),
                    help = "show the labels of all CFG nodes"
                  },
                  { short = "", long = ["log-ticks"],
                    desc = setFlag (Stats.logTicks, false),
                    help = "log each optimization tick as it is recorded"
                  }
                ]
          val checkFlags = List.map mkOpt Ctl.checkControls @ [
                { short = "", long = ["check-all"],
                  desc = setFlag (Ctl.checkAll, true),
                  help = "always check intermediate representations"
                }]
          val otherOpts = [
                  {
                    short = "", long = ["force-bsp"],
                    desc = setFlag (bspFlg, true),
                    help = "execute strands in BSP mode"
                  },
                  {
                    short = "", long = ["no-space-partition"],
                    desc = setFlag (noSpacePartFlg, true),
                    help = "implement spatial queries without acceleration"
                  }
                ]
          val otherOpts = if Paths.runtimeLogging
                then { short = "", long = ["runtime-logging"],
                    desc = setFlag (runtimeLogFlg, true),
                    help = "enable runtime event logging (parallel target only)"
                  } :: otherOpts
                else otherOpts
          in
            otherOpts @ optimizeFlags @ dumpFlags @ checkFlags
          end

    fun mkTargetDesc (srcFile) : Tgt.t = let
          val (outDir, outBase) = (case !outputOpt
                 of NONE => let
                      val {dir, file} = P.splitDirFile srcFile
                      in
                        case P.splitBaseExt file
                         of {base, ext=SOME "diderot"} => (dir, base)
                          | {base, ext=SOME "ddro"} => (dir, base)
                          | _ => (dir, file)
                        (* end case *)
                      end
                  | SOME outFile => let
                      val {dir, file} = P.splitDirFile outFile
                      in
                        if !execFlg
                          then (dir, file)
                          else (case P.splitBaseExt file
                             of {base, ext=SOME "o"} => (dir, base)
                              | {base, ext=SOME "obj"} => (dir, base)
                              | _ => (dir, file)
                            (* end case *))
                      end
                (* end case *))
        (* get the namespace and check that it is legal *)
          val namespace = (case !prefix
                 of NONE => "Diderot"
                  | SOME "diderot" => raise Usage "namespace \"diderot\" is reserved"
                  | SOME ns => (case String.explode ns
                       of [] => raise Usage "invalid empty namespace specifier"
                        | c::cs => let
                            fun isAlpha #"_" = true | isAlpha c = Char.isAlpha c
                            fun isAlphaNum #"_" = true | isAlphaNum c = Char.isAlphaNum c
                            in
                              if isAlpha c andalso List.all isAlphaNum cs
                                then ns
                                else raise Usage "invalid namespace specifier"
                            end
                      (* end case *))
                (* end case *))
        (* if necessary, set the Diderot integer size to long *)
          val () = if !longIntFlg then IntLit.setDiderotLongInt() else ()
        (* if the target is the Diderot Debugger, then enable the JSON API too *)
          val () = if !platform = TargetOptions.DEBUGGER then jsonFlg := true else ()
          in {
            srcFile = srcFile,
            outDir = outDir,
            outBase = outBase,
            exec = !execFlg,
            staticLib = !staticFlg,
            jsonAPI = !jsonFlg,
            snapshot = !snapshotFlg,
            platform = !platform,
            namespace = namespace,
            double = !doubleFlg,
            longint = !longIntFlg,
            scalar = !scalarFlg,
            runtimeLog = !runtimeLogFlg,
            debug = !debugFlg,
            bsp = !bspFlg,
            kdtree = not(!noSpacePartFlg)
          } end

    fun parse [] = {
            help = SOME false,
            version = false,
            about = false,
            dumpBasis = false,
            defs = CmdLineConstants.empty,
            target = mkTargetDesc "",
            file = ""
          }
      | parse args = (case CmdLineConstants.initFromArgs args
           of SOME(defs, rest) => let
                val (opts, files) = G.getOpt {
                        argOrder = G.RequireOrder,
                        options = optionList @ ctlOptions,
                        errFn = fn s => raise Usage s
                      } rest
              (* figure out filename pieces *)
                val srcFile =
                      if isSome(!helpFlg) orelse !versionFlg orelse !aboutFlg
                      orelse !dumpBasisFlg
                        then ""
                        else (case files
                           of [] => raise Usage "missing file argument"
                            | [f] => f
                            | _ => raise Usage "too many files"
                          (* end case *))
                in {
                  help = !helpFlg,
                  version = !versionFlg,
                  about = !aboutFlg,
                  dumpBasis = !dumpBasisFlg,
                  defs = defs,
                  target = mkTargetDesc srcFile,
                  file = srcFile
                } end
            | NONE => raise Usage "badly-formed constant definition"
          (* end case *))

  (* check for inconsistent command-line options.  The conflicts are:
   *
   *    --runtime-logging and non-parallel target
   *                                    ## runtime logging is only supported for the
   *                                       the parallel target
   *    --exec and --static             ## the --static option implies a library
   *    --exec and --jason              ## the --json option implies a library
   *    --exec and --target=debugger    ## the debugger requires a library
   *)
    fun checkOptions (arg as {help=SOME _, ...}) = arg
      | checkOptions (arg as {help, version, about, dumpBasis, defs, target : TargetOptions.t, file}) = (
          if (#runtimeLog target andalso (#platform target <> TargetOptions.PARALLEL))
          orelse (#exec target andalso (#jsonAPI target orelse #staticLib target))
          orelse ((#platform target = TargetOptions.DEBUGGER) andalso #exec target)
            then raise Usage "inconsistent options"
            else arg)

    fun parseCmdLine args = checkOptions (parse args)

    val fakeConstOpt = {
            desc = G.NoArg(fn _ => ()),
            help = "specify value for a constant variable",
            long = ["C<var>=<value>"],
            short = ""
          }
    val fakeDefOpt = {
            desc = G.NoArg(fn _ => ()),
            help = "specify preprocessor symbol (for runtime debugging)",
            long = ["D<var>=<value>"],
            short = ""
          }

    fun usage (cmd, long) = let
          val hdr = concat[
                  "usage: ", cmd, " [options] file.diderot\n",
                  "  Version: ", Version.message, "\n",
                  "  Options:"
                ]
        (* add item for "-C" and "-D" options*)
          val options = if long
                then optionList @ [fakeConstOpt, fakeDefOpt]
                else optionList @ [fakeConstOpt]
          in
            if long
              then G.usageInfo {header = hdr, options = options @ ctlOptions}
              else G.usageInfo {header = hdr, options = options}
          end

  end
