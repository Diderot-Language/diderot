(* paths.sml
 *
 * Various directory paths required by the compiler.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * src/compiler/common/paths.sml.  Generated from paths_sml.in by configure.
 *)

structure Paths : sig

  (* functions to get the paths to Diderot headers and libraries *)
    val diderotInclude : unit -> string
    val diderotLib : unit -> string

    val teemBin : unit -> string
    val teemInclude : unit -> string
    val teemLinkFlags : unit -> string list

  (* SIMD support *)
    val hasSSE : bool
    val hasAVX : bool
    val hasAVX512 : bool

  (* command to run the C++ compiler *)
    val ccxx : string

  (* command-line flags for the C++ compiler *)
    val cxxFlags : {
            base : string,      (* for all targets *)
            simd : string,      (* for CPU targets *)
            parallel : string,  (* for parallel target *)
            cl : string,        (* for OpenCL target *)
            cuda : string,      (* for CUDA target *)
            debug : string,     (* when '--debug' flag is specified *)
            ndebug : string     (* when '--debug' flag is _not_ specified *)
          }

  (* extra libraries to include in the linking command *)
    val extraLibs : {
            base : string,      (* for all targets *)
            parallel : string,  (* for parallel target *)
            cl : string,        (* for OpenCL target *)
            cuda : string       (* for CUDA target *)
          }

  (* command to link libraries; the argument should be true if the --static flag was
   * given on the diderotc command line.
   *)
    val linkLibrary : bool -> string

  (* file suffix for executables *)
    val execSuffix : string option

  (* file suffic for object files *)
    val objSuffix : string

  (* file suffix for dynamic shared libraries *)
    val dslSuffix : string

  (* true if the configuration supports the OpenCL target *)
    val clEnabled : bool

  (* true if the configuration supports the CUDA target *)
    val cudaEnabled : bool

  (* true if the configuration supports runtime event logging *)
    val runtimeLogging : bool

  (* true if the configuration supprts the Diderot Debugger *)
    val debuggerEnabled : bool

  end = struct

    local
      val // = OS.Path.concat
      infixr 4 //
    in

  (* installation directories for Diderot stuff *)
    val installBin = "/usr/local" // "bin"
    val installInclude = "/usr/local" // "include"
    val installLib = "/usr/local" // "lib"

  (* directories for Diderot stuff *)
    local
    (* dynamically determine the installation location based on the path to diderotc.  We assume
     * that the bin, lib, and include directories are siblings in the directory tree.
     *)
      val installLoc : string option ref = ref NONE
      fun installLocation () = (case !installLoc
             of SOME loc => loc
              | NONE => let
                  val cmdPath = OS.Path.dir(CommandLine.name())
                  val binDir = if OS.Path.isAbsolute cmdPath
                        then cmdPath
                        else OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(), cmdPath))
                  val diderotLoc = OS.Path.getParent binDir
                  in
                    installLoc := SOME diderotLoc;
                    diderotLoc
                  end
            (* end case *))
    in
    fun diderotLib () = installLocation() // "lib"
    fun diderotInclude () = installLocation() // "include"
    end

  (* look for an executable using the user's PATH prepended with a list of
   * additional search directories.
   *)
    fun findExe (searchDirs, name) = let
          val dirs = searchDirs @ String.fields
                (fn #":" => true | _ => false)
                  (case (OS.Process.getEnv "PATH") of (SOME p) => p | _ => "")
          in
            case PathUtil.findExe dirs name
             of SOME p => p
              | NONE => raise Fail(concat[
                    "unable to find ", name, " executable in \"",
                    String.concatWith ":" dirs, "\""
                  ])
          end (* getPath *)

  (* directories for Teem stuff *)
    local
      val teemDir : string option ref = ref NONE
    (* return the path to the teem stuff *)
      fun resolveTeemDir subDir = (case !teemDir
             of SOME dir => dir // subDir
              | NONE => let
                (* extra places to look for unu *)
                  val extraPaths = let
                        val pl = ["/usr/local" // "bin"]
                        in
                          case OS.Process.getEnv "DIDEROT_TEEM_DIR"
                           of SOME p => (p // "bin") :: pl
                            | NONE => pl
                          (* end case *)
                        end
                  val unuPath = findExe (extraPaths, "unu")
                  val dir = OS.Path.getParent(OS.Path.dir unuPath)
                  in
                    teemDir := SOME dir;
                    dir // subDir
                  end
            (* end case *))
    in
    fun teemBin () = resolveTeemDir "bin"
    fun teemInclude () = resolveTeemDir "include"
    fun teemLinkFlags () = let
          val teemLib = resolveTeemDir "lib"
          in
            if false
              then ["-Wl,-rpath=" ^ teemLib, "-L" ^ teemLib, "-lteem"]
              else ["-L" ^ teemLib, "-lteem"]
          end
    end (* local *)

  (* SIMD support *)
    local
      val simdFlags = String.tokens Char.isSpace " -mmmx -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2 -maes -mavx -mfma -mavx2"
      fun hasFlag flg = List.exists (fn f => flg = f) simdFlags
      fun findFirstFlag [] = NONE
        | findFirstFlag (flg::flgs) = if hasFlag flg
            then SOME flg
            else findFirstFlag flgs
      val bestSSEFlag = findFirstFlag ["-msse4.2","-msse4.1","-msse3","-msse2","-msse"]
      val bestAVXFlag = findFirstFlag ["-mavx2","-mavx"]
    (* the AVX512 flags are broken down by feature (e.g., "-mavx512f" for foundation instructions,
     * "-mavx512dq" for double and quad-word instructions, etc.  We just grab them all.
     *)
      val avx512Flags = List.filter (String.isPrefix "-mavx512") simdFlags
    in
    val hasSSE = Option.isSome bestSSEFlag
    val hasAVX = Option.isSome bestAVXFlag
    val hasAVX512 = not (List.null avx512Flags)
    val simdFlags =
          avx512Flags @ Option.fold (op ::) (Option.fold (op ::) [] bestSSEFlag) bestAVXFlag
    end (* local *)

  (* add a, possibly empty, option to a list of options *)
    fun addOpt ("", opts) = opts
      | addOpt (opt, opts) = opt :: opts

  (* tools etc. for building executables *)
    val ccxx = let
          val (cmd::args) = String.tokens Char.isSpace "clang++ -std=gnu++11"
          val cmd = findExe ([], cmd)
          val args = addOpt ("-m64", args)
          val args = addOpt ("", args)
          in
            String.concatWith " " (cmd::args)
          end

    val cxxFlags = {
            base = "-Wreturn-type -Wuninitialized",
            simd = String.concatWith " " simdFlags,
            parallel = "-D_THREAD_SAFE ",
            cl = "",
            cuda = "@CPPFLAGS_CUDA@",
            debug = "-g -O0",
            ndebug = "-march=native -O3 -DNDEBUG"
          }

    val extraLibs = {
            base = "",
            parallel = "",
            cl = "",
            cuda = "@LIBS_CUDA@"
          }

    fun linkLibrary true = let (* static library *)
          val (cmd::args) = String.tokens Char.isSpace "/usr/bin/ld"
          val cmd = findExe ([], cmd)
          in
            String.concatWith " " (cmd :: args)
          end
      | linkLibrary false = let (* dynamic library *)
          val (cmd::args) = String.tokens Char.isSpace "clang++ -std=gnu++11 -dynamiclib"
          val cmd = findExe ([], cmd)
          in
            String.concatWith " " (cmd :: args)
          end

  (* file suffix for executables *)
    val execSuffix = if ("" = "") then NONE else SOME ""
  (* file suffic for object files *)
    val objSuffix = "o"
  (* file suffix for dynamic shared libraries *)
    val dslSuffix = "dylib"

  (* OpenCL configuration *)
    val clEnabled = false
    val (clVersion, clStd) = if clEnabled
          then let
          (* assume a numbering scheme equivalent to __OPENCL_VERSION__ *)
            val major = 0 div 100
            val minor = (0 div 10) mod 10
            val patch = 0 mod 10
            val vers = if patch > 0 then [major, minor, patch] else [major, minor]
            in
              (vers, String.concat["CL", Int.toString major, ".", Int.toString minor])
            end
          else ([], "")
    val clVersionString = "0"

  (* CUDA configuration *)
    val cudaEnabled = false
    end (* local *)

  (* true if the configuration supports runtime event logging *)
    val runtimeLogging = false

  (* true if the configuration supprts the Diderot Debugger *)
    val debuggerEnabled = false

  end
