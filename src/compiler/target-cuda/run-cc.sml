(* run-cc.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Support for running the C++ compiler to compile and link the generated code.
 *)

structure RunCC : sig

  (* compile ("base", cflags) -- compiles the file "base.cxx" to produce "base.o". *)
    val compile : string * string list -> unit

  (* linkExec ("base", opts) -- links base.o to create an executable using the
   * given options (libraries, etc.)
   *)
    val linkExec : string * string list -> unit

  (* linkStaticLib (isStatic, "base", opts) -- links base.o to create a library
   * using the given options (libraries, etc.).  If isStatic is true, then the
   * library is statically linked.  Otherwise it is dynamically linked.
   *)
    val linkLib : bool * string * string list -> unit

  end = struct

    fun system cmd = (
          Log.msg [cmd, "\n"];
          if OS.Process.isSuccess(OS.Process.system cmd)
            then ()
            else raise Fail "error compiling/linking")

    fun compile (baseName, cxxflags) = let
          val cuFile = OS.Path.joinBaseExt{base=baseName, ext=SOME "cu"}
          val cmdArgs = [cuFile]
          val cmdArgs = if OS.Path.dir baseName <> ""
              (* the C++ compiler will, by default, put the ".o" file in the current working
               * directory, so we need to specify the path for the .o file.
               *)
                then "-o" :: OS.Path.joinBaseExt{base=baseName, ext=SOME Paths.objSuffix} :: cmdArgs
                else cmdArgs
          val cmdArgs = Paths.nvcc :: "-c" :: cxxflags @ cmdArgs
          val cmd = String.concatWith " " cmdArgs
          val _ = print cmd
          in
            PhaseTimer.withTimer Timers.timeCC system cmd
          end

    fun linkExec (baseName, ldOpts) = let
          val objFile = OS.Path.joinBaseExt{base=baseName, ext=SOME Paths.objSuffix}
          val exeFile = OS.Path.joinBaseExt{base=baseName, ext=Paths.execSuffix}
          val cmd = String.concatWith " " ([Paths.nvcc, "-o", exeFile, objFile] @ ldOpts)
          val _ = print (concat[cmd, "\n"]) 
          in
            PhaseTimer.withTimer Timers.timeCC system cmd
          end

    fun linkLib (isStatic, baseName, ldOpts) = let
          val objFile = OS.Path.joinBaseExt{base=baseName, ext=SOME Paths.objSuffix}
          val linkCmd = Paths.linkLibrary isStatic
          in
	    if isStatic
	      then let
		val tmpFile = let
		    (* on Linux systems, the rename fails if the src and dst are on
		     * different devices, so we create the temp file in the same
		     * directory as the final target.
		     *)
		      val {file, ...} = OS.Path.splitDirFile(OS.FileSys.tmpName())
		      val {dir, ...} = OS.Path.splitDirFile baseName
		      in
			OS.Path.joinDirFile{
			    dir = dir,
			    file = OS.Path.joinBaseExt{
				base = file,
				ext = SOME Paths.objSuffix
			      }
			  }
		      end
		val cmd = String.concatWith " " ([linkCmd, "-r", "-o", tmpFile, objFile] @ ldOpts)
		fun link () = (
		      system cmd;
		      Log.msg ["rename ", tmpFile, " to ", objFile, "\n"];
		      OS.FileSys.rename{old=tmpFile, new=objFile}
			handle ex => (OS.FileSys.remove tmpFile; raise ex))
		in
		  PhaseTimer.withTimer Timers.timeCC link ()
		end
	      else let
		val libFile = OS.Path.joinBaseExt{base=baseName, ext=SOME Paths.dslSuffix}
		val cmd = String.concatWith " " ([linkCmd, "-o", libFile, objFile] @ ldOpts)
		fun link () = system cmd
		in
		  PhaseTimer.withTimer Timers.timeCC link ()
		end
          end

  end
