(* code-output.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure CodeOutput : sig

    type t

  (* `openOut {ext, ppDecl} basename` opens an output stream for CLang code, where
   * `ext` is the file extension (e.g., "cxx" or "h"), and `ppDecl` is the function for
   * pretty printing a CLang decl in the appropriate syntax.
   *)
    val openOut : {
            ext : string,
            ppDecl : TextIOPP.stream * CLang.decl -> unit
          } -> string -> t

    val filename : t -> string

  (* pretty print a CLang decl to the output stream *)
    val decl : t -> CLang.decl -> unit

  (* `fragment substs (outS, frag)` prints the fragment `frag` to the output stream after
   * applying the given substitutions.
   *)
    val fragment : (string * string) list -> t -> string -> unit

  (* close the output stream *)
    val closeOut : t -> unit

  end = struct

    datatype t = OS of {
        file : string,
        outS : TextIO.outstream,
        ppStrm : TextIOPP.stream,
        ppDecl : TextIOPP.stream * CLang.decl -> unit
      }

    fun newPP outs = TextIOPP.openOut {dst = outs, wid = 120}

    fun openOut {ext, ppDecl} = let
          fun openOut' baseName = let
                val fileName = OS.Path.joinBaseExt{base=baseName, ext=SOME ext}
                val outS = TextIO.openOut fileName
                in
                  OS{
                      file = fileName,
                      outS = outS,
                      ppStrm = newPP outS,
                      ppDecl = ppDecl
                    }
                end
          in
            openOut'
          end

    fun filename (OS{file, ...}) = file

    fun decl (OS{ppStrm, ppDecl, ...}) dcl = ppDecl (ppStrm, dcl)

    fun fragment substs (OS{ppStrm, ppDecl, ...}) frag =
          ppDecl (ppStrm, CLang.verbatimDcl [frag] substs)

    fun closeOut (OS{outS, ppStrm, ...}) = (
          TextIOPP.closeStream ppStrm;
          TextIO.closeOut outS)

  end
