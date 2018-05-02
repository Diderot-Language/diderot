(* cmd-line-constants.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Support for processing constant-variable defintions from the command line.  These
 * definitions have the form --C<id>=<value>, where <id> is the constant-variable
 * name and <value> is its value.
 *
 * We also support a mechanism for specifying C-preprocessor definitions that
 * get included in the generated source.
 *)

structure CmdLineConstants : sig

  (* table of constant and C-preprocessor command-line definitions *)
    type t

    val empty : t

  (* initialize the table from the command-line arguments; returns NONE if there was
   * an error processing an argument.  Otherwise it returns the table and the
   * residual arguments.
   *)
    val initFromArgs : string list -> (t * string list) option

  (* `getConst tbl name` gets the value of the named input if it was defined on the
   * command line.
   *)
    val getConst : t -> Atom.atom -> string option

  (* `defines tbl` returns a list of the C-preprocessor definitions given from the
   * command line.
   *)
    val defines : t -> (string * string) list

  end = struct

    structure Tbl = AtomTable

    val inputs : string AtomTable.hash_table = AtomTable.mkTable(32, Fail "input table")

    fun notEq #"=" = false | notEq _ = true

    val isConstArg = Substring.isPrefix "--C"
    val isDefArg = Substring.isPrefix "--D"

    datatype t = CTbl of {
        consts : string Tbl.hash_table,
        defs : (string * string) list
      }

    val empty = CTbl{consts = Tbl.mkTable (1, Fail ""), defs = []}

    exception Error

    fun initFromArgs args = let
          val tbl = AtomTable.mkTable(16, Fail "const table")
          fun splitArg arg = let
                val (name, rest) = Substring.splitl notEq (Substring.triml 3 arg)
                in
                  if (Substring.size rest > 0)
                    then (name, Substring.triml 1 rest)
                    else (name, rest)
                end
          fun doArg (origArg, (defs, otherArgs))  = let
                val arg = Substring.full origArg
                in
                  if isConstArg arg
                    then let
                      val (name, rest) = splitArg arg
                      in
                        if (Substring.size name > 0) andalso (Substring.size rest > 0)
                          then (
                            AtomTable.insert tbl
                              (Atom.atom' name, Substring.string rest);
                            (defs, otherArgs))
                          else raise Error
                      end
                  else if isDefArg arg
                    then let
                      val (name, rest) = splitArg arg
                      fun continue value = ((Substring.string name, value)::defs, otherArgs)
                      in
                        if (Substring.size name = 0)
                          then raise Error
                        else if Substring.size rest = 0
                          then continue "1"
                          else continue (Substring.string rest)
                      end
                    else (defs, origArg::otherArgs)
                end
          val (defs, otherArgs) = List.foldl doArg ([], []) args
          val ctbl = CTbl{consts = tbl, defs = List.rev defs}
          in
            SOME(ctbl, List.rev otherArgs)
          end
            handle Error => NONE

    fun getConst (CTbl{consts, ...}) name = AtomTable.find consts name

    fun defines (CTbl{defs, ...}) = defs

  end
