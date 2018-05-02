(* typechecker.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure Typechecker : sig

  (* type check a Diderot program *)
    val check : CmdLineConstants.t
          -> Error.err_stream
          -> (ParseTree.version * ParseTree.program)
          -> AST.program * GlobalEnv.t

  end = struct

    structure PT = ParseTree
    structure E = Env

    fun check defs errStrm (vers, PT.Program{span, tree}) = let
          val {globals, globInit, strand, start, create, update} = tree
          val cxt = (errStrm, span)
          val env = E.new vers
          val {const_dcls, input_dcls, other_dcls, env} =
                CheckGlobals.check (defs, env, cxt, globals)
          val globInit' = Option.map
                (fn stm => CheckStmt.check (E.initScope env, cxt, stm)) globInit
          val (strand', env) = CheckStrand.check (env, cxt, strand)
          val create' = CheckCreate.check (env, cxt, create)
          val start' = (case start
                 of SOME stm => (
                      E.recordProp (env, Properties.GlobalStart);
                      SOME(CheckStmt.check(E.startScope env, cxt, stm)))
                  | NONE => NONE
                (* end case *))
          val update' = (case update
                 of SOME stm => (
                      E.recordProp(env, Properties.GlobalUpdate);
                      SOME(CheckStmt.check(E.updateScope env, cxt, stm)))
                  | NONE => NONE
                (* end case *))
          val props = E.properties env
          val prog = AST.Program{
                  props = props,
                  const_dcls = const_dcls,
                  input_dcls = input_dcls,
                  globals = other_dcls,
                  globInit = globInit',
                  strand = strand',
                  create = create',
                  start = start',
                  update = update'
                }
          in
          (* check for unused/uninitialized variables *)
            CheckVarUses.check (cxt, prog);
          (* check for new/die in a program that produces an array result *)
            if Properties.hasProp Properties.StrandArray props
              then let
                fun err kind = let
                      val cxt = (case create
                             of PT.CR_Mark{span, ...} => (#1 cxt, span)
                              | _ => cxt
                            (* end case *))
                      in
                        TypeError.error(cxt, [
                            TypeError.S "'create_array' conflicts with previous use of '",
                            TypeError.S kind,
                            TypeError.S "'; use 'create_collection' instead."
                          ])
                      end
                in
                  if Properties.hasProp Properties.StrandsMayDie props
                    then err "die"
                    else ();
                  if Properties.hasProp Properties.NewStrands props
                    then err "new"
                    else ()
                end
              else ();
            (prog, E.globalEnv env)
          end

  end
