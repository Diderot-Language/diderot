(* check-create.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckCreate : sig

  (* type check the initial strand creation code *)
    val check : Env.t * Env.context * ParseTree.create -> AST.create

  end = struct

    structure PT = ParseTree
    structure E = Env
    structure Ty = Types

    datatype token = datatype TypeError.token

    fun stripMark (_, PT.E_Mark{span, tree}) = stripMark(span, tree)
      | stripMark (span, e) = (span, e)

  (* process the iterators; note that they are in slow-to-fast order (or outer-to-inner) *)
    fun checkIters (env0, cxt, iters) = let
        (* check an iteration range specification from the creation code.  We do the checking
         * of the expressions using env0, which does not have any of the iteration variables in
         * it (the iteration is rectangular), but we also accumulate the iteration bindings,
         * which are used to create the final environment for checking the create call.
         *)
          fun checkIter (env, cxt, PT.I_Mark m) = checkIter (E.withEnvAndContext (env, cxt, m))
            | checkIter (env, cxt, PT.I_Iterator({span, tree=x}, e)) = (
                case CheckExpr.check (env, cxt, e)
                 of (e', ty as Ty.T_Sequence(elemTy, _)) => let
                      val x' = Var.new(x, span, Var.LocalVar, elemTy)
                      in
                        ((x', e'), (x, x'))
                      end
                  | (e', ty) => let
                      val x' = Var.new(x, span, Var.IterVar, Ty.T_Error)
                      in
                        TypeError.error (cxt, [
                            S "expected sequence type in iteration, but found '", TY ty, S "'"
                          ]);
                        ((x', e'), (x, x'))
                      end
                (* end case *))
          fun chk ([], iters, bindings) =
                (List.rev iters, List.foldl (fn ((x, x'), env) => E.insertLocal(env, cxt, x, x')) env0 bindings)
            | chk (iter::rest, iters, bindings) = let
                val (iter, binding) = checkIter (env0, cxt, iter)
                in
                  chk (rest, iter::iters, binding::bindings)
                end
          in
            chk (iters, [], [])
          end

    fun loopDepth (PT.COMP_Mark{tree, ...}) = loopDepth tree
      | loopDepth (PT.COMP_Comprehension(_, iters)) = List.length iters

    fun checkComp (env, cxt, PT.COMP_Mark m) = checkComp (E.withEnvAndContext (env, cxt, m))
      | checkComp (env, cxt, PT.COMP_Comprehension(e, iters)) = let
        (* check the iterations *)
          val (iters, env) = checkIters (env, cxt, iters)
          fun mkLoopNest ([], body) = AST.S_New body
            | mkLoopNest (iter::iters, body) = AST.S_Foreach(iter, mkLoopNest(iters, body))
        (* error message for illformed creation expression *)
          fun illformed cxt = (
                TypeError.error (cxt, [S "expected strand creation"]);
                mkLoopNest (iters, (Atom.atom "*bogus*", [])))
          in
          (* check the expression that is the body of the comprehension.  This
           * should have the form (once marks are removed)
           *
           *    E_Apply(E_Var strand, args)
           *)
            case stripMark(#2 cxt, e)
             of (span, PT.E_Apply(strand, args)) => (case stripMark(span, strand)
                   of (_, PT.E_Var s) => (case E.findStrand(env, s)
                         of SOME sEnv => let
                              val paramTys = StrandEnv.paramTys sEnv
                              val (args, argTys) = CheckExpr.checkList (env, cxt, args)
                              fun mismatch () = (
                                    TypeError.error ((#1 cxt, span), [
                                          S "type mismatch in strand creation\n",
                                          S "  expected: ", TYS paramTys, S "\n",
                                          S "  found:    ", TYS argTys
                                        ]);
                                    [])
                              fun chkArgs ([], [], [], args') = List.rev args'
                                | chkArgs (paramTy::paramTys, arg::args, ty::tys, args') = (
                                    case Util.coerceType (paramTy, (arg, ty))
                                     of SOME arg' => chkArgs (paramTys, args, tys, arg'::args')
                                      | NONE => mismatch()
                                    (* end case *))
                                | chkArgs _ = mismatch()
                              val args = chkArgs (paramTys, args, argTys, [])
                              in
                                mkLoopNest (iters, (s, args))
                              end
                          | NONE => (
                              TypeError.error (cxt, [A s, S " is not the name of a strand"]);
                              mkLoopNest (iters, (s, [])))
                        (* end case *))
                    | _ => illformed (#1 cxt, span)
                  (* end case *))
              | _ => illformed cxt
            (* end case *)
          end

    fun check (env, cxt, PT.CR_Mark m) = check (E.withEnvAndContext (env, cxt, m))
      | check (env, cxt, PT.CR_Collection comp) =
          Create.collection (checkComp (E.createScope env, cxt, comp))
      | check (env, cxt, PT.CR_Array(frame, comp)) = let
          val env = E.createScope env
          val loop = checkComp (env, cxt, comp)
          val dim = loopDepth comp
          in
            E.recordProp (env, Properties.StrandArray);
(* FIXME: typecheck optional frame once the syntax supports it *)
(* QUESTION: should there be limits in the dimension? *)
            Create.array(dim, loop)
          end

  end
