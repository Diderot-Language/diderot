(* check-params.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckParams : sig

  (* check a list of function or strand parameters *)
    val check : Env.t * Env.context * Var.kind * ParseTree.param list -> Var.t list * Env.t

  end = struct

    structure PT = ParseTree

    fun check (env, cxt, kind, params) = let
          fun chkParam (env, cxt, param) = (case param
                 of PT.P_Mark m => chkParam (Env.withEnvAndContext (env, cxt, m))
                  | PT.P_Param(ty, {span, tree=x}) => let
                      val x' = Var.new(x, span, kind, CheckType.check (env, cxt, ty))
                      in
                        Env.checkForRedef (env, cxt, x);
                        (x', Env.insertLocal(env, cxt, x, x'))
                      end
                (* end case *))
          fun chk (param, (xs, env)) = let
                val (x, env) = chkParam (env, cxt, param)
                in
                  (x::xs, env)
                end
          in
            List.foldr chk ([], env) params
          end

  end
