(* check-strand.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckStrand : sig

    val check : Env.t * Env.context * ParseTree.strand_dcl -> AST.strand * Env.t

  end = struct

    structure PT = ParseTree
    structure TU = TypeUtil
    structure E = Env

    val err = TypeError.error

    datatype token = datatype TypeError.token

    fun check (env, cxt, PT.SD_Mark m) = check (E.withEnvAndContext (env, cxt, m))
      | check (env0, cxt, PT.SD_Strand{name={span, tree=name}, params, stateInit, state, methods}) =
          let
          val env = E.strandScope(env0, name)
          val (params, env) = CheckParams.check (env, cxt, Var.StrandParam, params)
          val sEnv = StrandEnv.new (name, params)
        (* check the strand state-variable definitions *)
          val (vds, hasOutput, env) = let
                fun chkStateVar (env, cxt, PT.SVD_Mark m) =
                      chkStateVar (E.withEnvAndContext(env, cxt, m))
                  | chkStateVar (env, cxt, PT.SVD_VarDcl(isOut, vdcl)) = let
                      val kind = if isOut then AST.StrandOutputVar else AST.StrandStateVar
                      val (x, x', e') = CheckStmt.checkVarDecl (env, cxt, kind, vdcl)
                      in
                      (* check that strand output variables have value types *)
                        if not(TU.isValueType(Var.monoTypeOf x'))
                          then err (cxt, [
                              S "strand-state variable ", V x', S " has non-value type ",
                              TY(Var.monoTypeOf x')
                            ])
                          else ();
                      (* add variable to strand environment *)
                        StrandEnv.insertStateVar (sEnv, x, x');
                      (* check for redefinition *)
                        E.checkForRedef (env, cxt, x);
                      (* return isOut, AST decl, and extended environment *)
                        (isOut, (x', e'), E.insertLocal(env, cxt, x, x'))
                      end
                fun chkStateVar' (vd, (vds, hasOut, env)) = let
                      val (isOut, vd', env) = chkStateVar (env, cxt, vd)
                      in
                        (vd'::vds, hasOut orelse isOut, env)
                      end
                val (vds, hasOutput, env) = List.foldl chkStateVar' ([], false, env) state
                in
                  (List.rev vds, hasOutput, env)
                end
        (* check the additional state initialization code (if present) *)
          val stateInit = Option.map (fn stm => CheckStmt.check (env, cxt, stm)) stateInit
        (* add the strand to the global environment *)
          val env = E.insertStrand (env, (#1 cxt, span), sEnv)
        (* check the strand methods *)
          val startMeth = ref NONE
          val updateMeth = ref NONE
          val stabilizeMeth = ref NONE
          fun checkMethod (env, cxt, meth) = (case meth
                 of PT.M_Mark m => checkMethod (E.withEnvAndContext (env, cxt, m))
                  | PT.M_Method(name, body) => let
                      val body = CheckStmt.check (E.methodScope (env, name), cxt, body)
                      fun addMethod (name, ref(SOME _)) =
                            err (cxt, [S "duplicate definitions of '", S name, S " method"])
                        | addMethod (_, r) = r := SOME body
                      in
                        case name
                         of StrandUtil.Start => (
                              Env.recordProp (env, Properties.HasStartMethod);
                              addMethod ("start", startMeth))
                          | StrandUtil.Update => addMethod ("update", updateMeth)
                          | StrandUtil.Stabilize => (
                              Env.recordProp (env, Properties.HasStabilizeMethod);
                              addMethod ("stabilize", stabilizeMeth))
                        (* end case *)
                      end
                (* end case *))
          val methods = List.app (fn m => checkMethod (env, cxt, m)) methods
        (* build the strand *)
          val strand = AST.Strand{
                  name = name,
                  params = params,
                  spatialDim = StrandEnv.getSpaceDim sEnv,
                  state = vds,
                  stateInit = stateInit,
                  startM = !startMeth,
                  updateM = (case !updateMeth
                     of SOME stm => stm
                      | NONE => (
                          err (cxt, [S "strand ", A name, S " is missing an update method"]);
                          AST.S_Block[])
                    (* end case *)),
                  stabilizeM = !stabilizeMeth
                }
          in
(* FIXME: once there are global outputs, then it should be okay to have not strand outputs! *)
          (* check that there is at least one output variable *)
            if not hasOutput
              then err (cxt, [S "strand ", A name, S " does not have any outputs"])
              else ();
          (* we return the strand and the top-level environment with the strand added as a binding *)
            (strand, env0)
          end

  end
