(* translate-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * This functor supports the common parts of translating between different
 * instances of the SSA-based IRs (e.g., from HighIR to MidIR).
 *
 * FIXME: both this code and the Census code sets the bindings of variables (but the translation
 * from SimpleAST does not).
 *)

signature TRANSLATE_PARAMS =
  sig

    include TRANSLATE_ENV

    val expand : (t * SrcIR.assign) -> DstIR.cfg
    val mexpand : (t * SrcIR.massign) -> DstIR.cfg

  end

functor TranslateFn (Params : TRANSLATE_PARAMS) : sig

    structure SrcIR : SSA
    structure DstIR : SSA

    val translate : SrcIR.program -> DstIR.program

  end = struct

    structure SrcIR = Params.SrcIR
    structure SrcNd = SrcIR.Node
    structure VTbl = SrcIR.Var.Tbl
    structure DstIR = Params.DstIR
    structure DstNd = DstIR.Node
    structure DstCFG = DstIR.CFG

    fun rename env x = Params.rename (env, x)

    fun renameList (env, xs) = Params.renameList(env, xs)

    fun renameGV env x = Params.renameGV (env, x)

    fun renameSV env x = Params.renameSV (env, x)

    fun renameNd env (nd as SrcIR.ND{id, ...}) = (
          case Params.findNd env id
           of SOME nd' => nd'
            | NONE => raise Fail("unable to find " ^ SrcNd.toString nd)
          (* end case *))

    fun translateCFG (env, SrcIR.CFG{entry, exit}) = let
          val findNd = Params.findNd env
          fun cvtPhi (x, xs) = let
                val x = rename env x
                val xs = List.map (Option.map (rename env)) xs
                in
                  DstIR.Var.setBinding (x, DstIR.VB_PHI xs);
                  (x, xs)
                end
          fun trans (srcNd as SrcIR.ND{id, kind, ...}) = let
                fun newNd nd = (Params.insertNd (env, id, nd); nd)
                in
                  case findNd id
                   of SOME nd => nd
                    | NONE => (case kind
                         of SrcIR.NULL => raise Fail "unexpected NULL node"
                          | SrcIR.ENTRY{succ} => let
                              val nd = newNd (DstNd.mkENTRY())
                              in
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.JOIN{phis, succ, mask, ...} => let
                              val nd = newNd (DstNd.mkJOIN(List.map cvtPhi (!phis)))
                              in
                                DstNd.setEdgeMask (nd, !mask);
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.COND{cond, trueBranch, falseBranch, ...} => let
                              val nd = newNd (DstNd.mkCOND(rename env (!cond)))
                              val trueB = trans (!trueBranch)
                              val _ = (DstNd.setTrueBranch (nd, trueB); DstNd.setPred(trueB, nd))
                              val falseB = trans (!falseBranch)
                              val _ = (DstNd.setFalseBranch (nd, falseB); DstNd.setPred(falseB, nd))
                              in
                                nd
                              end
                          | SrcIR.FOREACH{var, src, phis, mask, bodyEntry, bodyExit, succ, ...} => let
                              val nd = newNd (DstNd.mkFOREACH(rename env var, rename env (!src)))
                              val bodyB = trans (!bodyEntry)
                              val SOME bodyE = findNd(SrcNd.id(!bodyExit))
                              in
                                DstNd.setPhis (nd, List.map cvtPhi (!phis));
                                DstNd.setEdgeMask (nd, !mask);
                                DstNd.setBodyEntry (nd, bodyB); DstNd.setPred(bodyB, nd);
                                DstNd.setBodyExit (nd, bodyE);
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.NEXT{succ, ...} => let
                              val nd = newNd (DstNd.mkNEXT())
                              in
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.COM{text, succ, ...} => let
                              val nd = newNd (DstNd.mkCOM text)
                              in
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.ASSIGN{stm, succ, ...} => let
                              val cfg = Params.expand (env, stm)
                              in
                                if DstCFG.isEmpty cfg
                                  then trans (!succ)
                                  else (
                                    DstNd.addEdge (DstCFG.exit cfg, trans (!succ));
                                    DstCFG.entry cfg)
                              end
                          | SrcIR.MASSIGN{stm, succ, ...} => let
                              val cfg = Params.mexpand (env, stm)
                              in
                                if DstCFG.isEmpty cfg
                                  then trans (!succ)
                                  else (
                                    DstNd.addEdge (DstCFG.exit cfg, trans (!succ));
                                    DstCFG.entry cfg)
                              end
                          | SrcIR.GASSIGN{lhs, rhs, succ, ...} => let
                              val nd = newNd (DstNd.mkGASSIGN (renameGV env lhs, rename env rhs))
                              in
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.NEW{strand, args, succ, ...} => let
                              val nd = newNd (DstNd.mkNEW(strand, List.map (rename env) args))
                              in
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.SAVE{lhs, rhs, succ, ...} => let
                              val nd = newNd (DstNd.mkSAVE (renameSV env lhs, rename env rhs))
                              in
                                DstNd.addEdge (nd, trans (!succ));
                                nd
                              end
                          | SrcIR.EXIT{kind, succ, ...} => let
                              val nd = newNd (DstNd.mkEXIT(ExitKind.map (rename env) kind))
                              in
                                case !succ
                                 of NONE => ()
                                  | SOME nd' => let (* add fake control-flow edges *)
                                      val nd' = trans nd'
                                      val DstIR.EXIT{succ, ...} = DstIR.Node.kind nd
                                      in
                                        succ := SOME nd';
                                        DstNd.addEdge (nd, nd')
                                      end
                                (* end case *);
                                nd
                              end
                       (* end case *))
                  (* end case *)
                end
          val entry = trans entry
          val exit = (case findNd (SrcNd.id exit)
                 of SOME nd => nd
                  | NONE => DstNd.mkACTIVE()    (* exit is unreachable *)
                (* end case *))
          in
            DstIR.CFG{entry = entry, exit = exit}
          end

    fun translate prog = let
          val SrcIR.Program{
                  props, consts, inputs, constInit, globals,
                  funcs, globInit, strand, create, start, update
                } = prog
          val env = Params.mkEnv ()
          fun translateCFG' cfg = translateCFG (env, cfg)
        (* rename constants and inputs *)
          val consts' = List.map (renameGV env) consts
          val inputs' = List.map (Inputs.map (renameGV env)) inputs
          val globals' = List.map (renameGV env) globals
        (* translate the constant initialization section *)
          val constInit' = translateCFG (env, constInit)
        (* register function definitions for translation on demand *)
          val () = let
                fun registerFunc (SrcIR.Func{name, params, body}) = let
                      fun mk (env, name') = DstIR.Func{
                              name = name',
                              params = renameList (env, params),
                              body = translateCFG (env, body)
                            }
                      in
                        Params.registerFunc (env, name, mk)
                      end
                in
                  List.app registerFunc funcs
                end
        (* translate the global initialization section *)
          val globInit' = translateCFG (env, globInit)
        (* translate the strand definition *)
          val strand' = let
                val SrcIR.Strand{
                        name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                      } = strand
                val params = renameList (env, params)
                val state = List.map (renameSV env) state
                in
                  List.app (fn x => DstIR.Var.setBinding(x, DstIR.VB_PARAM)) params;
                  DstIR.Strand{
                      name = name,
                      params = params,
                      spatialDim = spatialDim,
                      state = state,
                      stateInit = translateCFG (env, stateInit),
                      startM = Option.map translateCFG' startM,
                      updateM = translateCFG (env, updateM),
                      stabilizeM = Option.map translateCFG' stabilizeM
                    }
                end
        (* translate the initial strand creation code *)
          val create' = Create.map (fn code => translateCFG (env, code)) create
        (* translate the global start code *)
          val start' = Option.map translateCFG' start
        (* translate the global update code *)
          val update' = Option.map translateCFG' update
          in
            DstIR.Program{
                props = props,
                consts = consts',
                inputs = inputs',
                constInit = constInit',
                globals = globals',
                funcs = Params.getFuncs env,
                globInit = globInit',
                strand = strand',
                create = create',
                start = start',
                update = update'
              }
          end

  end
