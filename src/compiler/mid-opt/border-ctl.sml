(* border-ctl.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure BorderCtl : sig

    val transform : MidIR.program -> MidIR.program

  end = struct

    structure IR = MidIR
    structure Op = MidOps
    structure Ty = MidTypes
    structure V = IR.Var
    structure Nd = IR.Node
    structure DT = DomTree

    val use = MidCensus.use

  (* an environment that tracks available inside tests.  We map position-image
   * variable pairs to the support value.  I.e., "inside<info,s>(n,V)" adds
   * the mapping
   *
   *    (n, V) --> s
   *)
    structure Env = RedBlackMapFn (struct
        type ord_key = IR.var * IR.var
        fun compare ((x, f), (y, g)) = (case V.compare(f, g)
               of EQUAL => V.compare(x, y)
                | order => order
              (* end case *))
      end)

    type env = int Env.map

    fun getInsideTest cond = (case V.getDef cond
           of IR.OP(Op.Inside(_, s), [pos, img]) => SOME(pos, img, s)
            | _ => NONE
          (* end case *))

  (* for a node "x = LoadVoxelsWithCtl<I,s,ctl>(V, n)", replace it with
   *
   *    if (IndexInside<I,s> (n, V)) {
   *      x1 = LoadVoxels<I,s>(V, n)
   *    } else {
   *      x2 = LoadVoxelsWithCtl<I,s,ctl>(V, n)
   *    }
   *    x = phi(x1, x2)
   *)
    fun rewriteLoad origNd = (case Nd.kind origNd
           of IR.ASSIGN{
                stm = (x, rhs as IR.OP(Op.LoadVoxelsWithCtl(info, s, ctl), args as [img, pos])),
                ...
              } => let
                val d = (case V.ty pos
                      of Ty.IntTy  => 1
                       | Ty.SeqTy(Ty.IntTy, SOME d) => d
                       | ty => raise Fail("bogus type for voxel index: " ^ Ty.toString ty)
                      (* end case *))
                val x1 = V.copy x
                val x2 = V.copy x
                val cond = V.new("isInside", Ty.BoolTy)
                val loadNd = Nd.mkASSIGN(x1, IR.OP(Op.LoadVoxels(info,s), [use img, use pos]))
                val loadWithCtlNd = Nd.mkASSIGN(x2, rhs)
                val insideTstNd =
                      Nd.mkASSIGN(cond, IR.OP(Op.IndexInside(info,s), [use pos, use img]))
                val condNd = Nd.mkCOND(use cond)
                val joinNd = Nd.mkJOIN[(x, [SOME(use x1), SOME(use x2)])]
                in
                (* hook up edges *)
                  Nd.addEdge (insideTstNd, condNd);
                  Nd.setTrueBranch(condNd, loadNd);
                  Nd.setPred (loadNd, condNd);
                  Nd.setFalseBranch (condNd, loadWithCtlNd);
                  Nd.setPred (loadWithCtlNd, condNd);
                  Nd.setEdgeMask (joinNd, [false, false]);
                  Nd.addEdge (loadNd, joinNd);
                  Nd.addEdge (loadWithCtlNd, joinNd);
                (* replace the original node with the new CFG *)
                  IR.CFG.replaceNodeWithCFG (origNd, IR.CFG{entry=insideTstNd, exit=joinNd})
                end
            | _ => raise Fail "expected LoadVoxelsWithCtl"
          (* end case *))

    fun transformCFG cfg = let
        (* a list of nodes that need to be rewritten *)
          val workList = ref[]
        (* walk the dominator tree *)
          fun walk (env, nd) = let
                fun kids env = List.app (fn nd => walk (env, nd)) (DT.children nd)
                in
                  case Nd.kind nd
                   of IR.COND{cond, ...} => (case getInsideTest (!cond)
                         of SOME(pos, img, s) => (case DT.children nd
                               of [trueBr, falseBr] => (
                                  (* add the inside test to the walk of the true branch *)
                                    walk (Env.insert (env, (pos, img), s), trueBr);
                                    walk (env, falseBr))
                                | _ => kids env
                              (* end case *))
                          | NONE => kids env
                        (* end case *))
                    | IR.ASSIGN{stm=(y, IR.OP(Op.LoadVoxelsWithCtl(info, s, _), [V, n])), ...} =>
                        let
                        fun addToList () = (workList := nd :: !workList; kids env)
                        in
                          case Env.find (env, (n, V))
                           of SOME s' => if (s <= s')
                                then let
                                (* no need for an inside test; replace node with LoadVoxels *)
                                  val newNd = Nd.mkASSIGN(y, IR.OP(Op.LoadVoxels(info, s), [V, n]))
                                  in
                                    IR.CFG.replaceNode (nd, newNd);
                                    kids env
                                  end
                                else addToList()
                            | NONE => addToList()
                          (* end case *)
                        end
                    | _ => kids env
                  (* end case *)
                end
          in
            DT.computeTree cfg;
          (* compute border control info *)
            walk (Env.empty, IR.CFG.entry cfg);
            DT.clear cfg;
          (* add inside tests to protect border control *)
            List.app rewriteLoad (!workList)
          end

    fun transform prog = let
          val IR.Program{funcs, globInit, strand, create, start, update, ...} = prog
        (* transform a user-defined function *)
          fun transformFunc (IR.Func{body, ...}) = transformCFG body
        (* transform a strand *)
          fun transformStrand (IR.Strand{stateInit, startM, updateM, stabilizeM, ...}) = (
                transformCFG stateInit;
                Option.app transformCFG startM;
                transformCFG updateM;
                Option.app transformCFG stabilizeM)
          in
          (* NOTE: we don't process the constInit code, because it is trivial *)
            List.app transformFunc funcs;
            transformCFG globInit;
            transformStrand strand;
            Create.app transformCFG create;
            Option.app transformCFG start;
            Option.app transformCFG update;
            prog
          end

  end
