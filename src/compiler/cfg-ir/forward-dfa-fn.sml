(* forward-dfa-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Forward data-flow analysis for IR code.
 *)

functor ForwardDFAFn (D : DOMAIN) : sig

    structure D : DOMAIN

  (* given the entry value and root statement, do the forward DFA on the CFG and
   * return the list of nodes that were analysed.
   *)
    val analyse : D.t * D.IR.cfg -> D.IR.node list

  (* get results for a node *)
    val inValue : D.IR.node -> D.t
    val outValue : D.IR.node -> D.t

  (* scrub results to reclaim space *)
    val scrub : D.IR.node list -> unit

  end = struct

    structure D = D
    structure IR = D.IR

    type result = IR.node list

    val {setFn=setIn, getFn=getIn, clrFn= clrIn, ...} =
          PropList.newProp (fn (IR.ND{props, ...}) => props, fn _ => D.bottom)
    val {setFn=setOut, getFn=getOut, clrFn=clrOut, ...} =
          PropList.newProp (fn (IR.ND{props, ...}) => props, fn _ => D.bottom)

    fun clear node = (clrIn node; clrOut node)

    fun analyse (entryVal, cfg) = let
(* DEBUG val t = Timer.startCPUTimer()*)
        (* use DFS order to get quicker convergence *)
          val nodes as entry::rest = IR.CFG.sort cfg
(*DEBUG val nIters = ref 0*)
(*DEBUG val nVisits = ref 0*)
        (* the first pass does not do change propagation, since we haven't computed anything yet *)
          fun initNode nd = let
                val inValue = D.join (List.map getOut (IR.Node.preds nd))
                val outValue = D.transfer (inValue, nd)
                in
(*DEBUG nVisits := !nVisits + 1;*)
                  setIn (nd, inValue);
                  setOut (nd, outValue)
                end
        (* once nodes are initialized, we iterate until we reach a fixed point *)
          fun iterate () = let
                val anyChange = ref false
                fun doNode nd = let
                      val inValue = D.join (List.map getOut (IR.Node.preds nd))
                      in
                        if D.same(getIn nd, inValue)
                          then () (* input unchanged, so output will be unchanged *)
                          else let
                            val outValue = D.transfer (inValue, nd)
                            in
(*DEBUG nVisits := !nVisits + 1;*)
                              anyChange := true;
                              setIn (nd, inValue);
                              if D.same(getOut nd, outValue)
                                then ()
                                else setOut(nd, outValue)
                            end
                      end
                in
(*DEBUG nIters := !nIters + 1;*)
                  List.app doNode rest;
                  if !anyChange then iterate() else ()
                end
          in
          (* initialize the output of the CFG entry node *)
            setOut (entry, entryVal);
          (* initialize the rest of the nodes *)
            List.app initNode rest;
          (* iterate to a fixed point *)
            iterate ();
(*DEBUG*
let val gcTime = Timer.checkGCTime t
val {usr, sys} = Timer.checkCPUTimer t
in
print(concat[IR.irName, " DFA: cpu = ", Time.toString(Time.+(usr, sys)),
" seconds, gc = ", Time.toString gcTime, " seconds, ",
Int.toString(List.length rest + 1), " nodes, ",
Int.toString(!nVisits), " visits, ",
Int.toString(!nIters), " iterations\n"])
end;
*DEBUG*)
            nodes
          end

    fun inValue nd = getIn nd
    fun outValue nd = getOut nd

    fun scrub nodes = List.app clear nodes

  end
