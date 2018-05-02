(* tree-opt.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Optimization of the TreeIR representation of Diderot terms.
 *)

structure TreeOptimizer : sig

    val optimize : TreeIR.program -> TreeIR.program

    val checkAfter : string * TreeIR.program -> TreeIR.program

  end = struct

    val checkAfter = Log.after {
            dumpCtl = Ctl.dumpTreeIR,
            checkCtl = Ctl.checkTreeIR,
            output = TreePP.output,
            checkIR = PhaseTimer.withTimer Timers.timeTreeCheck (fn arg => CheckTree.check arg)
          }

    fun transform (ctl, timer, phase, transform, prog) =
          if Controls.get ctl
            then checkAfter (phase, PhaseTimer.withTimer timer transform prog)
            else prog

    fun optimize prog = let
          val prog = transform (Ctl.treeContract, Timers.timeTreeContract, "contraction", TreeContract.transform, prog)
          in
            prog
          end

  end
