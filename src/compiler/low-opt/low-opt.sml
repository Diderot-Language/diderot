(* low-opt.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Optimization of the LowIR representation of Diderot terms.
 *)

structure LowOptimizer : sig

    val optimize : LowIR.program -> LowIR.program

    val checkAfter : string * LowIR.program -> LowIR.program

  end = struct

  (* Value numbering for LowIR *)
    structure VN = ValueNumberingFn (DomTreeFn(LowIR))

    val checkAfter = Log.after {
            dumpCtl = Ctl.dumpLowIR,
            checkCtl = Ctl.checkLowIR,
            output = LowPP.output,
            checkIR = PhaseTimer.withTimer Timers.timeLowCheck (fn arg => CheckLow.check arg)
          }

    fun transform (ctl, timer, phase, transform, prog) =
          if Controls.get ctl
            then checkAfter (phase, PhaseTimer.withTimer timer transform prog)
            else prog

    fun optimize prog = let
          val prog = transform (Ctl.lowContract, Timers.timeLowContract, "contraction(1)", LowContract.transform, prog)
          val prog = transform (Ctl.lowVN, Timers.timeLowVN, "value numbering", VN.transform, prog)
          val prog = transform (Ctl.lowContract, Timers.timeLowContract, "contraction(2)", LowContract.transform, prog)
          in
            prog
          end

  end
