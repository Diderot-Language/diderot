(* mid-opt.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Optimization of the MidIR representation of Diderot terms.  The main
 * task of this phase is to statically resolve field definitions.
 *)

structure MidOptimizer : sig

    val optimize : MidIR.program -> MidIR.program

    val checkAfter : string * MidIR.program -> MidIR.program

  end = struct

  (* Value numbering for MidIR *)
    structure VN = ValueNumberingFn (DomTree)

    val checkAfter = Log.after {
            dumpCtl = Ctl.dumpMidIR,
            checkCtl = Ctl.checkMidIR,
            output = MidPP.output,
            checkIR = PhaseTimer.withTimer Timers.timeMidCheck (fn arg => CheckMid.check arg)
          }

    fun transform (ctl, timer, phase, transform, prog) =
          if Controls.get ctl
            then checkAfter (phase, PhaseTimer.withTimer timer transform prog)
            else prog

    fun transform' (timer, phase, transform, prog) =
          checkAfter (phase, PhaseTimer.withTimer timer transform prog)

    fun optimize prog = let
          val prog = transform (Ctl.midVN, Timers.timeMidVN, "value numbering", VN.transform, prog)
          val prog = transform (Ctl.midContract, Timers.timeMidContract, "contraction", MidContract.transform, prog)
          val prog = transform' (Timers.timeMidBorderCtl, "border control", BorderCtl.transform, prog)
          in
            prog
          end

  end
