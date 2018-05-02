(* high-opt.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Optimization of the HighIR representation of Diderot terms.  The main
 * task of this phase is to statically resolve field definitions.
 *)

structure HighOptimizer : sig

    val optimize : HighIR.program -> HighIR.program

    val checkAfter : string * HighIR.program -> HighIR.program

  end = struct

  (* Value numbering for HighIR *)
    structure VN = ValueNumberingFn (DomTreeFn (HighIR))

    val vnFlag = ref true               (* controls value numbering *)
    val debugFlag = ref true            (* controls printing *)
    val checkFlag = ref true            (* controls IR checking *)

    val checkAfter = Log.after {
            dumpCtl = Ctl.dumpHighIR,
            checkCtl = Ctl.checkHighIR,
            output = HighPP.output,
            checkIR = PhaseTimer.withTimer Timers.timeHighCheck (fn arg => CheckHigh.check arg)
          }

  (* used to run the Normalize.rewrite phase; we cannot run checks, since some locals
   * may have had their scopes extended and thus would trigger use-count failures.
   * The Normalize.promote phase will fix those issues.
   *)
    val noCheckAfter = Log.after {
            dumpCtl = Ctl.dumpHighIR,
            checkCtl = Ctl.checkHighIR,
            output = HighPP.output,
            checkIR = fn arg => false
          }

    fun transform (flag, timer, phase, transform, prog) =
          if flag
            then checkAfter (phase, PhaseTimer.withTimer timer transform prog)
            else prog

    fun optimize prog = let
          val prog = transform (Controls.get Ctl.highVN, Timers.timeHighVN, "value numbering", VN.transform, prog)
          val prog = noCheckAfter ("normalization rewriting",
                PhaseTimer.withTimer Timers.timeHighNorm Normalize.rewrite prog)
          val prog = transform (true, Timers.timeHighNorm, "normalization promotion", Normalize.promote, prog)
          in
            prog
          end

  end
