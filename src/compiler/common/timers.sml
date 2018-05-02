(* timers.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Timers =
  struct

    val timeCompiler = PhaseTimer.newTimer "compiler"
    val timeFront = PhaseTimer.newPhase (timeCompiler, "front end")
    val timeParser = PhaseTimer.newPhase (timeFront, "parser")
    val timeTypechecker = PhaseTimer.newPhase (timeFront, "typechecker")
    val timeSimplify = PhaseTimer.newPhase (timeFront, "simplify")
    val timeTranslate = PhaseTimer.newPhase (timeCompiler, "translate")
    val timeHigh = PhaseTimer.newPhase (timeCompiler, "High IR optimization")
    val timeHighVN = PhaseTimer.newPhase (timeHigh, "High IR value numbering")
    val timeHighNorm = PhaseTimer.newPhase (timeHigh, "High IR normalization")
    val timeHighCheck = PhaseTimer.newPhase (timeHigh, "High IR checking")
    val timeHighToMid = PhaseTimer.newPhase (timeCompiler, "High to Mid translation")
    val timeMid = PhaseTimer.newPhase (timeCompiler, "Mid IR optimization")
    val timeMidContract = PhaseTimer.newPhase (timeMid, "Mid IR contraction")
    val timeMidVN = PhaseTimer.newPhase (timeMid, "Mid IR value numbering")
    val timeMidBorderCtl = PhaseTimer.newPhase (timeMid, "Mid IR border control")
    val timeMidCheck = PhaseTimer.newPhase (timeMid, "Mid IR checking")
    val timeMidToLow = PhaseTimer.newPhase (timeCompiler, "Mid to Low translation")
    val timeLow = PhaseTimer.newPhase (timeCompiler, "Low IR optimization")
    val timeLowContract = PhaseTimer.newPhase (timeLow, "Low IR contraction")
    val timeLowVN = PhaseTimer.newPhase (timeLow, "Low IR value numbering")
    val timeLowCheck = PhaseTimer.newPhase (timeLow, "Low IR checking")
    val timeLowToTree = PhaseTimer.newPhase (timeCompiler, "Low to Tree translation")
    val timeTree = PhaseTimer.newPhase (timeCompiler, "Tree IR optimization")
    val timeTreeContract = PhaseTimer.newPhase (timeTree, "Tree IR contraction")
    val timeTreeCheck = PhaseTimer.newPhase (timeTree, "Tree IR checking")
    val timeCodegen = PhaseTimer.newPhase (timeCompiler, "code generation")
    val timeCC = PhaseTimer.newPhase (timeCodegen, "C compiler")

  end
