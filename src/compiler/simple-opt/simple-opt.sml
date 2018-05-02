(* simple-opt.sml
 *
 * Optimization of the SimpleAST representation
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure SimpleOpt : sig

    val transform : Simple.program -> Simple.program

    val checkAfter : string * Simple.program -> Simple.program

  end = struct

    structure S = Simple

    val checkAfter = Log.after {
            dumpCtl = Ctl.dumpSimple,
            checkCtl = Ctl.dumpSimple,
            output = SimplePP.output,
            checkIR = CheckSimple.check
          }

    fun transform prog = let
          val prog = checkAfter ("contraction (1)", SimpleContract.transform prog)
          val prog = checkAfter ("map-reduce-fusion", MapReduceOpt.transform prog)
          val prog = if Controls.get Ctl.inline
                then checkAfter ("inlining", Inliner.transform prog)
                else prog
        (* second round of contraction after inlining *)
          val prog = if Controls.get Ctl.inline
                then checkAfter ("contraction (2)", SimpleContract.transform prog)
                else prog
          val prog = checkAfter ("simplify fields", SimplifyFields.transform prog)
          val prog = checkAfter ("simplify variables", SimplifyVars.transform prog)
          val prog = CheckForBSP.check prog
          in
            prog
          end

  end
