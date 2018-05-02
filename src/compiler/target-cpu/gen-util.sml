(* gen-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenUtil : sig

  (* generate code for a block, where we optionally add a local variable that points
   * to the globals struct.
   *)
    val genBodyWithGlobPtr : CodeGenEnv.t * TreeIR.block -> CLang.stm

  end = struct

    structure IR = TreeIR
    structure Env = CodeGenEnv
    structure RN = CxxNames
    structure CL = CLang

    fun genBodyWithGlobPtr (env, IR.Block{locals, body}) = let
          fun mkBody env = let
                val spec = Env.target env
                val stms = #2(TreeToCxx.trStms(env, body))
                val stms = if #hasGlobals spec orelse #hasConsts spec
                      then CL.mkDeclInit(
                          RN.globalPtrTy, RN.globalsVar,
                          CL.mkIndirect(CL.mkVar(Env.world env), "_globals")
                        ) :: stms
                      else stms
                in
                  stms
                end
          in
            TreeToCxx.trWithLocals (env, !locals, mkBody)
          end

  end
