(* assign-types.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure AssignTypes : sig

  (* analyze the local variables in a CFG to determine which ones should have memory
   * allocated for them (as opposed to being represented as references)
   *)
    val analyze : LowIR.cfg -> unit

  (* return true if the variable should have memory allocated for it *)
    val isMemoryVar : LowIR.var -> bool

  end = struct

    structure IR = LowIR
    structure Ty = LowTypes
    structure Op = LowOps
    structure UV = UnifyVars

    val {setFn, getFn} = IR.Var.newFlag ()

    fun needsMemory (Ty.TensorTy(_::_::_)) = true
      | needsMemory (Ty.SeqTy(_, SOME _)) = true
      | needsMemory _ = false

  (* mark a variable; for those in an equivalence class, we mark the representative *)
    fun markVar x = (case UnifyVars.eqClassOf x
           of SOME y => setFn (y, true)
            | NONE => setFn (x, true)
        (* end case *))

    fun isMemoryVar x = (case UnifyVars.eqClassOf x
           of SOME y => getFn y
            | NONE => getFn x
        (* end case *))

    fun analyze cfg = let
          fun doNode nd = (case IR.Node.kind nd
                 of IR.ASSIGN{stm = (x, rhs), ...} => (case rhs
                       of IR.CONS(_, Ty.TensorTy(_::_::_)) => markVar x
                        | IR.SEQ _ => markVar x
                        | IR.VAR y => if isMemoryVar y then markVar x else ()
                        | _ => ()
                      (* end case *))
                  | IR.MASSIGN{stm=([vals, vecs], IR.OP(Op.EigenVecs2x2, _)), ...} => (
                      markVar vals; markVar vecs)
                  | IR.MASSIGN{stm=([vals, vecs], IR.OP(Op.EigenVecs3x3, _)), ...} => (
                      markVar vals; markVar vecs)
                  | IR.GASSIGN{lhs, rhs, ...} => if needsMemory(IR.GlobalVar.ty lhs)
                      then markVar rhs
                      else ()
                  | IR.SAVE{lhs, rhs, ...} => if needsMemory(IR.StateVar.ty lhs)
                      then markVar rhs
                      else ()
                  | _ => ()
                (* end case *))
          in
            IR.CFG.apply doNode cfg
          end

  end
