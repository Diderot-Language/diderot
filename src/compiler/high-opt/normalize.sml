(* normalize.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure Normalize : sig

    val rewrite : HighIR.program -> HighIR.program

    val promote : HighIR.program -> HighIR.program

  end = struct

    structure IR = HighIR
    structure Op = HighOps
    structure V = IR.Var
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntUnused               = ST.newCounter "high-opt:unused"

    structure UnusedElim = UnusedElimFn (
        structure IR = IR
        val cntUnused = cntUnused)

    fun useCount (IR.V{useCnt, ...}) = !useCnt

  (* adjust a variable's use count *)
    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun decUse (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)
    fun use x = (incUse x; x)

(*** OLD VERSION
    fun getEinApp x = (case V.getDef x
           of IR.EINAPP(e, arg) => SOME(e, arg)
            | _ => NONE
          (* end case *))
****)
  (* get the EIN application that "x" is bound to (if any).  Note that we are
   * conservative on following globals so as to avoid duplicating computation.
   *)
    fun getEinApp x = let
          fun getEinRHS (IR.EINAPP app) = SOME app
            | getEinRHS _ = NONE
          in
            case V.ty x
             of HighTypes.KernelTy => getEinRHS(V.getDef x)
              | HighTypes.FieldTy => getEinRHS(V.getDef x)
              | _ => getEinRHS(V.getLocalDef x)
            (* end case *)
          end

 (* doNormalize : EIN -> EIN
  * Orders EIN, normalizes it, then cleans the summation
  *)
    fun doNormalize e' = let
          val ordered = Reorder.transform e'
          val rtn = case NormalizeEin.transform ordered
             of NONE => ordered
              | SOME e => EinSums.clean e
            (* end case *)
          in
            rtn
          end

(* FIXME: what does this function do? *)
   (* rewriteEin : _ -> boolean * EIN * int * HighIR.var list
    *
    * Parameters:
    *    changed        -- boolean that is true if any rewriting has been done
    *    origParams     -- Updated parameters for main EIN operator
    *    origEinOp      -- Updated EIN operator for the main EIN operator
    *    origArgs       -- Updated arguments for the main EIN operator
    *    place          -- integer that represents which argument in main operator we are examining
    *    newLHS         -- lhs variable for the new replaced argument
    *    newEinOp       -- EIN operator for new replaced argument
    *    newArgs        -- Arguments for the new replaced argument
    *)
    fun rewriteEin (changed,  origParams, origEinOp, origArgs, place, newLHS, newEinOp, newArgs) = (
          case List.nth(origParams, place)
           of Ein.TEN(false, _) => (
                (changed, origEinOp, place+1, origArgs@[newLHS]))
            | _ => (case Apply.apply (origEinOp, place, newEinOp, newArgs, origArgs)
                 of SOME einOp => ( (* einOp is the result of the beta-reduction *)
                      decUse newLHS; List.app incUse newArgs;
                      (true, einOp, place + length newArgs, origArgs @ newArgs))
                  | NONE => ((* arg was unused in origEinOp so we can get rid of it *)
                      decUse newLHS;
                      (true, origEinOp, place, origArgs))
                (* end case *))
          (* end case *))

(* FIXME: it would be much more efficient to do all of the substitutions in one go,
 * instead of repeatedly rewriting the term for each argument.
 *)
  (* doRHS: HighIR.var * rhs -> (var * rhs) list option
   * Looks at each argument to the original EINAPP.
   * If it is another EIN APP calls rewriteEin to do application
   * "place"-The Param-id for the EIN operator.
   * Keeps track of the place of the argument in substitution.
   *)
    fun doRHS (lhs, IR.EINAPP(ein, args)) = let
          fun rewrite (false, _, _, [], _) = NONE
            | rewrite (true, origEinOp, _, [], origArgs) =
                SOME[(lhs, IR.EINAPP(doNormalize origEinOp, origArgs))]
            | rewrite (changed, origEinOp, place,  newLHS::xs, origArgs) = (
                case getEinApp newLHS
                 of NONE => rewrite (changed, origEinOp, place+1, xs, origArgs@[ newLHS])
                  | SOME(newEinOp, newArgs) => let
                      val Ein.EIN{params, ...} = origEinOp
                      val (changed, origEinOp', place', origArgs') = rewriteEin (
                            changed, params, origEinOp, origArgs, place,
                            newLHS, newEinOp, newArgs)
                      in
                        rewrite (changed, origEinOp', place', xs, origArgs')
                      end
                (* end case *))
          in
            rewrite (false, ein, 0, args, [])
          end
      | doRHS _ = NONE

    structure Rewrite = RewriteFn (
      struct
        structure IR = IR
        val doAssign = doRHS

        fun doMAssign _ = NONE
        val elimUnusedVars = UnusedElim.reduce
      end)

    structure Promote = PromoteFn (IR)

    val rewrite = Rewrite.transform
    val promote = Promote.transform

  end
