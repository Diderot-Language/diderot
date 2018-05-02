(* dead-code.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure DeadCode : sig

  (* Eliminate unreachable code; i.e., code that follows return, continue, die,
   * or stabilize statements.
   *)
    val eliminate : Simple.block -> Simple.block

  end = struct

    structure S = Simple

    datatype 'a ctl_flow_info
      = EXIT                    (* stm sequence always exits; no pruning so far *)
      | CONT                    (* stm sequence falls through; no pruning *)
      | PRUNE of 'a             (* stm sequence always exits at last stm in its argument,
                                 * which is either a block or stm list *)
      | EDIT of 'a              (* pruned code that has non-exiting paths *)

  (* check to see if a loop-body block has a reachable "continue" statement.  This
   * function is only called when the body has "EXIT" or "PRUNE" info.
   *)
(* NOTE: we don't support "continue" in loops yet, so it is not clear what this does. *)
    fun hasContinue (S.Block{props, code}) = let
          fun chk [] = false
            | chk (S.S_IfThenElse(_, b1, b2)::stms) =
                hasContinue b1 orelse hasContinue b2 orelse chk stms
            | chk (S.S_Continue::_) = true
            | chk (S.S_Die::_) = false
            | chk (S.S_Stabilize::_) = false
            | chk (S.S_Return _ :: _) = false
            | chk (S.S_MapReduce _ :: _) = raise Fail "impossible: MapReduce inside loop"
            | chk (_::stms) = chk stms
          in
            chk code
          end

    fun eliminate blk = let
          fun isExit S.S_Continue = true
            | isExit S.S_Die = true
            | isExit S.S_Stabilize = true
            | isExit (S.S_Return _) = true
            | isExit _ = false
          fun pruneStms [] = CONT
            | pruneStms [S.S_IfThenElse(x, blk1, blk2)] = (
                case pruneIf(x, blk1, blk2)
                 of EXIT => EXIT
                  | CONT => CONT
                  | PRUNE stm => PRUNE[stm]
                  | EDIT stm => EDIT[stm]
                (* end case *))
            | pruneStms [S.S_Foreach(x, xs, blk)] = (
                case pruneForeach(x, xs, blk)
                 of EXIT => EXIT
                  | CONT => CONT
                  | PRUNE stm => PRUNE[stm]
                  | EDIT stm => EDIT[stm]
                (* end case *))
            | pruneStms [stm] = if isExit stm then EXIT else CONT
            | pruneStms ((stm as S.S_IfThenElse(x, blk1, blk2))::stms) = (
                case pruneIf(x, blk1, blk2)
                 of EXIT => PRUNE[stm]
                  | CONT => fallThrough (stm, stms)
                  | PRUNE stm => PRUNE[stm]
                  | EDIT stm => edit (stm, stms)
                (* end case *))
            | pruneStms ((stm as S.S_Foreach(x, xs, blk))::stms) = (
                case pruneForeach(x, xs, blk)
                 of EXIT => PRUNE[stm]
                  | CONT => fallThrough (stm, stms)
                  | PRUNE stm => PRUNE[stm]
                  | EDIT stm => edit (stm, stms)
                (* end case *))
            | pruneStms (stm::stms) = if isExit stm
                then PRUNE[stm]
                else (case pruneStms stms
                   of PRUNE stms => PRUNE(stm::stms)
                    | EDIT stms => EDIT(stm::stms)
                    | info => info
                  (* end case *))
          and fallThrough (stm, stms) = (case pruneStms stms
                 of EXIT => EXIT
                  | CONT => CONT
                  | PRUNE stms => PRUNE(stm::stms)
                  | EDIT stms => EDIT(stm::stms)
                (* end case *))
          and edit (stm, stms) = (case pruneStms stms
                 of PRUNE stms => PRUNE(stm::stms)
                  | EDIT stms => EDIT(stm::stms)
                  | _ => EDIT(stm::stms)
                (* end case *))
          and pruneIf (x, blk1, blk2) = (case (pruneBlk blk1, pruneBlk blk2)
                 of (EXIT,       EXIT      ) => EXIT
                  | (CONT,       CONT      ) => CONT
                  | (CONT,       EXIT      ) => CONT
                  | (EXIT,       CONT      ) => CONT
                  | (CONT,       EDIT blk2 ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (EDIT blk1,  CONT      ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (CONT,       PRUNE blk2) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (PRUNE blk1, CONT      ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (EXIT,       EDIT blk2 ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (EDIT blk1,  EXIT      ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (EDIT blk1,  EDIT blk2 ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (EDIT blk1,  PRUNE blk2) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (PRUNE blk1, EDIT blk2 ) => EDIT(S.S_IfThenElse(x, blk1, blk2))
                  | (EXIT,       PRUNE blk2) => PRUNE(S.S_IfThenElse(x, blk1, blk2))
                  | (PRUNE blk1, EXIT      ) => PRUNE(S.S_IfThenElse(x, blk1, blk2))
                  | (PRUNE blk1, PRUNE blk2) => PRUNE(S.S_IfThenElse(x, blk1, blk2))
                (* end case *))
          and pruneForeach (x, xs, blk) = (case pruneBlk blk
                 of EXIT => if hasContinue blk then CONT else EXIT
                  | CONT => CONT
                  | PRUNE blk => if hasContinue blk
                      then EDIT(S.S_Foreach(x, xs, blk))
                      else PRUNE(S.S_Foreach(x, xs, blk))
                  | EDIT blk => EDIT(S.S_Foreach(x, xs, blk))
                (* end case *))
          and pruneBlk (S.Block{props, code}) = (case pruneStms code
                 of EXIT => EXIT
                  | CONT => CONT
                  | PRUNE stms => PRUNE(S.Block{props=props, code=stms})
                  | EDIT stms => EDIT(S.Block{props=props, code=stms})
                (* end case *))
          in
            case pruneBlk blk
             of PRUNE blk => blk
              | EDIT blk => blk
              | _ => blk
            (* end case *)
          end

  end
