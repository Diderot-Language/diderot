(* handle-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure HandleEin : sig

    val expand : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assignment list

  end = struct

    structure E = Ein
    structure SrcIR = HighIR
    structure DstIR = MidIR

    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt

    fun iter([], ys) = ys
      | iter((lhs, DstIR.EINAPP(e, a))::es, ys) = iter(es, ys@(FloatEin.transform (lhs, e, a)))
          
    fun expand (lhs, ein, args) = let
        val _ = (concat["\n\n\n*****\n expand ***\n\t:", EinPP.toString(ein),"\n\t"])
        (* ************** distribute and push Summation*********** *)
          val ein' = EinSums.transform ein
        (* **************** split phase ************* *)
          val newbie = (lhs, DstIR.EINAPP(ein', args))
          val newbies = iter([newbie], [])
          val newbies = iter(newbies, [])
        (* **************** translate of fields ************* *)
          val newbies = List.foldr (fn (e, acc) => TranslateOField.transform e @ acc) [] newbies
        (* ************** ProbeEIN *********** *)
          val avail = AvailRHS.new()
          val _ = List.app (ProbeEin.expand avail) (newbies);
          val stmts = List.rev (AvailRHS.getAssignments avail)
          in
            List.map DstIR.ASSGN stmts
          end

  end (* HandleEin *)
