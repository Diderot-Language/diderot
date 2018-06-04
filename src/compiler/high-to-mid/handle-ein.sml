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

    fun expand (lhs, ein, args) = let
        (* ************** distribute and push Summation*********** *)
          val ein' = EinSums.transform ein
        (* **************** split phase ************* *)
         fun iter([], ys) = ys
		  | iter(e1::es, ys) = 
		    let
			  val (lhs, DstIR.EINAPP(e, a)) = e1
			  val y1 = FloatEin.transform (lhs, e, a)
			  in
				iter(es, ys@y1)
			  end
		  val newbies = [(lhs, DstIR.EINAPP(ein', args))]
          val newbies = iter(newbies, [])
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
