(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin : sig

    val transform : MidIR.var * Ein.ein * MidIR.var list -> MidIR.assign list

  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet
    structure SrcIR = HighIR
    structure DstIR = MidIR
    structure H = Helper
    structure P2 = PolyEin2
    structure P3 = PolyEin3
 
    (********************************** main *******************************)
    (* main function 
    * translate probe of cfexp to  poly terms 
    *)
    fun transform_Core (y, ein as Ein.EIN{body, index, params} , args) = 
        let
            val E.Probe(E.OField(E.CFExp cfexp_ids, e, E.Partial dx), expProbe) = body 
            val probe_ids = List.map (fn E.Tensor(tid, _) => tid)  [expProbe]
            (*Note that Dev branch allows multi-probe which is why we use a list of ids*)
            (*check that the number of into parameters matches number of probed arguments*)
            val n_pargs = length(cfexp_ids)
            val n_probe = length(probe_ids)
            val _ = if(not(n_pargs = n_probe))
                    then raise  Fail(concat[" n_pargs:", Int.toString( n_pargs), "n_probe:", Int.toString(n_probe)])
                    else 1
            (* replace polywrap args/params with probed position(s) args/params *)        
            val (args, params, e) = P2.polyArgs(params, e, args, cfexp_ids, probe_ids)
            (* need to flatten before merging polynomials in product *)
            val e = P3.rewriteMerge(e)
           (* normalize ein by cleaning it up and differntiating*)
            val e = P3.rewriteDifferentiate(E.Apply(E.Partial dx, e))       
         in (args, params, e) end 
            
            
    (********************************** main *******************************)
    (* transform ein operator *)
    (* Note: Dev branch handles reduction and wrappers here *)
    fun transform (y, ein, args) = 
        let
            val Ein.EIN{body, index, params} = ein
            val (sx, body) = (case body
                    of  (E.Sum(sx, e)) => (sx, e)
                    | _ => ([], body)
                    (* end case*))
            val ein = Ein.EIN{body = body, index = index, params = params}
            val (args, params, body)  = transform_Core (y, ein, args) 
            (* Add summation wrapper back to ein expression *)
            val body = (case sx
                    of [] => body
                    |  _  => E.Sum(sx, body)
                (* end case *))
            val ein = Ein.EIN{body = body, index = index, params = params}
        in  
            [(y, IR.EINAPP(ein, args))]
        end


  end
