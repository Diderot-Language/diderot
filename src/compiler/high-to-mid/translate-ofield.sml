(* translate-ofield.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TranslateOField : sig

    val transform : MidIR.assign -> MidIR.assign list

  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet

    fun transform (y, IR.EINAPP(ein as E.EIN{body,...}, args)) = (case body
           of E.Probe(E.OField _, _) => PolyEin.transform(y, ein, args)
            | E.Sum(_, E.Probe(E.OField _,  _)) => PolyEin.transform(y, ein, args)
            | _ => [(y, IR.EINAPP(ein, args))]
         (* end case*))
      | transform (y, e) =  [(y, e)]

end
