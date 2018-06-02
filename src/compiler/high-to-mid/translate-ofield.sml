(* translate-ofield.sml
 *
 * Translation for EIN Terms that represent other fields (E.OField)
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

    fun mkEin(body, index, params) = Ein.EIN{body = body, index = index, params = params}

    fun wrapCFExp(sx, y, ein as E.EIN{index,...}, args) = let
          val (args, params, body)  = TranslateCFExp.transform_CFExp (y, ein, args)
        (* Add summation wrapper back to ein expression *)
          val body = (case sx
                 of [] => body
                  | _ => E.Sum(sx, body)
                (* end case *))
          val ein = mkEin(body, index, params)
          in
            [(y, IR.EINAPP(ein, args))]
          end

    fun transform (y, IR.EINAPP(ein as E.EIN{body,index, params}, args)) = (case body
           of E.Probe(E.OField _, _) => wrapCFExp ([], y, ein, args)
            | E.Sum(sx, p as E.Probe(E.OField _,  _)) =>
                wrapCFExp (sx, y, mkEin(p, index, params), args)
            | _ => [(y, IR.EINAPP(ein, args))]
         (* end case *))
      | transform (y, e) =  [(y, e)]

  end
