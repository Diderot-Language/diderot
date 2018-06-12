(* clean-param.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(*
*cleanParam.sml cleans the parameters in an EIN expression.
*Cleaning parameters is simple.
*We keep track of all the paramids used in subexpression(getIdCount()),
*remap the param ids(mkMap)
*and choosing the mid-il args that are used, and then lastly rewrites the body.
*)
structure CleanParams : sig

    val clean : Ein.ein_exp * Ein.param_kind list * Ein.index_bind list * MidIR.var list -> MidIR.rhs
    val getFreeParams : Ein.ein_exp -> IntRedBlackSet.set

  end = struct

    structure E = Ein
    structure DstIR = MidIR
    structure DstV = DstIR.Var
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet

    (*dictionary to lookup mapp*)
    fun lookupSingleIndex (e1, mapp, str) = (case IMap.find(mapp, e1)
           of SOME l => l
            | _ => raise Fail(str ^ Int.toString e1)
          (* end case *))

  (* walk the ein expression and compute the set of free parameter indices (i.e., tensor,
   * image, and kernel variables) in the expression.
   *)
    fun getFreeParams b = let
          fun walk (b, mapp) = (case b
                 of E.Tensor(id, _) => ISet.add(mapp, id)
                  | E.Conv(v, _, h, _) => ISet.add(ISet.add(mapp, h), v)
                  | E.Probe(e1, e2) => walk (e2, walk (e1, mapp))
                  | E.OField(E.CFExp es, e2, dx) => let
                      val es = List.map (fn (id, _) => E.Tensor(id, [])) es
                      in
                        walk(dx, walk (e2, List.foldl walk mapp es))
                      end
                  | E.Value _ => raise Fail "unexpected Value"
                  | E.Img _ => raise Fail "unexpected Img"
                  | E.Krn _ => raise Fail "unexpected Krn"
                  | E.Sum(_, e1) => walk (e1, mapp)
                  | E.Op1(_, e1) => walk (e1, mapp)
                  | E.Op2(_, e1, e2) => walk (e2, walk (e1, mapp))
                  | E.Op3(_, e1, e2, e3) => walk(e3, walk(e2, walk(e1, mapp)))
                  | E.Opn(_, es) => List.foldl walk mapp es
                  | _ => mapp
                (* end case *))
          in
            walk (b, ISet.empty)
          end

    (* mkMapp:dict*params*var list ->dict*params*var list
    * countmapp dictionary keeps track of which ids have been used
    * mapp id the dictionary of the new ids
    *)
    fun mkMapp (freeParams, params, args) = let
          fun m (_, _, mapp, p, [], a, []) = (mapp, rev p, rev a)
            | m (i, j, mapp, p, p1::params, a, a1::arg) =
                if ISet.member(freeParams, i)
                  then let
                    val mapp2 = IMap.insert(mapp, i, j)
                    in
                      m (i+1, j+1, mapp2, p1::p, params, a1::a, arg)
                    end
                  else m (i+1, j, mapp, p, params, a, arg)
            | m (_, _, _, _, _, _, []) = raise Fail "too many parameters"
            | m (_, _, _, _, [], _, _) = raise Fail "too many args"
          in
            m (0, 0, IMap.empty, [], params, [], args)
          end

    (*rewriteParam:dict*ein_exp ->ein_exp
    *rewrite ids in exp using mapp
    *)
    fun rewriteParam (mapp, e) = let
          fun getId id =  lookupSingleIndex(id, mapp, "Mapp doesn't have Param Id ")
          fun rewrite b = (case b
                 of E.Tensor(id, alpha) => E.Tensor(getId id, alpha)
                  | E.Conv(v, alpha, h, dx) =>
                      E.Conv(getId v, alpha, getId h, dx)
                  | E.Probe(f, t) => E.Probe(rewrite f, rewrite t)
                  | E.OField(E.CFExp es, e2, dx) => E.OField(
                      E.CFExp(List.map (fn (id, inputTy) => (getId id, inputTy)) es),
                      rewrite e2,
                      rewrite dx)
                  | E.Sum(sx ,e1) => E.Sum(sx, rewrite e1)
                  | E.Op1(op1, e1) => E.Op1(op1, rewrite e1)
                  | E.Op2(op2, e1,e2) => E.Op2(op2, rewrite e1, rewrite e2)
                  | E.Op3(op3, e1, e2, e3) => E.Op3(op3, rewrite e1, rewrite e2, rewrite e3)
                  | E.Opn(opn, es) => E.Opn(opn, List.map rewrite es)
                  | _ => b
                (* end case *))
          in
            rewrite e
          end

    (* cleanParams:var*ein_exp*param*index* var list ->code
    *cleans params
    *)
    fun clean (body, params, index, args) = let
          val freeParams = getFreeParams body
          val (mapp, Nparams, Nargs) = mkMapp (freeParams, params, args)
          val Nbody = rewriteParam (mapp, body)
          in
            DstIR.EINAPP(Ein.EIN{params=Nparams, index=index, body=Nbody}, Nargs)
          end

  end (* CleanParam *)
