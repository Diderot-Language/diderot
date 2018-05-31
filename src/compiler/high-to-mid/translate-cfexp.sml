(* translate-cfexp.sml
 *
 * Translation for EIN Term that represents closed form expressions
 * 
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)
structure TranslateCFExp : sig

    val transform_CFExp : MidIR.var * Ein.ein * MidIR.var list -> MidIR.var list * Ein.param_kind list * Ein.ein_exp
    
  end = struct

    structure IR = MidIR
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure IMap = IntRedBlackMap
    structure ISet = IntRedBlackSet
    structure SrcIR = HighIR
    structure DstIR = MidIR
    
    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s
    fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
      | paramToString (i, E.FLD d) = concat["F", i2s i, "(", i2s d, ")"]
      | paramToString (i, E.KRN) = "H" ^ i2s i
      | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]  
    
    (* The terms with a param_id in the mapp are replaced
    * body - ein expression
    * args - variable arguments
    * dim - dimension 
    * SeqId_current - current sequential option
    * mapp - map for replacements 
    *)
    fun replace (body, dim, mapp) = let
        (*rewriteTensor
        * This a is a tensor that is treated like a field
        * Replace tensor term with a new term (new id and space components)
        * V => [V_0 , V_1, V_2]
        * and with deltas to turn each component on and off
        * V => V_0*Delta_0i + V_1*Delta_1i V_2+Delta_2i
        *)
        fun rewriteTensor (E.Tensor(tid, alpha)) = 
            let 
                fun mkBase (alpha') = E.Tensor(tid, alpha')
                fun mkPoly (alpha') = E.Poly(mkBase alpha', 1, [])
            in  
                (case alpha
                    of [] => mkPoly []
                    | [vx] =>
                        let
                            fun mkComponent cx = E.Opn(E.Prod, [mkPoly [E.C cx], E.Delta(E.C cx,  vx)]) 
                            val polyTerms =  List.tabulate(dim, (fn n => mkComponent n))
                        in  E.Opn(E.Add, polyTerms) end
                    | _ => raise Fail "unhandled size"
                (*end case*))
            end
        (*search body for tensor terms that are meant to be replaced*)
        fun rewrite body = 
            (case body
                of E.Tensor (id, _)      => if (ISet.member(mapp, id)) then rewriteTensor body else body
                | E.Lift(e1)            => E.Lift(rewrite e1)
                | E.Sum(op1, e1)        => E.Sum(op1, rewrite e1)
                | E.Op1(E.PowInt n, e1) => 
                    let
                        val tmp = rewrite e1
                        in EinUtil.iterPP (List.tabulate(n, fn _ => tmp))
                    end
                | E.Op1(op1, e1)                       => E.Op1(op1, rewrite e1)
                | E.Op2(op2, e1, e2)                   => E.Op2(op2, rewrite e1, rewrite e2) 
                | E.Opn(E.Prod, E.Opn(E.Add, ps)::es)  =>
                    let
                        val ps = List.map (fn  e1 =>  EinUtil.iterPP(e1::es)) ps
                        val body = E.Opn(E.Add, ps)
                    in  rewrite body end
                | E.Opn(E.Prod, ps)             => EinUtil.iterPP(List.map rewrite  ps)
                | E.Opn(E.Add , ps)             => EinUtil.iterAA(List.map rewrite  ps)  
                | _    => body
            (* end case*))
        in rewrite body end
        
    (* Replace the arguments identified in cfexp-ids with the arguments in probe-ids
    * params - EIN params
    * e - EIN body  
    * args - vars
    * SeqId - optional sequence index variable
    * cfexp_ids - closed-form expression has ids
    * probe_ids - field is probed at position with ids
    *  PROBE(CFEXP (cfexp_ids), probe_ids)
    *)
    fun polyArgs(params, e, args, cfexp_ids, probe_ids) = 
        let
            (*rewrites a single variable 
            * rewritement instances of arg at pid position with arg at idx position 
            *)
            fun single_TF (pid, args, params, idx, e)  = 
                let
                    (*check if the current parameter is a sequence and get dimension*)
                    (*Note Dev branch supports sequence parameter*)
                    val dim = (case List.nth(params, idx)
                            of E.TEN (_, []) => 1
                            | E.TEN (_, [i]) => i
                            | p => raise Fail("unsupported argument type:"^paramToString(idx, p))
                        (* end case *))                     
                    (*variable arg, and param*)
                    val arg_new = List.nth(args, idx)
                    val param_new = List.nth(params, idx)
                    val arg_rewrited = List.nth(args, pid)
                    (*id keeps track of placement and puts it in mapp*)
                    fun findArg(_, es, newargs, [], newparams, mapp) = ((List.rev newargs)@es, List.rev newparams, mapp)
                    | findArg(id, e1::es, newargs, p1::ps, newparams, mapp) = 
                        if(IR.Var.same(e1, arg_rewrited))
                        then findArg(id+1, es, arg_new::newargs, ps, param_new::newparams, ISet.add(mapp, id))
                        else findArg(id+1, es, e1::newargs, ps , p1::newparams, mapp)
                    val (args, params, mapp) = findArg(0, args, [], params, [], ISet.empty)
                    (* get dimension of vector that is being broken into components*)
                    val param_pos = List.nth(params, pid)
                    (* rewrite position tensor with deltas in body *)
                    val e = replace (e, dim, mapp)
                in (args, params, e) end                                
            (*EinUtil.iterAAte over all the input tensor variable expressions *)
            fun iter([], args, params, _, e) = (args, params, e)
              | iter((pid, E.T)::es, args, params, idx::idxs, e) = let
                (*variable is treated as a tensor so a simple variable swap is sufficient *)
                val args = List.take(args, pid)@[List.nth(args, idx)]@List.drop(args, pid+1) 
                in iter(es, args, params, idxs, e) end
              | iter((pid, E.F)::es, args, params, idx::idxs, e) = 
                (*variable is treated as a field so it needs to be expanded into its components*)
                let
                    val (args, params, e) = single_TF (pid, args, params, idx, e)
                in
                    iter(es, args, params, idxs, e)
                end
            (*probe_id: start of position variables for probe operation *)
            val (args, params, e) = iter(cfexp_ids, args, params, probe_ids, e)
        in (args, params, e) end
        
    (*apply differentiation*)
    fun rewriteDifferentiate body = (case body
        of E.Apply (E.Partial [], e)       => e  
        | E.Apply(E.Partial (d1::dx), e)   =>
            let 
                (* differentiate *)
                val e = DerivativeEin.differentiate ([d1], e)
            in rewriteDifferentiate (E.Apply(E.Partial dx, e)) end      
        | E.Op1(op1, e1)                => E.Op1(op1, rewriteDifferentiate e1)
        | E.Op2(op2, e1, e2)             => E.Op2(op2, rewriteDifferentiate e1, rewriteDifferentiate e2)
        | E.Opn(opn, es)                => E.Opn(opn, List.map rewriteDifferentiate es)
        | _                             => body
        (* end case*))
        
    (* main function 
    * translate probe of cfexp to  poly terms 
    *)
    fun transform_CFExp  (y, ein as Ein.EIN{body, index, params} , args) = 
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
            val (args, params, e) = polyArgs(params, e, args, cfexp_ids, probe_ids)
            (*merging polynomials in product *)
            (*val e = P3.rewriteMerge(e)*)
           (* normalize ein by cleaning it up and differntiating*)
            val e = rewriteDifferentiate(E.Apply(E.Partial dx, e))       
         in (args, params, e) end

  end
