(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin2 : sig

    val polyArgs :  Ein.param_kind list * Ein.ein_exp * MidIR.var list 
          * (int * Ein.inputTy) list * int list
          -> MidIR.var list * Ein.param_kind list * Ein.ein_exp

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
	val paramToString = H.paramToString
	val iterP = H.iterP
	val iterA = H.iterA
	

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
                of E.Tensor (id, _)		 => if (ISet.member(mapp, id)) then rewriteTensor body else body
                | E.Lift(e1)            => E.Lift(rewrite e1)
                | E.Sum(op1, e1)        => E.Sum(op1, rewrite e1)
                | E.Op1(E.PowInt n, e1) => 
                	let
                    	val tmp = rewrite e1
                    	in iterP (List.tabulate(n, fn _ => tmp))
                    end
                | E.Op1(op1, e1)                       => E.Op1(op1, rewrite e1)
                | E.Op2(op2, e1, e2)                   => E.Op2(op2, rewrite e1, rewrite e2) 
                | E.Opn(E.Prod, E.Opn(E.Add, ps)::es)  =>
                    let
                    	val ps = List.map (fn  e1 =>  iterP(e1::es)) ps
                    	val body = E.Opn(E.Add, ps)
                    in  rewrite body end
                | E.Opn(E.Prod, ps)             => iterP(List.map rewrite  ps)
                | E.Opn(E.Add , ps)             => iterA(List.map rewrite  ps)  
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
			(*does rewritement of a single variable 
			* rewritement instances of arg at pid position with arg at idx position 
			*)
			fun single_TF (pid, args, params, idx, e)  = 
				let
					(*check if the current parameter is a sequence and get dimension*)
					(*Note Dev branch supports sequence parameter*)
					val dim = (case List.nth(params, idx)
							of E.TEN (_, []) => 1
							| E.TEN (_, [i]) => i
							| p => raise Fail("unsupported argument type:"^H.paramToString(idx, p))
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
			(*iterate over all the input tensor variable expressions *)
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
		

  end
