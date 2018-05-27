(* poly-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure PolyEin3 : sig

    val rewriteMerge : Ein.ein_exp -> Ein.ein_exp
    val rewriteDifferentiate :  Ein.ein_exp  -> Ein.ein_exp

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
	val iterP = H.iterP
	val iterA = H.iterA
	
	(*flatten product and merge poly terms into a single poly term
	*  x *x  => X^2
	*)
	fun rewriteMergeProd es  = 
		let			
			fun iter([], rest, ids, mapp) = (rest, ids, mapp)
				| iter(e1::es, rest, ids,  mapp) = (case e1
					of E.Poly(E.Tensor(id, ix), n, _) =>
						let
							val ((nS, n1, n2, n3, n4, n5, n6), ids) = (case IMap.find (mapp, id)
								of SOME e => (e, ids)
								| NONE => ((0, 0, 0, 0, 0, 0, 0), id::ids)
								(* end case*))
							fun SetMap ns = IMap.insert (mapp, id, ns)							
							val mapp = 
								if(ix=[]) then SetMap (nS+n, n1, n2, n3, n4, n5, n6)
								else if(ix=[E.C 0]) then SetMap (nS, n1+n, n2, n3, n4, n5, n6)
								else if(ix=[E.C 1]) then SetMap (nS, n1, n2+n, n3, n4, n5, n6)
								else if(ix=[E.C 2]) then SetMap (nS, n1, n2, n3+n, n4, n5, n6)
								else if(ix=[E.C 3]) then SetMap (nS, n1, n2, n3, n4+n, n5, n6)
								else if(ix=[E.C 4]) then SetMap (nS, n1, n2, n3, n4, n5+n, n6)
								else if(ix=[E.C 5]) then SetMap (nS, n1, n2, n3, n4, n5, n6+n)
								else  raise Fail("unhandled:"^EinPP.expToString(e1))
						in 
							iter(es, rest, ids, mapp)
						end
					| _ => iter(es, e1::rest, ids, mapp)
					(* end case *))			
			val (rest, ids, mapp) = iter(es , [], [], IMap.empty)
			(*make poly term for each id given component and power*)
			fun mkPolyAll([], ps) =  ps
			  | mkPolyAll(id::es, ps) =  
				(case IMap.find (mapp, id)
					of NONE => mkPolyAll(es, ps)
					| SOME (nS, n1, n2, n3, n4, n5, n6) =>
						let (*each term is T(id)_c^n*)
							fun mkPoly([]) = []
							   | mkPoly((_, 0)::es) = mkPoly es
							   | mkPoly((SOME c, n)::es) = E.Poly(E.Tensor(id, [E.C c]), n, []):: (mkPoly es)
							   | mkPoly((NONE, n)::es) = E.Poly(E.Tensor(id, []), n, []) :: (mkPoly es) 
							   (*Note: None is used for scalars*)							  
							val pnew =  mkPoly([(NONE, nS), (SOME 0, n1), (SOME 1, n2), (SOME 2, n3), (SOME 3, n4), (SOME 4, n5), (SOME 5, n6)])
						in mkPolyAll(es, pnew@ps) end	
				(* end case*))					
			val ps =  mkPolyAll(ids, []) 
			(*flatten *)
		in 
			iterP(ps@rest)	
		end	
    (* collect the poly expressions*)
    fun rewriteMerge body =  
    	(case body
			of E.Lift(e1)           => E.Lift(rewriteMerge e1)
			| E.Sum(op1, e1)        => E.Sum(op1, rewriteMerge e1)
			| E.Op1(op1, e1)        => E.Op1(op1, rewriteMerge e1)
			| E.Op2(op2, e1, e2)    => E.Op2(op2, rewriteMerge e1, rewriteMerge e2)
			| E.Opn(E.Prod, E.Opn(E.Prod, ps)::es) => rewriteMerge(iterP(ps@es))
			| E.Opn(E.Prod, E.Opn(E.Add, ps)::es)
				=> rewriteMerge (E.Opn(E.Add, List.map (fn  e1 => rewriteMerge(iterP(e1::es))) ps))
			| E.Opn(E.Add , ps) 		=> iterA(List.map rewriteMerge ps)
			| E.Opn(E.Prod, ps)         => rewriteMergeProd (List.map rewriteMerge ps)
		    | _            			    => body
		(* end case*))
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
  end
