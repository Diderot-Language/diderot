(* clean-index.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* Example 1
Input to clean ()
    e: Σ_14[0-2]Prod< T20_14*  T21_14,1>, index:[2,3], sx:[E.V(6)[0-2]]
Analyzing
    Get shape of e, getShapes()
        aShape : 14,14,14,1,tShape : 1
    Create sizeMapp: index_id to dimension, mkSizeMapp()
        0 => 2, 1 => 3, 6 => 3
    Find size of e by looking up tshape in the sizeMapp
        sizes=[3]
    Create indexMapp: Map the index variables e => e', mkIndexMap()
        Set map for tshape indices first, vxToMapp()
        E.V(1) => E.V(0)
        Checks indices from E.V 0 to E.V15, intToMapp()
        E.V(14) => E.V(1)
    Rewrite subexpression: e =>e', rewriteIx()
        e =>  Σ_1[0-2]Prod< T20_1*  T21_1,0>
Output: tshape:[E.V(1)],sizes:[3],e': Σ_1[0-2]Prod< T20_1*  T21_1,0>

        b=<Σ_[E.V(6)[0-2]]( Σ_14[0-2]Prod< T20_14*  T21_14,1>)... >2,3 (args)
        ===>
        a=< Σ_1[0-2]Prod< T20_1*  T21_1,0>>_3 (args)
        b'=<T_{E.V(1)}..>2,3 (args,a)

* Example 2
Input to clean ()
    e:Add( T23_6,1+ T24_6,1), index:[2,3], sx:[E.V(6)[0-2]]
Analyzing
    Get shape of e
        aShape : 6,1,6,1,tShape : 6,1
    Create sizeMapp: index_id to dimension
        0 => 2,1 => 3,6 => 3
    Find size of e by looking up tshape in the sizeMapp
        sizes=[3,3]
    Create indexMapp: Map the index variables e => e'
        Set map for tshape indices first
        E.V(6) => E.V(0),E.V(1) => E.V(1)
        Checks indices from E.V 0 to E.V7
    Rewrite subexpression: e =>e'
        e => Add( T23_0,1+ T24_0,1)
Output:
    tshape:[E.V(6),E.V(1)], sizes:[3,3], e':Add( T23_0,1+ T24_0,1)
*)

structure CleanIndex : sig

    val clean : Ein.ein_exp * int list * Ein.sumrange list -> Ein.mu list * int list * Ein.ein_exp

  end = struct

    structure E = Ein
    structure ISet = IntRedBlackSet
    structure IMap = IntRedBlackMap

    fun lookupId (e1, mapp) = (case IMap.find (mapp, e1)
           of SOME l => l
            | _ => raise Fail (concat["lookupId: ", Int.toString e1, " not found"])
          (* end case *))

    fun lkupVx (E.V e1, mapp) = E.V (lookupId(e1, mapp))
      | lkupVx (E.C e1, mapp) = E.C e1

    fun lkupSx ([], mapp) = []
      | lkupSx ((e1, ub, lb)::es, mapp) = (case IMap.find(mapp, e1)
           of SOME l => (l, ub, lb) :: lkupSx(es, mapp)
            | _ => lkupSx(es, mapp)
          (* end case *))

  (* compute the set of indices (both parameter and summation-index) that are used in an
   * ein expression.
   *)
    fun aShape b = let
          fun addMus (s, []) = s
            | addMus (s, E.V i :: mus) = addMus (ISet.add(s, i), mus)
            | addMus (s, E.C _ :: mus) = addMus (s, mus)
          fun addSingle(s, []) = s
            | addSingle(s, i :: ixs) = addSingle (ISet.add(s, i), ixs)
          fun shape (b, ixs) = (case b
                 of E.Const _ => ixs
                  | E.ConstR _ => ixs
                  | E.Tensor(_, alpha) => addMus(ixs, alpha)
                  | E.Zero(alpha) => addMus(ixs, alpha)
                  | E.Delta(i, j) => addMus(ixs, [i, j])
                  | E.Epsilon(i, j, k) => addMus (ixs, [i, j, k])
                  | E.Eps2(i, j) => addMus(ixs, [i, j])
                  | E.Field(_, alpha) => addMus(ixs, alpha)
                  | E.Lift e => shape(e, ixs)
                  | E.Conv(_, alpha, _, dx) => addMus(addMus(ixs, alpha), dx)
                  | E.Partial alpha => addMus(ixs, alpha)
                  | E.Apply(E.Partial alpha, e1) => shape (e1, addMus(ixs, alpha))
                  | E.Probe(e, _) => shape (e, ixs)
                  | E.Comp(e1, _) => shape(e1, ixs)
                  | E.OField(_, e2, E.Partial alpha) => shape (e2, addMus(ixs, alpha))
                  | E.Poly(_, _, alpha) => addMus (ixs, alpha)
                  | E.Value e1 => raise Fail "Error in Ashape"
                  | E.Img _ => raise Fail "Error in Ashape"
                  | E.Krn _ => raise Fail "Error in Ashape"
                  | E.Sum(sx, e) => shape (e, addSingle (ixs, List.map #1 sx))
                  | E.Op1 (_, e) => shape (e, ixs)
                  | E.Op2(_, e1, e2) => shape (e1, shape(e2, ixs))
                  | E.Op3(_, e1, e2, e3) => shape (e1, shape(e2, shape(e3, ixs)))
                  | E.Opn(_, es) => List.foldl shape ixs es
                  | _ => raise Fail "impossible"
                (* end case *))
          in
            shape (b, ISet.empty)
          end

    (* eShape: list of index-ids with potential to be in tshape
    *   of T_α -> eshape = α
    *   |  e1 +.. -> eshape = eShape(e1)
    *   |  e1 / e2 ->
    *       eshape = eShape(e1) and b = eShape(e2).
    *       forall i in b. if i not in eshape then add i to eshape
    *   |  e1 * e2 ->
    *       eshape = eShape(e1) and b = eShape(e2).
    *       forall i in b. if i not in eshape then add i to eshape
    *)
    fun eShape b = let
          fun shape (b, ixs) = (case b
                 of E.Const _ => ixs
                  | E.ConstR _ => ixs
                  | E.Tensor(_, alpha) => alpha @ ixs
                  | E.Zero(alpha) => alpha @ ixs
                  | E.Delta(i, j) => i :: j :: ixs
                  | E.Epsilon(i, j, k) => i :: j :: k :: ixs
                  | E.Eps2(i, j) => i :: j :: ixs
                  | E.Field(_, alpha) => alpha @ ixs
                  | E.Lift e => shape (e, ixs)
                  | E.Conv(_, alpha, _, dx) => alpha @ dx @ ixs
                  | E.Partial alpha => alpha @ ixs
                  | E.Apply(E.Partial dx, e) => shape (e, dx@ixs)
                  | E.Comp(e1, _) => shape(e1, ixs)
                  | E.Probe(e, _) => shape (e, ixs)
                  | E.OField(_, e2, E.Partial alpha) => shape(e2, alpha@ixs)
                  | E.Poly(_, _, alpha) => alpha@ ixs
                  | E.Value _ => raise Fail "unexpected Value"
                  | E.Img _ => raise Fail "unexpected Img"
                  | E.Krn _ => raise Fail "unexpected Krn"
                  | E.Sum(_ , e) => shape (e, ixs)
                  | E.Op1(_, e) => shape (e, ixs)
                  | E.Op2(_, e1, e2) => shape' ([e1, e2], ixs)
                  | E.Op3(_, e1, e2, e3) => shape' ([e1, e2, e3], ixs)
                  | E.Opn(E.Add, e::_) => shape(e, ixs)
                  | E.Opn(E.Prod, es) => shape' (es, ixs)
                  | _ => raise Fail "impossible"
                (* end case *))
        (* processing a list of subexpressions that is under a division or product operator.
         *  es  -- list of sub expressions
         *  ixs -- indices to the right of the parent operator
         *)
          and shape' (es, ixs) = let
                fun f ([], _, jxs) = List.revAppend(jxs, ixs)
                  | f (e::es, seen, jxs) = let
(* QUESTION: perhaps we don't need the set and could just use jxs instead *)
                      fun add ([], seen, jxs) = f (es, seen, jxs)
                        | add (E.V i::ixs, seen, jxs) = if ISet.member(seen, i)
                            then add (ixs, seen, jxs)
                            else add (ixs, ISet.add(seen, i), E.V i::jxs)
                        | add (E.C i::ixs, seen, jxs) =  add (ixs, seen, jxs)
                      in
                        add (shape (e, []), seen, jxs)
                      end
                in
                  f (es, ISet.empty, [])
                end
          in
            shape (b, [])
          end

    (* tShape: get shape of tensor replacement
    * :int list, sumrange list, ein expression -> mu list
    *)
    fun tShape (index, sx, e) = let
        (* outerAlpha = set of indices supported by original EIN *)
          val outerAlpha = let
                fun add ([], _, s) = ISet.addList(s, List.map (fn (v, _, _) => v) sx)
                  | add (_::r, i, s) = add (r, i+1, ISet.add(s, i))
                in
                  add (index, 0, ISet.empty)
                end
        (* getT: filters eShape to create tShape
        * getT(eshape, accumulator)
        * for every i in eshape if it is in outerAlpha then i::tshape
        *)
          fun getT ([], rest) = List.rev rest
            | getT ((E.C _)::es, rest) = getT(es, rest)
            | getT ((e as E.V v)::rest, es) =
                if ISet.member(outerAlpha, v)
                  then getT (rest, e::es)
                  else getT (rest, es)
          in
            getT (eShape e, [])
          end

  (* sizeMapp: creates a map for index_id to dimension*)
    fun mkSizeMapp (index, sx) = let
          fun idToMapp (mapp, [],_ ) = mapp
            | idToMapp (mapp, ix::es, cnt) = idToMapp (IMap.insert (mapp, cnt, ix), es,cnt+1)
          fun sxToMapp (mapp, []) = mapp
            | sxToMapp (mapp, (v, _, ub)::es) = sxToMapp (IMap.insert (mapp, v, ub+1), es)
          in
            sxToMapp (idToMapp (IMap.empty, index, 0), sx)
          end

  (* mkIndexMapp: maps the index variables in subexpression*)
    fun mkIndexMapp (index, sx, ashape, tshape) =let
        (* adds index e1 to the mapp E.V e1=> E.V cnt *)
          fun vxToMapp (mapp, [], cnt) = (mapp, cnt)
            | vxToMapp (mapp, (E.V e1)::es, cnt) = vxToMapp (IMap.insert (mapp, e1, cnt), es, cnt+1)
        (* Creates an map for indices in tshape first. *)
          val (mapp, tocounter) = vxToMapp (IMap.empty, tshape, 0)
        (* finds max element in ashape and creates list [0, 1, 2, ...., max] *)
          val maxmu = if ISet.isEmpty ashape then ~1 else ISet.maxItem ashape
        (* iff index e1 is in ashape add e1 the mapp E.V e1=> E.V cnt *)
          fun intToMapp (mapp, i, cnt) = if (i > maxmu)
                  then mapp
                else if IMap.inDomain(mapp, i)
                  then intToMapp (mapp, i+1, cnt)
                else if ISet.member(ashape, i)
                  then intToMapp (IMap.insert (mapp, i, cnt), i+1, cnt+1)
                  else intToMapp (mapp, i+1, cnt)
        (* creates a map for the rest of the indices that may be used in the ein expression *)
          in
            intToMapp (mapp, 0, tocounter)
          end

    (* rewriteIndices: rewrites indices in e using mapp *)
    fun rewriteIx (mapp, e) = let
          fun getAlpha alpha = List.map (fn e=> lkupVx (e, mapp)) alpha
          fun getIx ix = lookupId (ix, mapp)
          fun getVx ix = lkupVx (ix, mapp)
          fun getSx sx = lkupSx (sx, mapp)
          fun rewrite b = (case b
                 of E.Const _ => b
                  | E.ConstR _ => b
                  | E.Tensor(id, alpha) => E.Tensor(id, getAlpha alpha)
                  | E.Zero(alpha) => E.Zero(getAlpha alpha)
                  | E.Delta(i, j) => E.Delta(getVx i, getVx j)
                  | E.Epsilon(i, j, k) => E.Epsilon(getVx  i, getVx  j, getVx k)
                  | E.Eps2(i, j) => E.Eps2(getVx i, getVx j)
                  | E.Field(id, alpha) => E.Field(id, getAlpha alpha)
                  | E.Lift e1 => E.Lift(rewrite e1)
                  | E.Conv(v, alpha, h, dx) => E.Conv (v, getAlpha alpha, h, getAlpha dx)
                  | E.Partial dx => E.Partial (getAlpha dx)
                  | E.Apply (e1, e2) => E.Apply(rewrite e1, rewrite e2)
                  | E.Probe(E.Conv(v, alpha, h,dx), t) =>
                      E.Probe(E.Conv(v, getAlpha alpha, h, getAlpha dx), rewrite t)
                  | E.Probe (e1, e2) => E.Probe(rewrite e1, rewrite e2)
                  | E.Comp(e1, es) => E.Comp(rewrite e1, es)
                  | E.OField (opn, e1, E.Partial dx) =>
                      E.OField(opn, rewrite e1, E.Partial(getAlpha dx))
                  | E.Poly(e1, n, dx) => E.Poly(rewrite e1, n, getAlpha dx)
                  | E.Value e1 => raise Fail "unexpected Value"
                  | E.Img _ => raise Fail "unexpected Img"
                  | E.Krn _ => raise Fail "unexpected Krn"
                  | E.Sum(sx, e1) => E.Sum(getSx sx, rewrite e1)
                  | E.Op1(op1, e1) => E.Op1(op1, rewrite e1)
                  | E.Op2(op2, e1, e2) => E.Op2(op2, rewrite e1, rewrite e2)
                  | E.Op3(op3, e1, e2, e3) => E.Op3(op3, rewrite e1, rewrite e2, rewrite e3)
                  | E.Opn(opn, es) => E.Opn(opn, List.map rewrite es)
              (* end case *))
          in
            rewrite e
          end

    fun shapeToString (pre, es) = String.concat[
            pre, "-", String.concatWithMap "," (fn E.V(e) => "E.V:"^Int.toString e) es
          ]
    fun nToString (pre, es) = String.concat[
            pre, "-", String.concatWithMap "," (fn e => ":"^Int.toString e) es
          ]

    (*clean ()  cleans the indices in an EIN expression*)
    (*input-  e:ein expression
     index: int list for original EIN operator
     sx:sumrange list for outer summation expression, if any exist
     output- tshape:indices for tensor replacment,
     sizes: Tensor type of new EIN operator,
     e': rewritten e
     Generic Example
     x = λT < Σ_sx (e...)  ...)  >_{index} (arg0)
     ===>
     arg1 = λT <e'>_{sizes} (arg0),
     x =λ T T' < Σ_sx (T1_{tshape}...) ...) >_{index} (arg0, arg1)
     *)
    fun clean (e, index, sx) = let
         (* Get shape of e
          * ashape ISet.set  : all the indices mentioned in body
          * tshape (mu list) : shape of tensor replacement
          *)
(* DEBUG val _ = print (String.concat["\n\n clean: ",EinPP.expToString(e)])*)
           val ashape = aShape e
           (*val _ = print(shapeToString ("ashape",ashape))*)
           val tshape = tShape(index, sx, e)
(* DEBUG val _ = print(shapeToString (" tshape",  tshape))*)
         (* Create sizeMapp: index_id to dimension index_id is bound to*)
           val sizeMapp = mkSizeMapp (index, sx)
         (* Find size of e by looking up tshape in the sizeMapp.
          * sizes (int list) : TensorType of tensor replacement
          *)
           val sizes = List.map (fn E.V e1 => lookupId (e1, sizeMapp))  tshape
(* DEBUG val _ =   print(nToString("sizes ",sizes ))*)
         (* Create indexMapp: Mapps the index variables e  => e'*)
           val indexMapp = mkIndexMapp (index, sx, ashape, tshape)
         (* Rewrite subexpression: e  =>e' *)
           val e' = rewriteIx (indexMapp, e)
(* DEBUG val _ = print (String.concat["\n===> ",EinPP.expToString(e')])*)
           in
             (tshape, sizes, e')
           end

  end (* CleanIndex *)
