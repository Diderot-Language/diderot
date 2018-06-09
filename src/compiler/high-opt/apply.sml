(* apply.sml
 *
 * Apply EIN operator arguments to EIN operator.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Apply : sig

    val apply : Ein.ein * int * Ein.ein  * HighIR.var list * HighIR.var list -> Ein.ein option

  end = struct

    structure E = Ein

    structure IMap = IntRedBlackMap

    fun mapId (i, dict, shift) = (case IMap.find(dict, i)
          of NONE => i + shift
           | SOME j => j
          (* end case *))

    fun mapIndex (ix, dict, shift) = (case IMap.find(dict, ix)
           of NONE => E.V(ix + shift)
            | SOME j => j
        (* end case *))

    fun mapId2 (i, dict, shift) = (case IMap.find(dict, i)
           of NONE => (
                print(concat["Error: ", Int.toString i, " is out of range\n"]);
                i+shift)
            | SOME j => j
         (* end case *))

    fun rewriteSubst (e, subId, mx, paramShift, sumShift, newArgs, done) = let
          fun insertIndex ([], _, dict, shift) = (dict, shift)
            | insertIndex (e::es, n, dict, _) = let
                val shift = (case e of E.V ix => ix - n | E.C i => i - n)
                in
                  insertIndex(es, n+1, IMap.insert(dict, n, e), shift)
                end
          val (subMu, shift) = insertIndex(mx, 0, IMap.empty, 0)
          val shift' = Int.max(sumShift, shift)
          val insideComp = ref(false)
          fun mapMu (E.V i) = if (!insideComp)
                then E.V i
                else mapIndex(i, subMu, shift')
            | mapMu c = c
          fun mapAlpha mx = List.map mapMu mx
          fun mapSingle i = let
                val E.V v = mapIndex(i, subMu, shift')
                in
                  v
                end
          fun mapSum l = List.map (fn (a, b, c) => (mapSingle a, b, c)) l
          fun mapParam id = let
                val vA = List.nth(newArgs, id)
                fun iter ([], _) = mapId2(id, subId, 0)
                  | iter (e1::es, n) = if (HighIR.Var.same(e1, vA)) then n else iter(es, n+1)
                in
                  iter (done@newArgs, 0)
                end
          fun apply e = (case e
                 of E.Const _ => e
                  | E.ConstR _ => e
                  | E.Tensor(id, mx) => E.Tensor(mapParam id, mapAlpha mx)
                  | E.Zero(mx) => E.Zero(mapAlpha mx)
                  | E.Delta(i, j) => E.Delta(mapMu i, mapMu j)
                  | E.Epsilon(i, j, k) => E.Epsilon(mapMu i, mapMu j, mapMu k)
                  | E.Eps2(i, j) => E.Eps2(mapMu i,mapMu j)
                  | E.Field(id, mx) => E.Field(mapParam id, mapAlpha mx)
                  | E.Lift e1 => E.Lift(apply e1)
                  | E.Conv (v, mx, h, ux) => E.Conv(mapParam v, mapAlpha mx, mapParam h, mapAlpha ux)
                  | E.Partial mx => E.Partial (mapAlpha mx)
                  | E.Apply(e1, e2) => E.Apply(apply e1, apply e2)
                  | E.Probe(f, pos) => E.Probe(apply f, apply pos)
                  | E.Comp(e1, es) => let
                      val e1' = apply e1
                      val es' = List.map (fn(e2, n2)=> (insideComp:=true; (apply e2, n2))) es
                      in
                        (insideComp:=false; E.Comp(e1', es'))
                      end
                  | E.Value _ => raise Fail "expression before expand"
                  | E.Img _ => raise Fail "expression before expand"
                  | E.Krn _ => raise Fail "expression before expand"
                  | E.OField(E.CFExp es, e2,dx) => let
                      val es = List.map (fn (id, inputTy) => (mapParam id, inputTy)) es
                      val e2 = apply e2
                      val dx = apply dx
                      in
                        E.OField(E.CFExp es, e2,dx)
                      end
                  | E.Sum(c, esum) => E.Sum(mapSum c, apply esum)
                  | E.Op1(op1, e1) => E.Op1(op1, apply e1)
                  | E.Op2(op2, e1, e2) => E.Op2(op2, apply e1, apply e2)
                  | E.Op3(op3, e1, e2, e3) => E.Op3(op3, apply e1, apply e2, apply e3)
                  | E.Opn(opn, e1) => E.Opn(opn, List.map apply e1)
                (* end case *))
          in
            apply e
          end

 (* params subst *)
    fun rewriteParams (params, params2, place) = let
          val beg = List.take(params, place)
          val next = List.drop(params, place+1)
          val params' = beg@params2@next
          val n= length params
          val n2 = length params2
          val nbeg = length beg
          val nnext = length next
          fun createDict (0, shift1, shift2, dict) = dict
            | createDict (n, shift1, shift2, dict) =
                createDict (n-1, shift1, shift2, IMap.insert (dict, n+shift1, n+shift2))
          val origId = createDict (nnext, place, place+n2-1, IMap.empty)
          val subId = createDict (n2, ~1, place-1, IMap.empty)
          in
            (params', origId, subId, nbeg)
          end

  (* Looks for params id that match substitution *)
    fun apply (e1 as E.EIN{params, index, body}, place, e2, newArgs, done) = let
          val E.EIN{params=params2, index=index2, body=body2} = e2
          val changed = ref false
          val (params', origId, substId, paramShift) = rewriteParams(params, params2, place)
          val sumIndex = ref(length index)
          val insideComp = ref(false)
          fun rewrite (id, mx, e, shape) = let
                val comp = !insideComp
                val x = if(comp) then (length index) else !sumIndex
                in
                  if (id = place)
                    then if (length mx = length shape)
                      then (
                        changed := true;
                        rewriteSubst (body2, substId, mx, paramShift, x, newArgs, done))
                      else raise Fail "argument/parameter mismatch"
                    else (case e
                       of E.Tensor(id, mx) => E.Tensor(mapId(id, origId, 0), mx)
                        | E.Field(id, mx) => E.Field(mapId(id, origId, 0), mx)
                        |  _ => raise Fail "term to be replaced is not a Tensor or Fields"
                      (* end case *))
                end
          fun sumI e = let val (v,_,_) = List.last e in v end
          fun apply (b, shape) = (case b
                 of E.Tensor(id, mx) => rewrite (id, mx, b, shape)
                  | E.Field(id, mx) => rewrite (id, mx, b, shape)
                  | E.Zero(mx) => b
                  | E.Lift e1 => E.Lift(apply (e1, shape))
                  | E.Conv(v, mx, h, ux) => E.Conv(mapId(v, origId, 0), mx, mapId(h, origId, 0), ux)
                  | E.Apply(e1, e2) => E.Apply(apply (e1, shape), apply (e2, shape))
                  | E.Probe(f, pos) => E.Probe(apply (f, shape), apply (pos, shape))
                  | E.Value _ => raise Fail "expression before expand"
                  | E.Img _ => raise Fail "expression before expand"
                  | E.Krn _ => raise Fail "expression before expand"
                  | E.Comp(e1, es) => let
                      val fouter = apply (e1, shape)
                      val es' = List.map (fn (e2, n2) => (insideComp:=true; (apply (e2, n2), n2))) es
                      in
                        (insideComp:= true; E.Comp(fouter, es'))
                      end
                  | E.OField(E.CFExp es, e2, E.Partial alpha) => let
                      val ps = List.map (fn (id, inputTy) => (mapId(id, origId, 0), inputTy)) es
                      in
                        E.OField(E.CFExp ps, apply (e2, shape), E.Partial alpha)
                      end
                  | E.Poly _ => raise Fail "expression before expand"
                  | E.Sum(indices, esum) => let
                      val (ix, _, _) = List.last indices
                      in
                        sumIndex := ix;
                        E.Sum(indices, apply (esum, shape))
                      end
                  | E.Op1(op1, e1) => E.Op1(op1, apply (e1, shape))
                  | E.Op2(op2, e1, e2) => E.Op2(op2, apply (e1, shape), apply (e2, shape))
                  | E.Op3(op3, e1, e2, e3) => E.Op3(op3, apply (e1, shape), apply (e2, shape), apply (e3, shape))
                  | E.Opn(opn, es) => E.Opn(opn, List.map (fn e1=> apply(e1, shape)) es)
                  | _ => b
                (* end case *))
          val body'' = apply (body, index2)
          in
            if (! changed)
              then SOME(E.EIN{params=params', index=index, body=body''})
              else NONE
          end

    end
