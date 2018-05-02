(* ein-to-low.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(*
* genfn-Does preliminary scan of the body of EIN.EIN for vectorization potential
* If there is a field then passes to FieldToLow
* If there is a tensor then passes to handle*() functions to check if indices match
* i.e. <A_ij+B_ij>_ij vs.<A_ji+B_ij>_ij
*
*     (1) If indices match then passes to Iter->ToVec functions.
*            Creates LowIR vector operators.
*     (2) Iter->ScaToLow
*           Creates Low-IL scalar operators
* Note. The Iter function creates LowIR.CONS and therefore binds the indices in the EIN.body
*)

structure EinToLow : sig

    val expand : LowIR.var * Ein.ein * LowIR.var list -> LowIR.assignment list

  end = struct

    structure Var = LowIR.Var
    structure E = Ein
    structure Op = LowOps
    structure Mk = MkLowIR
    structure ToVec = EinToVector
    structure IMap = IntRedBlackMap

  (* `dropIndex alpha` returns the (len, i, alpha') where
   * len = length(alpha') and alpha = alpha'@[i].
   *)
    fun dropIndex alpha = let
          fun drop ([], _, _) = raise Fail "dropIndex[]"
            | drop ([idx], n, idxs') = (n, idx, List.rev idxs')
            | drop (idx::idxs, n, idx') = drop (idxs, n+1, idx::idx')
          in
            drop (alpha, 0, [])
          end

    (*matchLast:E.alpha*int -> (E.alpha) Option
    * Is the last index of alpha E.V n.
    * If so, return the rest of the list
    *)
    fun matchLast (alpha, n) = (case List.rev alpha
           of (E.V v)::es => if (n = v) then SOME(List.rev es) else NONE
            | _ => NONE
          (* end case *))

    (*matchFindLast:E.alpha *int -> E.alpha option* E.mu option
    * Is the last index of alpha = n.
    * is n anywhere else?
    *)
    fun matchFindLast (alpha, n) = let
          fun find es = List.find (fn (E.V idx') => (n = idx') | _ => false) es
          in
            case List.rev alpha
             of (E.V v)::es => if (n = v)
                  then (SOME(List.rev es), find es)
                  else (NONE, find es)
              | _::es => (NONE, find es)
              | [] => (NONE, NONE)
            (* end case *)
          end

  (* unroll the body of an Ein expression.  The arguments are
   *   shape  -- the shape of the tensor computed by the expression
   *   index  -- the shape of the iteration structure
   *   bodyFn -- the function for generating the body
   *)
    fun unroll (shape, index, bodyFn) = let
          val avail = AvailRHS.new()
          fun bodyFn' (mapp, n, m) = bodyFn (avail, IMap.insert(mapp, n, m))
          fun iter (mapp, xs, ys, shape, n, zs) = (case (xs, ys)
                 of ([], []) => bodyFn (avail, mapp)
                  | ([x], []) =>
                      Mk.cons (avail, shape, List.rev (bodyFn' (mapp, n, x) :: zs))
                  | (x::xr, []) =>
                      iter (mapp, xr, [], shape, n, bodyFn' (mapp, n, x) :: zs)
                  | ([], [y]) =>
                      iter (mapp, List.tabulate (y, Fn.id), [], shape, n, zs)
                  | ([], y::yr) => let
                      val _ :: shape' = shape
                      val n' = n + 1
                      fun lp (i, ws) = if (i < y)
                            then let
                              val w = iter (IMap.insert (mapp, n, i), [], yr, shape', n+1, [])
                              in
                                lp (i+1, w::ws)
                              end
                            else Mk.cons (avail, shape, List.rev ws)
                      in
                        lp (0, [])
                      end
                  | _ => raise Fail "unroll: shape is larger than index"
                (* end case *))
          in
            ignore (iter (IMap.empty, [], index, shape, 0, []));
            avail
          end

  (* in the general case, we expand the body to scalar code *)
    fun scalarExpand (params, body, index, lowArgs) =
          unroll (
            index, index,
            fn (avail, mapp) => EinToScalar.expand {
                avail=avail, mapp=mapp, body=body, lowArgs=lowArgs
              })

    fun createP (args, vecIndex, id, ix) =
          ToVec.Param{id = id, arg = List.nth(args, id), ix = ix, kind = ToVec.Proj vecIndex}
    fun createI (args, id, ix) =
          ToVec.Param{id = id, arg = List.nth(args, id), ix = ix, kind = ToVec.Indx}

  (* generate low-IL code for scaling a non-scalar tensor; `sId` is the scalar
   * parameter's ID and `vId` is the tensor parameter's ID.
   *)
    fun expandScale (sId, vId, shape, params, body, index, args) = let
          val (n, vecIX, index') = dropIndex index
          in
            case matchFindLast(shape, n)
             of (SOME ix, NONE) => let
                  val vecA = createI (args, sId, [])
                  val vecB = createP (args, vecIX, vId, ix)
                  val binop = ToVec.binopV (Op.VScale vecIX, vecIX)
                  in
                    unroll (
                      index, index',
                      fn (avail, mapp) => binop (avail, mapp, vecA, vecB))
                  end
              | _ => scalarExpand (params, body, index, args)
            (* end case *)
          end

  (* handle potential sum-of-products (i.e., inner products); otherwise fall back to the
   * general scalar case.
   *)
    fun expandInner (params, body, index, args) = (case body
           of E.Sum(
                [(v, _, ub)],
                E.Opn(E.Prod, [E.Tensor(id1, alpha as _::_), E.Tensor(id2, beta as _::_)])
              ) => (case (matchFindLast(alpha, v), matchFindLast(beta, v))
                 of ((SOME ix1, NONE), (SOME ix2, NONE)) => let
                    (* v is the last index of alpha, beta and nowhere else *)
                      val vecIX= ub+1
                      val vecA = createP (args, vecIX, id1, ix1)
                      val vecB = createP (args, vecIX, id2, ix2)
                      in
                        unroll (
                          index, index,
                          fn (avail, mapp) => ToVec.dotV (avail, mapp, vecA, vecB))
                      end
                  | _ => scalarExpand (params, body, index, args)
                (* end case *))
            | E.Sum(
                [(v1, lb1, ub1), (v2, lb2, ub2)],
                E.Opn(E.Prod, [E.Tensor(id1, alpha as _::_), E.Tensor(id2, beta as _::_)])
              ) => let
                fun check (v, ub, i, lb', ub') = (
                      case (matchFindLast(alpha, v), matchFindLast(beta, v))
                       of ((SOME ix1, NONE), (SOME ix2, NONE)) => let
                          (* v is the last index of alpha, beta and nowhere else *)
                            val vecIX = ub+1
                            val vecA = createP (args, vecIX, id1, ix1)
                            val vecB = createP (args, vecIX, id2, ix2)
                            in
                              SOME(unroll (
                                index, index,
                                fn (avail, mapp) =>
                                  ToVec.sumDotV (avail, mapp, i, lb', ub', vecA, vecB)))
                            end
                        | _ => NONE
                      (* end case *))
                in
                  case check(v1, ub1, v2, lb2, ub2)
                   of SOME e =>e
                    | _ => (case check(v2, ub2, v1, lb1, ub1)
                         of SOME e => e
                          | _ => scalarExpand (params, body, index, args)
                      (* end case *))
                  (* end case *)
                end
            |  _ => scalarExpand (params, body, index, args)
          (* end case *))

  (* expand an Ein expression that has a non-scalar result *)
  (*In order to project a tensor expression into a vector
  * (1) the last index must be E.V n (found by dropIndex()) and
  * (2) the index can not repeat in the term*)
  fun nonScalar (params, body, index, args) = (case body
           of E.Op2(E.Sub, E.Tensor(id1, alpha as _::_), E.Tensor(id2, beta as _::_)) => let
                val (n, vecIX, index') = dropIndex index
                in
                  case (matchFindLast(alpha, n), matchFindLast(beta, n))
                   of ((SOME ix1, NONE), (SOME ix2, NONE)) => let
                        val vecA = createP (args, vecIX, id1, ix1)
                        val vecB = createP (args, vecIX, id2, ix2)
                        val binop = ToVec.binopV (Op.VSub vecIX, vecIX)
                        in
                          unroll (
                            index, index',
                            fn (avail, mapp) => binop (avail, mapp, vecA, vecB))
                        end
                    | _  => scalarExpand (params, body, index, args)
                  (* end case *)
                end
            | E.Opn(E.Add, es as E.Tensor(_, _::_)::_) => let
                val (n, vecIX, index') = dropIndex index
                fun sample ([], rest) =
                      unroll (
                        index, index',
                        fn (avail, mapp) => ToVec.addV (avail, mapp, List.rev rest))
                  | sample (E.Tensor(id1, alpha)::ts, rest) = (case matchFindLast(alpha, n)
                       of (SOME ix1, NONE) => sample(ts, createP (args, vecIX, id1, ix1)::rest)
                        | _ => scalarExpand (params, body, index, args)
                      (* end case *))
                  | sample _ = scalarExpand (params, body, index, args)
                in
                  sample (es, [])
                end
            | E.Op1(E.Neg, E.Tensor(id, alpha as (_::_))) => let
                val (n, vecIX, index') = dropIndex index
                in
                  case matchFindLast (alpha, n)
                   of (SOME ix1, NONE) => unroll (
                        index, index',
                        fn (avail, mapp) => ToVec.negV (avail, mapp, createP (args, vecIX, id, ix1)))
                    | _ => scalarExpand (params, body, index, args)
                  (* end case *)
                end
            | E.Opn(E.Prod, [E.Tensor(s, []), E.Tensor(v, shp as _::_)]) =>
                expandScale (s, v, shp, params, body, index, args)
            | E.Opn(E.Prod, [E.Tensor(v, shp as _::_), E.Tensor(s , [])]) =>
                expandScale (s, v, shp, params, body, index, args)
            | E.Opn(E.Prod, [E.Tensor(id1 , alpha as [_]), E.Tensor(id2, beta as [_])]) => let
                val (n, vecIX, index') = dropIndex index
                in
                  case (matchFindLast(alpha, n), matchFindLast(beta, n))
                   of ((SOME ix1, NONE), (SOME ix2, NONE)) => let
                      (* n is the last index of alpha, beta and nowhere else, possible modulate *)
                        val vecA = createP (args, vecIX, id1, ix1)
                        val vecB = createP (args, vecIX, id2, ix2)
                        val binop = ToVec.binopV (Op.VMul vecIX, vecIX)
                        in
                          unroll (
                            index, index',
                            fn (avail, mapp) => binop (avail, mapp, vecA, vecB))
                        end
                    | ((NONE, NONE), (SOME ix2, NONE)) => let
                      (* n is the last index of beta and nowhere else, possible scaleVector *)
                        val vecA = createI (args, id1, alpha)
                        val vecB = createP (args, vecIX, id2, ix2)
                        val binop = ToVec.binopV (Op.VScale vecIX, vecIX)
                        in
                          unroll (
                            index, index',
                            fn (avail, mapp) => binop (avail, mapp, vecA, vecB))
                        end
                    | ((SOME ix1, NONE), (NONE, NONE)) => let
                      (* n is the last index of alpha and nowhere else, ossile scaleVector *)
                        val vecA = createI (args, id2, beta)
                        val vecB = createP (args, vecIX, id1, ix1)
                        val binop = ToVec.binopV (Op.VScale vecIX, vecIX)
                        in
                          unroll (
                            index, index',
                            fn (avail, mapp) => binop (avail, mapp, vecA, vecB))
                        end
                    | _ => scalarExpand (params, body, index, args)
                  (* end case *)
                end
            |  _ => expandInner (params, body, index, args)
          (* end case *))

  (* scan:var*E.Ein*Var list * Var list-> Var*LowIR.Assgn list
   * scans body  for vectorization potential
   *)
    fun expand (y, ein as Ein.EIN{params, index, body}, args) = let
          val avail = (case (List.rev index)
                 of _::_ => nonScalar (params, body, index, args)
                  | _ => expandInner (params, body, index, args)
                (* end case *))
          in
            case AvailRHS.getAssignments avail
             of [] => [LowIR.ASSGN(y, LowIR.VAR(List.nth (args, 0)))]
                | (_, asgn)::rest => List.revMap LowIR.ASSGN ((y, asgn)::rest)
            (* end case *)
          end

    end
