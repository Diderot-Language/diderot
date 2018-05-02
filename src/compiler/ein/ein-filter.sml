(* ein-filter.sml
 *
 * A collection of Fiter function that are used to organize ein_exps.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure EinFilter : sig

  (* flattens nested additions; returns NONE if there is no change *)
    val mkAdd : Ein.ein_exp list -> Ein.ein_exp option

  (* flattens nested products; returns NONE if there is no change *)
    val mkProd : Ein.ein_exp list -> Ein.ein_exp option

    val partitionGreek : Ein.ein_exp list
          -> Ein.ein_exp list * Ein.ein_exp list * Ein.ein_exp list * Ein.ein_exp list

    val partitionField : Ein.ein_exp list -> Ein.ein_exp list * Ein.ein_exp list

    val filterPartial : Ein.ein_exp list-> Ein.mu list

    val filterEps : Ein.ein_exp list-> Ein.ein_exp list * Ein.ein_exp list * Ein.ein_exp list

    val filterSca : Ein.sumrange list * Ein.ein_exp list -> bool * Ein.ein_exp

    val isScalar : Ein.ein_exp -> bool

  end = struct

    structure E = Ein

    fun err str = raise Fail(String.concat["Ill-formed EIN Operator ", str])

  (************** Functions that rewrite ein_exp********)

    fun mkAdd [e] = SOME e
      | mkAdd exps = let
          fun flatten (changed, E.Opn(E.Add, l)::l') = flatten(true, l@l')
            | flatten (changed, (E.Const c)::l') =
                if (c <> 0)
                  then let
                    val (changed, a) = flatten(changed, l')
                    in
                      (changed, E.Const c :: a)
                    end
                  else flatten(true, l')
            | flatten (changed, E.Zero _::l') =
                flatten(true, l')
            | flatten (changed, []) = (changed, [])
            | flatten (changed, e::l') = let
                val (changed, a) = flatten(changed, l')
                in
                  (changed, e :: a)
                end
          val (changed, a) = flatten(false, exps)
          in
            case a
             of [] => SOME(E.Const 0)
              | [e] => SOME e
              | es => if changed then SOME(E.Opn(E.Add, es)) else NONE
            (* end case *)
          end

    fun mkProd [e] = SOME e
      | mkProd exps = let
          fun flatten (_, [], []) = SOME(E.Const 1)
            | flatten (_, [], [e]) = SOME e
            | flatten (changed, [], es) = if changed
                then SOME(E.Opn(E.Prod, List.rev es))
                else NONE
            | flatten (changed, E.Opn(E.Prod, l)::rest, l') =
                flatten(true, rest, List.revAppend(l, l'))
            | flatten (changed, (E.Const 0)::rest, l') = SOME(E.Const 0)
            | flatten (changed, (E.Const 1)::rest, l') = flatten(true, rest, l')
            | flatten (changed, (E.Delta (E.C(0), E.C(1)))::rest,l') = SOME(E.Const 0)
            | flatten (changed, (E.Delta (E.C(1), E.C(0)))::rest,l') = SOME(E.Const 0)
            | flatten (changed, (E.Delta (E.C(0), E.C(0)))::rest,l') = flatten(true, rest, l')
            | flatten (changed, (E.Delta (E.C(1), E.C(1)))::rest,l') = flatten(true, rest, l')
            | flatten (changed, e::rest, l') = flatten (changed, rest, e::l')
          in
            flatten(false, exps, [])
          end

  (* rewriteProd : ein_exp list -> ein_exp
   * rewrite. Prod A
   * used by move_sum.sml
   *)
    fun rewriteProd [a] = a
      | rewriteProd exps = E.Opn(E.Prod, exps)

  (* rewriteSum : sum_indexid list* ein_exp list -> ein_exp
   * rewrite. Sum(c,Prod p))
   * used by move_sum.sml
   *)
    fun rewriteSum (c, p) = E.Sum(c, rewriteProd p)

  (* rewriteProdSum : ein_exp list*sum_indexid list* ein_exp list -> ein_exp
   * rewrite. Prod( pre*Sum(out,Prod post))
   * used by move_sum.sml
   *)
    fun rewriteProdSum (pre, _, []) = rewriteProd pre
      | rewriteProdSum (pre, outer, post) = rewriteProd (pre@[rewriteSum(outer, post)])

    fun isScalar e = (case e
        of E.Field(_, [])                   => true
        | E.Conv(_, [], _, [])              => true
        | E.Probe(E.Field(_, []) ,_)        => true
        | E.Probe(E.Conv(_, [], _, []), _)  => true
        | E.Tensor(id,[])                   => true
        | E.Const _                         => true
        | E.ConstR _                        => true
        | _                                 => false
    (* end case *))

  (************** Functions that partition expressions **************)

    (* partition scalars and greeks *)
    fun partitionGreek exps = let
          fun part ([], pre, eps, dels, post) =
                (List.rev pre, List.rev eps, List.rev dels, List.rev post)
            | part (e::es, pre, eps, dels, post) = (case e
                 of E.Opn(E.Prod, p)                 => part (p@es, pre, eps, dels, post)
                  | E.Field(_, [])                   => part (es, e::pre, eps, dels, post)
                  | E.Conv(_, [], _, [])             => part (es, e::pre, eps, dels, post)
                  | E.Probe(E.Field(_, []) ,_)       => part (es, e::pre, eps, dels, post)
                  | E.Probe(E.Conv(_, [], _, []), _) => part (es, e::pre, eps, dels, post)
                  | E.Tensor(id,[])                  => part (es, e::pre, eps, dels, post)
                  | E.Const _                        => part (es, e::pre, eps, dels, post)
                  | E.ConstR _                       => part (es, e::pre, eps, dels, post)
                  | E.Epsilon _                      => part (es, pre, e::eps, dels, post)
                  | E.Delta _                        => part (es, pre, eps, e::dels, post)
                  | _                                => part (es, pre, eps, dels, e::post)
                (* end case *))
          in
            part (exps, [], [], [], [])
          end


    (*filterField:ein_exp list->ein_exp list * ein_exp list
    * Note Lift indicates a Tensor
    * So expression is either Lift, del, eps, or contains a Field
    *used by order-ein.sml
    *)
    fun partitionField exps = let
          fun part ([], pre, post) = (List.rev pre, List.rev post)
            | part (e::es, pre, post) = (case e
                 of E.Opn(E.Prod, p) => part (p@es, pre, post)
                  | E.Lift _         => part (es, e::pre, post)
                  | E.Epsilon _      => part (es, e::pre, post)
                  | E.Eps2 _         => part (es, e::pre, post)
                  | E.Delta _        => part (es, e::pre, post)
                  | E.Const _        => part (es, e::pre, post)
                  | E.ConstR _       => part (es, e::pre, post)
                  | _                => part (es, pre, e::post)
              (*end case*))
          in
            part (exps, [], [])
          end

    (*filterPartial:ein_exp list-> mu list
    * peels mu in partial expression
    *)
    fun filterPartial [] = []
      | filterPartial (E.Partial d1::es) = d1 @ filterPartial es
      | filterPartial _ = err "Found non-Partial in Apply"

    (*filterEps:ein_exp list-> ein_exp list * ein_exp*ein_exp
    * filters eps and other
    * stops when we find embedded summation
    *)
    fun filterEps eps = let
          fun find (eps, [], rest) = (eps, rest, [])
            | find (eps, e::es, rest) = (case e
                 of E.Epsilon eps1                       => find (eps@[e], es, rest)
                  | E.Opn(E.Prod, p)                     => find (eps, p@es, rest)
                  | E.Field _                            => find (eps, es, rest@[e])
                  | E.Tensor _                           => find (eps, es, rest@[e])
                  | E.Zero _                           => find (eps, es, rest@[e])
                  | E.Sum(c, E.Opn(E.Prod, E.Epsilon eps1::ps))
                                                         => (eps, rest@es, [e])
                  |  _                                   => (eps, rest@[e]@es, [])
              (* end case *))
          in
            find ([], eps, [])
          end

    (*filterSca:sum_index_is list * ein_exp-> int*ein_exp
    *filter Scalars outside Summation product
    *)
    fun filterSca (c, exps) = let
          fun filter ([], [], post) = (false, E.Sum(c, rewriteProd(rev post)))
            | filter ([], pre, post) = (true, rewriteProdSum(rev pre, c, rev post))
            | filter (e::es, pre, post) = (case e
                 of E.Opn(E.Prod, p)              => filter (p@es, pre, post)
                  | E.Field(_,[])                 => filter (es, e::pre, post)
                  | E.Conv(_,[],_,[])             => filter (es, e::pre, post)
                  | E.Probe(E.Field(_,[]),_)      => filter (es, e::pre, post)
                  | E.Probe(E.Conv(_,[],_,[]),_)  => filter (es, e::pre, post)
                  | E.Tensor(id,[])               => filter (es, e::pre, post)
                  | E.Const _                     => filter (es, e::pre, post)
                  | E.ConstR _                    => filter (es, e::pre, post)
                  | _                             => filter (es, pre, e::post)
                (* end case *))
          in
            filter (exps, [], [])
          end

  end
