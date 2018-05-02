(* eps-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

(* Helper functions for normalize-ein_exp
 * does the rewriting for Epsilon and deltas
 *)
structure EpsUtil : sig

    val epsToDels : Ein.ein_exp list
          -> (Ein.ein_exp * Ein.sumrange list) option * Ein.ein_exp list option * Ein.ein_exp list

    val reduceDelta : Ein.ein_exp list * Ein.ein_exp list * Ein.ein_exp list
          -> bool * Ein.ein_exp

    val matchEps : Ein.mu list * Ein.mu list -> bool

  end = struct

    structure E = Ein

    fun err str = raise Fail (String.concat["Ill-formed EIN Operator",str])

  (* doubleEps (E.Epsilon(a,b,c), E.Epsilon(d,e,f))
   * check to see if a pair of expressions can be converted to deltas.
   * The parameter is
   *    (E.Epsilon(a,b,c), E.Epsilon(d,e,f))    -- pair of epsilon EIN expressions
   *)
    fun doubleEps (E.Epsilon(a,b,c), E.Epsilon(d,e,f)) = let
          fun createDeltas (s, t, u, v) =
            (* If multiple variable ids are the same
                then do not do epsilon rewriting
            *)
                if (s=u andalso t=v) orelse (s=v andalso t=u)
                  then NONE
                  else let
                    val d1 = [E.Delta(s, u), E.Delta(t, v)]
                    val d2 = [E.Delta(s, v), E.Delta(t, u)]
                    in
                      SOME(d1, d2)
                    end
          in
            if (a=d) then createDeltas(b, c, e, f)
            else if (a=e) then createDeltas(b, c, f, d)
            else if (a=f) then createDeltas(b, c, d, e)
            else if (b=d) then createDeltas(c, a, e, f)
            else if (b=e) then createDeltas(c, a, f, d)
            else if (b=f) then createDeltas(c, a, d, e)
            else if (c=d) then createDeltas(a, b, e, f)
            else if (c=e) then createDeltas(a, b, f, d)
            else if (c=f) then createDeltas(a, b, d, e)
            else NONE
          end
      | doubleEps _ = raise Fail "None Epsilon Argument"

  (* distributeEps (epsAll, sumexp)
   * Check two epsilons at a time for a match. The parameters are
   *    expsAll     --  list of epsilons
   *    sumexp      --  summation expression with epsilon
   * Note there is either a list of epsilons in epsAll
   * or a single item in both arguments
   *)
    fun distributeEps (epsAll, sumexp) = let
          fun distEps ([], _) = NONE
            | distEps ([e1], eps) = NONE
            | distEps (e1::e2::current,eps) = (case doubleEps(e1, e2)
                 (* check two epislons at a time for a match *)
                 of SOME(d1, d2) => SOME(eps@current, d1, d2, [], sumexp)
                  | _ => distEps(e2::current, eps@[e1])
                 (* return unused epsilon list, and rewriten deltas pairs *)
               (* end case *))
          in
            distEps (epsAll, [])
          end

   (* epsToDels exps
    * filters litst of ein expressions to look for epsilons
    * when appropriate transforms epsilons to deltas. The parameter is
    *   exps    -- list of ein expressions
    *)
    fun epsToDels exps = let
          val (epsAll, rest, sumexp) = EinFilter.filterEps exps
          in
            case distributeEps(epsAll, sumexp)
             of NONE => if (length epsAll > 0)
                  then (NONE, SOME epsAll, rest@sumexp)
                  else (NONE, NONE, rest@sumexp)
              | SOME(epsUnused, d1, d2, sx, ps) => let
                (* d1, d2 are rewritten delta expressions *)
                (* in some cases there are embedded expressions of the form E.Sum(sx, ...ps)*)
                (* sx does not apply to the rest of the expression ("rest")*)
                (* return expression in pieces in order to preserve structure*)
                  val a = E.Opn(E.Prod, epsUnused@d1@ps)
                  val b = E.Opn(E.Prod, epsUnused@d2@ps)
                  in
                    (SOME(E.Op2(E.Sub, a, b), sx), NONE, rest)
                  end
            (* end case *)
          end


  (* reduceDelta (eps, dels, es)
   * Apply deltas to tensors/fields. The parameters are
   *    eps     -- ein expressions that are epsilons
   *    dels    -- ein expressions that are deltas
   *    es      -- the rest of ein expressions
   *)
    fun reduceDelta (eps, dels, es)=let
          fun appDel (changed, [], beta, mu, nu) = (changed, List.rev beta, mu @ List.rev nu)
            | appDel (changed, a::alpha, beta, [], nu) =
                appDel(changed, alpha, a::beta, List.rev nu, [])
            | appDel (changed, a::alpha, beta, d1::mu, nu) = (case d1
                 of E.Delta (_, E.C _) => appDel(changed, a::alpha, beta, mu, d1::nu)
                  | E.Delta(i, j) => if (a = j)
                      then appDel(true, alpha, i::beta, mu, nu)
                      else appDel(changed, a::alpha, beta, mu, d1::nu)
                (* end case *))
          fun distribute (changed, [], rest, mu) = (changed, E.Opn(E.Prod, eps@mu@(List.rev rest)))
            | distribute (changed, p1::ps, rest, mu) = (case p1
                 of E.Tensor(id, alpha) => let
                      val (changed, alpha_betas, nu) = appDel(changed, alpha, [], mu, [])
                      in
                        distribute(changed, ps, E.Tensor(id, alpha_betas)::rest, nu)
                      end
                  | E.Field(id, alpha) => let
                      val (changed, alpha_betas, nu) = appDel(changed, alpha, [], mu, [])
                      in
                        distribute(changed, ps, E.Field(id, alpha_betas)::rest, nu)
                      end
                  | E.Conv(v, alpha, h, dx) => let
                      val (changed, alpha_betas, nu1) = appDel(changed, alpha, [] ,mu, [])
                      val (changed, dx_betas, nu2) = appDel(changed, dx, [], nu1, [])
                      in
                        distribute(changed, ps, E.Conv(v,alpha_betas, h ,dx_betas)::rest, nu2)
                      end
                  | E.Probe(E.Conv(v, alpha, h, dx), t) => let
                      val (changed, alpha_betas, nu1) = appDel(changed, alpha, [], mu, [])
                      val (changed, dx_betas, nu2) = appDel(changed, dx, [], nu1, [])
                      in
                        distribute(changed, ps, E.Probe(E.Conv(v,alpha_betas, h ,dx_betas),t)::rest, nu2)
                      end
                  | E.Apply(E.Partial d, e) => let
                      val (changed, d_betas, nu) = appDel(changed,d,[],mu,[])
                      in
                        distribute(changed, ps, E.Apply(E.Partial d_betas,e)::rest, nu)
                      end
                  | _ => distribute(changed, ps, p1::rest, mu)
                (* end case *))
          in
            distribute (false, es, [], dels)
          end

  (* matchEps (ids, mus)
   * checks if two different indices in 'ids' both appear in 'mus'. The parameters are
   *    ids     -- ein indices in epsilon
   *    mus     -- ein indeces in differentiation
   *)
    fun matchEps (ids, mus) = let
          (* fun inMus id = List.exists (fn (E.C _) => false | (E.V x) => (x = id)) mus*)
          fun inMus id = List.exists (fn x => (x = id)) mus
          fun match0 [] = false
            | match0 (id::ids) = if inMus id
                then match1 (ids, id)
                else match0 ids
          and match1 ([], id) = false
            | match1 (id::ids, id') = ((id <> id') andalso inMus id) orelse match1 (ids, id')
          in
            match0 ids
          end

  end
