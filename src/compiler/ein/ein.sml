(* ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Ein =
  struct

    datatype param_kind
      = TEN of bool * int list  (* the boolean specifies if the parameter is substitutable *)
      | FLD of int
      | KRN
      | IMG of int * int list

    (* placement in argument list *)
    type param_id= int
    (* variable index *)
    type index_id = int
    (* binding of variable index *)
    type index_bind = int

    datatype ein = EIN of {
        params : param_kind list,
        (* Parameters to equation are either tensors or fields *)
        (* These variables are named using de Bruijn indexing *)
(* QUESTION: why is this index_bind list and not index_id list?
* Answer: index_bind is the range of the index_ids.
* Both ints but ultimately different types
*)
        index : index_bind list,        (* Index variables in the equation. *)
        body : ein_exp
      }

    and mu
      = V of index_id
      | C of int

    and unary
      = Neg
      | Exp
      | Sqrt
      | Cosine
      | ArcCosine
      | Sine
      | ArcSine
      | Tangent
      | ArcTangent
(* QUESTION: should the int be IntInf.int? *)
      | PowInt of int
      | Abs
      | Sgn (* sign (positive or negative) *)

    and binary = Sub | Div

    and ternary = Clamp

    and opn = Add | Prod

    and input_ty = T | F (* treat as a tensor or field during differentiaton *)

  (* other kinds of fields *)
    and ofield = CFExp of (param_id * input_ty) list      (* input variables TT and TF*)

    and ein_exp
    (* Basic terms *)
      = Const of int
(* QUESTION: should this be RealLit.t? *)
      | ConstR of Rational.t
      | Tensor of param_id * alpha
      | Zero of alpha
      | Delta of mu * mu
      | Epsilon of mu * mu * mu
      | Eps2 of mu * mu
    (* High-IL Terms *)
      | Field of param_id * alpha
      | Lift of ein_exp
      | Conv of param_id * alpha * param_id * alpha
      | Partial of alpha
      | Apply of ein_exp * ein_exp
      | Comp of ein_exp * subEIN list
      | Probe of ein_exp * ein_exp
      | OField of ofield * ein_exp * ein_exp (* field arg T, exp, E.Partial dx *) (* FIXME: need more info *)
    (* Mid-IL Terms *)
      | Value of index_id (* Lift index *)
      | Img of param_id * alpha * pos list * int
      | Krn of param_id * (mu * mu) list * int
      | Poly of ein_exp * int * alpha  (* ein_exp^n dx *)
    (* Ops *)
      | Sum of sumrange list * ein_exp
      | Op1 of unary * ein_exp
      | Op2 of binary * ein_exp * ein_exp
      | Op3 of ternary * ein_exp * ein_exp * ein_exp
      | Opn of opn * ein_exp list

    withtype alpha = mu list
        and pos = ein_exp
        and sumrange = index_id * int * int
        and subEIN = ein_exp * index_bind list
  end

