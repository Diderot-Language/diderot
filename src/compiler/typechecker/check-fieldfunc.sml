(* check-field-func.sml
 *
 * The typechecker for expressions.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure CheckFieldFunc : sig

  (* type check an expression *)
    val check : Env.t * Env.context * ParseTree.expr -> unit

  end = struct

    structure PT = ParseTree
    structure L = Literal
    structure E = Env
    structure Ty = Types
    structure BV = BasisVars
    structure TU = TypeUtil
    structure ISet = IntRedBlackSet
   
  (* an expression to return when there is a type error *)
    fun err arg = (TypeError.error arg; ())

    datatype token = datatype TypeError.token

  (* mark a variable use with its location *)
    fun useVar (cxt : Env.context, x) = (x, #2 cxt)

  (* strip any marks that enclose an expression and return the span and the expression *)
    fun stripMark (_, PT.E_Mark{span, tree}) = stripMark(span, tree)
      | stripMark (span, e) = (span, e)
         
  (* 
   * In order to differentiate an unary-input defined field the entire 
   * function expression needs to be represented as an EIN operator. 
   * The following is a list of operators that are later represented in EIN
   *)
 (* FIXME: use AtomSet instead of list *)
    val einOps = [
            BasisNames.op_add, BasisNames.op_sub, BasisNames.op_mul, BasisNames.op_dot,
            BasisNames.op_cross, BasisNames.op_convolve, BasisNames.op_outer, BasisNames.op_colon,
            BasisNames.op_div, BasisNames.op_at ,BasisNames.op_neg,
            BasisNames.op_D, BasisNames.op_Ddot, BasisNames.op_Dotimes, BasisNames.op_curl, BasisNames.op_norm,
            BasisNames.fn_inv, BasisNames.fn_modulate, BasisNames.fn_normalize, BasisNames.fn_trace,
            BasisNames.fn_transpose, BasisNames.fn_acos, BasisNames.fn_asin, BasisNames.fn_atan,
            BasisNames.fn_cos, BasisNames.fn_exp, BasisNames.fn_sin, BasisNames.fn_sqrt, BasisNames.fn_tan
          ]
   
    fun funcErr (cxt, exp) = err(cxt, [
            S "Field definition is invalid: ", exp,  S " is not yet supported inside field definition"
          ])
 
    fun checkRator (cxt, rator) = (case (List.find (fn e => Atom.same(rator, e)) einOps)
           of SOME _ => ()
            | NONE => funcErr (cxt, A rator)          
          (* end case *))

  (* check the type of an expression *)
    fun check (env, cxt, e) = (case e
           of PT.E_Mark m => check (E.withEnvAndContext (env, cxt, m))
            | PT.E_Cond(e1, cond, e2) => funcErr (cxt, S "conditional expression")
            | PT.E_Range(e1, e2) => funcErr (cxt, S "range expression")
            | PT.E_OrElse(e1, e2) => funcErr (cxt, S "orelse expression")
            | PT.E_AndAlso(e1, e2) => funcErr (cxt, S "andalso expression")
            | PT.E_BinOp (e1, rator, e2) => (
                check(env, cxt, e1);
                check(env, cxt, e2);
                checkRator (cxt, rator))
            | PT.E_UnaryOp(rator, e) => (
                check(env, cxt, e);
                checkRator (cxt, rator))
            | PT.E_Apply(e, args) => (
                List.app (fn e1 => check (env, cxt, e1)) (e::args);
                case stripMark(#2 cxt, e)
                 of (span, PT.E_Var rator) => checkRator (cxt, rator)
                  | _ => funcErr (cxt, S "application expression")
                (* end case *))
            | PT.E_Subscript(e, indices) => funcErr (cxt, S "subscript expression")
            | PT.E_Select(e, field) => funcErr (cxt, S "select expression")
            | PT.E_Real e => funcErr (cxt, S "real expression")
            | PT.E_LoadSeq nrrd=> funcErr (cxt, S "load Seq")
            | PT.E_LoadImage nrrd => funcErr (cxt, S "load Image")
            | PT.E_Var x => funcErr (cxt, S "variable")
                (* FIXME: need to look up this variable and check corresponding expressions *)
            | PT.E_Kernel(kern, dim) => funcErr (cxt, S "select expression")
            | PT.E_Lit lit => ()
            | PT.E_Id d => ()
            | PT.E_Zero dd => funcErr (cxt, S "zero matrix")
            | PT.E_NaN dd => funcErr (cxt, S "NaN")
            | PT.E_Sequence exps => funcErr (cxt, S "sequence")
            | PT.E_SeqComp comp => funcErr (cxt, S "sequence comp")
            | PT.E_Cons args => funcErr (cxt, S "cons construction")
          (* end case *))

  end
