(* checkPT-field-func.sml
 *
 * The typecheckPTer for expressions.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure CheckFieldFunc : sig

 (* 
  * check expressions in the body in a field function
  * only allows operators that can be expressed in EIN notation
  *)
    val checkPT : Env.t * Env.context * ParseTree.expr -> unit
    val checkAST : Env.t * Env.context * AST.expr -> unit
    
  end = struct

    structure PT = ParseTree
    structure L = Literal
    structure E = Env
    structure Ty = Types
    structure BV = BasisVars
    structure TU = TypeUtil
    structure ISet = IntRedBlackSet
    structure AMap = AtomRedBlackMap
    structure IMap = IntRedBlackMap
    
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
    (* translated to EIN*)
    val einOp_ok = [
            BasisNames.op_add, BasisNames.op_sub, BasisNames.op_mul, BasisNames.op_dot,
            BasisNames.op_cross, BasisNames.op_convolve, BasisNames.op_outer, BasisNames.op_colon,
            BasisNames.op_div, BasisNames.op_at, BasisNames.op_neg, BasisNames.op_convolve,
            BasisNames.op_D, BasisNames.op_Ddot, BasisNames.op_Dotimes, BasisNames.op_curl, BasisNames.op_norm,
            BasisNames.fn_inv, BasisNames.fn_modulate, BasisNames.fn_normalize, BasisNames.fn_trace,
            BasisNames.fn_transpose, BasisNames.fn_acos, BasisNames.fn_asin, BasisNames.fn_atan,
            BasisNames.fn_cos, BasisNames.fn_exp, BasisNames.fn_sin, BasisNames.fn_sqrt, BasisNames.fn_tan
          ]
    (* some variables are translated to EIN *)
    val einOp_limit = [BasisNames.op_pow]
    val flag_ok = 0
    val flag_limit = 1
    val einMapp = List.foldr (fn (a, m) => AMap.insert(m, a, flag_ok)) AMap.empty einOp_ok
    val einMapp = List.foldr (fn (a, m) => AMap.insert(m, a, flag_limit)) einMapp einOp_limit
    
    fun funcErr (cxt, exp) = err(cxt, [
            S "Field definition is invalid: ", exp,  S " is not yet supported inside field definition"
          ])
 
    (* Note: in the Dev branch another checking pass is applied during the high-to-mid phase *)
    fun checkPTRator (cxt, rator) = 
        (case AMap.find(einMapp, rator)
           of SOME 0 => ()
            | SOME 1 => funcErr (cxt, A rator) 
            | NONE => funcErr (cxt, A rator) 
          (* end case *))

  (* check parse tree expression *)
    fun checkPT (env, cxt, e) = (case e
           of PT.E_Mark m => checkPT (E.withEnvAndContext (env, cxt, m))
            | PT.E_Cond(e1, cond, e2) => funcErr (cxt, S "conditional expression")
            | PT.E_Range(e1, e2) => funcErr (cxt, S "range expression")
            | PT.E_OrElse(e1, e2) => funcErr (cxt, S "orelse expression")
            | PT.E_AndAlso(e1, e2) => funcErr (cxt, S "andalso expression")
            | PT.E_BinOp (e1, rator, e2) => (
                checkPT(env, cxt, e1);
                checkPT(env, cxt, e2);
                checkPTRator (cxt, rator))
            | PT.E_UnaryOp(rator, e) => (
                checkPT(env, cxt, e);
                checkPTRator (cxt, rator))
            | PT.E_Apply(e, args) => (
                List.app (fn e1 => checkPT (env, cxt, e1)) (e::args);
                case stripMark(#2 cxt, e)
                 of (span, PT.E_Var rator) => checkPTRator (cxt, rator)
                  | _ => funcErr (cxt, S "application expression")
                (* end case *))
            | PT.E_Subscript(e, indices) => funcErr (cxt, S "subscript expression")
            | PT.E_Select(e, field) => funcErr (cxt, S "select expression")
            | PT.E_Real e => funcErr (cxt, S "real expression")
            | PT.E_LoadSeq nrrd=> funcErr (cxt, S "load Seq")
            | PT.E_LoadImage nrrd => funcErr (cxt, S "load Image")
            | PT.E_Var x => ()
            | PT.E_Kernel(kern, dim) => funcErr (cxt, S "select expression")
            | PT.E_Lit lit => ()
            | PT.E_Id d => ()
            | PT.E_Zero dd => funcErr (cxt, S "zero matrix")
            | PT.E_NaN dd => funcErr (cxt, S "NaN")
            | PT.E_Sequence exps => funcErr (cxt, S "sequence")
            | PT.E_SeqComp comp => funcErr (cxt, S "sequence comp")
            | PT.E_Cons args => funcErr (cxt, S "cons construction")
          (* end case *))
              
    (* check AST expression *)
    fun checkAST (env, cxt, e) = (case e
           of AST.E_Var _ => ()
            | AST.E_Lit _ => ()
            | AST.E_Kernel _ => ()
            | AST.E_Select _ => funcErr (cxt, S "selection")
            | AST.E_Prim (rator_var, v, e, ty) => let
                val rator = Var.atomNameOf rator_var
                in (case AMap.find(einMapp, rator)
                        of SOME 0 => () (* operator is okay *)
                         | SOME 1 =>    (* operator might be okay *)
                            if(Var.same(rator_var, BV.pow_ri))
                                then () 
                                (* 
                                 * BV.pow_ri might be translated 
                                 * to EIN or direct-style depending on arguments.
                                 * Therefor we are unsure if it is "OK" and relying 
                                 * on the check in high-to-mid to check
                                 *)
                                else funcErr (cxt, A rator) 
                         | NONE => funcErr (cxt, A rator) 
                     (* end case *))
                end       
            | AST.E_Apply _ => funcErr (cxt, S "application expression")
            | AST.E_Comprehension _ => funcErr (cxt, S "Comprehension")
            | AST.E_ParallelMap _ => funcErr (cxt, S "ParallelMap")
            | AST.E_Tensor (es, _) => checkASTs (env, cxt, es)
            | AST.E_Field (es, _) => checkASTs (env, cxt, es)
            | AST.E_Seq (es, _) => checkASTs (env, cxt, es)
            | AST.E_Slice (e1, _, _) => checkAST (env, cxt, e1)
            | AST.E_Cond _ => funcErr (cxt, S "conditional")
            | AST.E_Orelse _ => funcErr (cxt, S "orelse")
            | AST.E_Andalso _ => funcErr (cxt, S "andalso")
            | AST.E_LoadNrrd _ => ()
            | AST.E_Coerce _=> funcErr (cxt, S "Coerce")
          (* end case *))
    and checkASTs (env, cxt, es) = List.app (fn e => checkAST(env, cxt, e)) es
  end
