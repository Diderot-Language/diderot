(* check-fieldfunc.sml
 *
 * The typechecker for expressions.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
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

  (* an expression to return when there is a type error *)
    fun err arg = (TypeError.error arg; ())
    val warn = TypeError.warning

    datatype token = datatype TypeError.token

  (* mark a variable use with its location *)
    fun useVar (cxt : Env.context, x) = (x, #2 cxt)

  (* resolve overloading: we use a simple scheme that selects the first operator in the
   * list that matches the argument types.
   *)
   (*
    fun resolveOverload (_, rator, _, _, []) = raise Fail(concat[
            "resolveOverload: \"", Atom.toString rator, "\" has no candidates"
          ])
      | resolveOverload (cxt, rator, argTys, args, candidates) = let
(* FIXME: we could be more efficient by just checking for coercion matchs the first pass
 * and remembering those that are not pure EQ matches.
 *)
        (* build the result *)
          fun done (rator, tyArgs, args, rngTy) = if Var.same(rator, BV.pow_si)
                then let (* check that the second argument is a constant expression *)
                  val [e1, e2] = args
                  in
                    case CheckConst.eval (cxt, false, e2)
                     of SOME e2' =>
                          (AST.E_Prim(rator, tyArgs, [e1, ConstExpr.valueToExpr e2'], rngTy), rngTy)
                      | NONE => err(cxt, [
                            S "constant-integer exponent is required when lhs of '^' is a field"
                          ])
                  end
                else (AST.E_Prim(rator, tyArgs, args, rngTy), rngTy)
        (* try to match candidates while allowing type coercions *)
          fun tryMatchCandidates [] = err(cxt, [
                  S "unable to resolve overloaded operator ", A rator, S "\n",
                  S "  argument type is: ", TYS argTys, S "\n"
                ])
            | tryMatchCandidates (x::xs) = let
                val (tyArgs, Ty.T_Fun(domTy, rngTy)) = TU.instantiate(Var.typeOf x)
                in
                  case Unify.tryMatchArgs (domTy, args, argTys)
                   of SOME args' => done(x, tyArgs, args', rngTy)
                    | NONE => tryMatchCandidates xs
                  (* end case *)
                end
        (* try to match candidates without type coercions *)
          fun tryCandidates [] = tryMatchCandidates candidates
            | tryCandidates (x::xs) = let
                val (tyArgs, Ty.T_Fun(domTy, rngTy)) = TU.instantiate(Var.typeOf x)
                in
                  if Unify.tryEqualTypes(domTy, argTys)
                    then done(x, tyArgs, args, rngTy)
                    else tryCandidates xs
                end
          in
            tryCandidates candidates
          end
     *)     
   fun funcErr (cxt, exp) = err(cxt, [S "Field definition is invalid: ", exp,  S " is not yet supported inside field definition"])

  (* check the type of an expression *)
    fun check (env, cxt, e) = (case e
           of PT.E_Mark m => check (E.withEnvAndContext (env, cxt, m))
            | PT.E_Cond(e1, cond, e2) => funcErr (cxt, S "conditional expression")
            | PT.E_Range(e1, e2) =>  funcErr (cxt, S "range expression")
            | PT.E_OrElse(e1, e2) =>  funcErr (cxt, S "or else expression")
            | PT.E_AndAlso(e1, e2) =>  funcErr (cxt, S "andalso expression")
            (*
            | PT.E_BinOp (e1, rator, e2) => 
                  case Env.findFunc (env, rator)
                     of Env.PrimFun[rator] =>  funcErr (cxt, V rator)
                      | _ =>  raise Fail "impossible"
                      *)
            | PT.E_UnaryOp(rator, e) =>  
                let 
                    val _ = print"found unary"
                    val eTy = check(env, cxt, e)
                    in   case Env.findFunc (env, rator)
                        of Env.PrimFun[rator] => funcErr (cxt, V rator)
                         (* | Env.PrimFun ovldList => (resolveOverload (cxt, rator, [#2 eTy], [#1 eTy], ovldList);())*)
                        | _ => raise Fail "impossible"
                    end
            | PT.E_Apply(e, args) => (print "found apply";List.map (fn e1 => check (env, cxt, e1)) (e::args);())
            | PT.E_Subscript(e, indices) => funcErr (cxt, S "subscript expression")
            | PT.E_Select(e, field) => funcErr (cxt, S "select expression")
            | PT.E_Real e => funcErr (cxt, S "real expression")
            | PT.E_LoadSeq nrrd =>funcErr (cxt, S "load Seq")
            | PT.E_LoadImage nrrd => funcErr (cxt, S "load Image")
            | PT.E_Var x => (print "found var";())
            | PT.E_Kernel(kern, dim) => funcErr (cxt, S "select expression")
            | PT.E_Lit lit => ()
            | PT.E_Id d => ()
            | PT.E_Zero dd => funcErr (cxt, S "zero matrix")
            | PT.E_NaN dd => funcErr (cxt, S "NaN")
            | PT.E_Sequence exps => funcErr (cxt, S "Sequence")
            | PT.E_SeqComp comp => funcErr (cxt, S "Sequence Comp")
            | PT.E_Cons args => funcErr (cxt, S "CONS construction")
          (* end case *))


  end
