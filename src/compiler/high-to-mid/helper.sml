(* expand-fem.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* handle for evaluating fem
* and checking inside a fem field
*)

structure Helper : sig

    val getRHSEINSrc: HighIR.var -> string
    val getRHSEIN :  MidIR.var -> Ein.ein * MidIR.var list
    val getRHSS :  MidIR.var ->  string
    val ll : MidIR.var list * int -> string
    val paramToString : int * Ein.param_kind -> string
    val prntNewbies: (MidIR.var * MidIR.rhs) list * 'a -> string list
    val toStringBA: string * Ein.ein_exp * MidIR.var list  -> string
    val line : string * MidIR.var * Ein.ein * MidIR.var list -> string
    val iterP: Ein.ein_exp list -> Ein.ein_exp
    val iterA: Ein.ein_exp list  -> Ein.ein_exp
    
  end = struct

    structure SrcIR = HighIR
    structure IR = MidIR
    structure Ty = MidTypes
    structure Op = MidOps
    structure V = IR.Var
    structure GVar = IR.GlobalVar
    structure E = Ein
    structure SrcIR = HighIR
    structure DstIR = MidIR

     (* ------------------------------------ cvt to strings  --------------------------------------------  *)
    fun useCount (SrcIR.V{useCnt, ...}) = !useCnt
    fun ll ([], cnt) = ""
      | ll (a1::args, cnt) = 
        String.concat["\n\t", Int.toString(cnt), "_", MidTypes.toString(DstIR.Var.ty a1), " ", MidIR.Var.name(a1), ", ", ll(args, cnt+1)]
    val i2s = Int.toString
    val shp2s = String.concatWithMap " " i2s
    fun paramToString (i, E.TEN(t, shp)) = concat["T", i2s i, "[", shp2s shp, "]"]
      | paramToString (i, E.FLD d) = concat["F", i2s i, "(", i2s d, ")"]
      | paramToString (i, E.KRN) = "H" ^ i2s i
      | paramToString (i, E.IMG(d, shp)) = concat["V", i2s i, "(", i2s d, ")[", shp2s shp, "]"]
     fun prntNewbies(newbies, id) = let 
        val _ = (id)
        in  List.map (fn (lhs, DstIR.EINAPP(e, a)) => (concat["\n\n ->:", MidTypes.toString(DstIR.Var.ty lhs), " ", DstIR.Var.name(lhs), " = ", EinPP.toString(e) , "-", ll(a, 0), "---->"])
                    | (lhs, rhs) => (concat["\n\n -->:", DstIR.Var.name(lhs), " = ", DstIR.RHS.toString rhs])
            ) newbies
        end
     fun line(name, y, ein, args) = String.concat[name, ":", MidIR.Var.name(y), " = ", EinPP.toString(ein), "-", ll(args, 0)]
     fun toStringBA(name, e, args) = (String.concat["\n\n", name, ": body:", EinPP.expToString(e), " args#:", Int.toString(length(args)), "\n\n"])
     (* ------------------------------------ get RHS --------------------------------------------  *)
    fun getRHSEINSrc x = (case SrcIR.Var.getDef x
        of  SrcIR.EINAPP (ein, args) => ("ein-app")
        | SrcIR.LIT l => (concat["\n\nSrcLIT expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.LIT l)])
        | SrcIR.OP l => (concat["\n\nsrcOP expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.OP l)])
        | SrcIR.CONS l => (concat["\n\nsrccons expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.CONS l)])
        | SrcIR.GLOBAL l => (concat["\n\nsrcglobal expected LHS rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString (SrcIR.GLOBAL l)])
        | rhs => (concat["\n\nsrcexpected rhs operator for ", SrcIR.Var.toString x, " but found ", SrcIR.RHS.toString rhs])
        (* end case *))
    fun getRHSEIN x = (case IR.Var.getDef x
        of  IR.EINAPP (ein, args) => (ein, args)
        | IR.LIT l => raise Fail(concat["\n\nLIT expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.LIT l)])
        | IR.OP l => raise Fail(concat["\n\nOP expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.OP l)])
        | IR.CONS l => raise Fail(concat["\n\ncons expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.CONS l)])
        | IR.GLOBAL l => raise Fail(concat["\n\nglobal expected LHS rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString (IR.GLOBAL l)])
        | rhs => raise Fail(concat["\n\nexpected rhs operator for ", IR.Var.toString x, " but found ", IR.RHS.toString rhs])
        (* end case *))
    fun getRHSS x = (case IR.Var.getDef x
        of  IR.LIT(Literal.String s) => s
        | rhs => raise Fail(concat[
         "expected rhs operator for ", IR.Var.toString x, 
            " but found ", IR.RHS.toString rhs
        ])
        (* end case *))        
    (* ------------------------------------ rewriting --------------------------------------------  *)
    fun iterP es =  let 
        fun iterPP([], [r]) = r
        | iterPP ([], rest) = E.Opn(E.Prod, rest)
        | iterPP (E.Const 0::es, rest) = E.Const(0)
        | iterPP (E.Const 1::es, rest) = iterPP(es, rest)
        | iterPP (E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::es, rest) = 
            (* variable can't be 0 and 1 '*)
            if(c1 = c2 orelse (not (v1 = v2)))
            then iterPP (es, E.Delta(E.C c1, E.V v1)::E.Delta(E.C c2, E.V v2)::rest)
            else  E.Const(0)
        | iterPP(E.Opn(E.Prod, ys)::es, rest) = iterPP(ys@es, rest)
        | iterPP (e1::es, rest)   = iterPP(es, e1::rest)
    in iterPP(es, []) end
    fun iterA es =  let
        fun iterAA([], []) = E.Const 0
        | iterAA([], [r]) = r
        | iterAA ([], rest) = E.Opn(E.Add, rest)
        | iterAA (E.Const 0::es, rest) = iterAA(es, rest)
        | iterAA (E.Opn(E.Add, ys)::es, rest) = iterAA(ys@es, rest)
        | iterAA (e1::es, rest)   = iterAA(es, e1::rest)
    in iterAA(es, []) end   
end
