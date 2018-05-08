(* const-expr.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *
 * TODO: add support for evaluating constants from the command-line
 *)

structure ConstExpr : sig

  (* compile-time constant values *)
    datatype t
      = String of string
      | Bool of bool
      | Int of IntLit.t
      | Real of RealLit.t
      | Tensor of t list * Types.ty
      | Seq of t list * Types.ty
      | Expr of AST.expr        (* for more complicated tensor-valued expressions *)

  (* a property to attach to 'const' variables to track their value *)
    val define : Var.t * t -> unit
    val valueOf : Var.t -> t option

  (* convert a constant value to an AST expression *)
    val valueToExpr : t -> AST.expr

  (* return the type of a constant value *)
    val typeOfConst : t -> Types.ty

  (* convert a string from the command line (i.e., a "--C" option) to a constant value
   * of the specified type.
   *)
    val fromString : Types.ty * string -> t option

  end = struct

    structure L = Literal
    structure Ty = Types
    structure SS = Substring

  (* compile-time constant values *)
    datatype t
      = String of string
      | Bool of bool
      | Int of IntLit.t
      | Real of RealLit.t
      | Tensor of t list * Ty.ty
      | Seq of t list * Ty.ty
      | Expr of AST.expr        (* for more complicated tensor-valued expressions *)

  (* a property to attach to 'const' variables to track their value *)
    local
      val {peekFn : Var.t -> t option, setFn, ...} =
            Var.newProp (fn x => raise Fail("undefined constant " ^ Var.uniqueNameOf x))
    in
    val define = setFn
    val valueOf = peekFn
    end (* local *)

    fun typeOfConst (String _) = Ty.T_String
      | typeOfConst (Bool _) = Ty.T_Bool
      | typeOfConst (Int _) = Ty.T_Int
      | typeOfConst (Real _) = Ty.realTy
      | typeOfConst (Tensor(_, ty)) = ty
      | typeOfConst (Seq(_, ty)) = ty
      | typeOfConst (Expr e) = TypeOf.expr e

    fun valueToExpr (String s) = AST.E_Lit(L.String s)
      | valueToExpr (Bool b) = AST.E_Lit(L.Bool b)
      | valueToExpr (Int i) = AST.E_Lit(L.Int i)
      | valueToExpr (Real r) = AST.E_Lit(L.Real r)
      | valueToExpr (Tensor(vs, ty)) = AST.E_Tensor(List.map valueToExpr vs, ty)
      | valueToExpr (Seq(vs, ty)) = AST.E_Seq(List.map valueToExpr vs, ty)
      | valueToExpr (Expr e) = e

    val skipWS = StringCvt.skipWS SS.getc

    fun fromString (ty, def) = let
          fun parseVal ty def = (case ty
                 of Ty.T_Bool => (case Bool.scan SS.getc def
                       of SOME (b, rest) => SOME(Bool b, rest)
                        | NONE => NONE
                      (* end case *))
                  | Ty.T_Int => (case IntInf.scan StringCvt.DEC SS.getc def
                       of SOME(n, rest) => SOME(Int n, rest)
                        | NONE => NONE
                      (* end case *))
                  | Ty.T_String => (case SS.getc def
                       of SOME(#"\"", rest) => let
(* FIXME: allow numeric escape characters and UTF-8 *)
                            fun scan (ss, chrs) = (case SS.getc ss
                                   of SOME(#"\"", rest) =>
                                        SOME(String(String.implodeRev chrs), rest)
                                    | SOME(#"\\", rest) => ((* escape sequence *)
                                        case SS.getc rest
                                         of SOME(c as #"\"", rest) => scan (rest, c::chrs)
                                          | SOME(c as #"\\", rest) => scan (rest, c::chrs)
                                          | SOME(#"a", rest) => scan (rest, #"\a"::chrs)
                                          | SOME(#"b", rest) => scan (rest, #"\b"::chrs)
                                          | SOME(#"f", rest) => scan (rest, #"\f"::chrs)
                                          | SOME(#"n", rest) => scan (rest, #"\n"::chrs)
                                          | SOME(#"r", rest) => scan (rest, #"\r"::chrs)
                                          | SOME(#"t", rest) => scan (rest, #"\t"::chrs)
                                          | SOME(#"v", rest) => scan (rest, #"\v"::chrs)
                                          | _ => NONE
                                        (* end case *))
                                    | SOME(c, rest) => if Char.isPrint c
                                        then scan (rest, c::chrs)
                                        else NONE
                                    | NONE => NONE
                                  (* end case *))
                            in
                              scan (rest, [])
                            end
                        | _ => NONE
                      (* end case *))
                  | Ty.T_Sequence(ty, dim) => (case SS.getc def
                       of SOME(#"{", rest) => (case parseVals (ty, rest)
                             of SOME(items, rest) => (case SS.getc (skipWS rest)
                                   of SOME(#"}", rest) => (case dim
                                         of NONE => SOME(Seq(items, ty), rest)
                                          | SOME(Ty.DimConst d) =>
                                              if (length items = d)
                                                then SOME(Seq(items, ty), rest)
                                                else NONE
                                        (* end case *))
                                    | _ => NONE
                                  (* end case *))
                              | NONE => NONE
                            (* end case *))
                        | _ => NONE
                      (* end case *))
                  | Ty.T_Tensor(Ty.Shape[]) => let
                      val (isNeg, rest) = (case SS.getc def
                             of SOME(#"-", rest) => (true, rest)
                              | SOME(#"+", rest) => (false, rest)
                              | _ => (false, def)
                            (* end case *))
                      fun getDigits start = let
                            fun return 0 = ("", start)
                              | return n = let
                                  val (digs, rest) = SS.splitAt(start, n)
                                  in
                                    (SS.string digs, rest)
                                  end
                            fun lp (ss, n) = (case SS.getc ss
                                   of SOME(c, rest) => if Char.isDigit c
                                        then lp (rest, n+1)
                                        else return n
                                    | NONE => return n
                                  (* end case *))
                            in
                              lp (start, 0)
                            end
                      fun mkReal ("", "", _, _) = NONE
                        | mkReal (whole, frac, exp, rest) = SOME(
                            Real(RealLit.real{isNeg = isNeg, whole = whole, frac = frac, exp = exp}),
                            rest)
                      fun getExp (ss, whole, frac) = let
                            fun getExpVal ss = (case IntInf.scan StringCvt.DEC SS.getc ss
                                   of NONE => NONE
                                    | SOME(e, rest) => mkReal (whole, frac, e, rest)
                                  (* end case *))
                            in
                              case SS.getc ss
                               of SOME(#"e", rest) => getExpVal rest
                                | SOME(#"E", rest) => getExpVal rest
                                | _ => mkReal(whole, frac, 0, ss)
                              (* end case *)
                            end
                      fun getFrac (ss, whole) = (case SS.getc ss
                             of SOME(#".", rest) => let
                                  val (frac, rest) = getDigits rest
                                  in
                                    getExp (rest, whole, frac)
                                  end
                              | _ => getExp (ss, whole, "")
                            (* end case *))
                      val (whole, rest) = getDigits rest
                      in
                        getFrac (rest, whole)
                      end
                  | Ty.T_Tensor(Ty.Shape(Ty.DimConst d :: dd)) => ((* higher-order tensor *)
                      case SS.getc def
                       of SOME(#"[", rest) => (case parseVals (Ty.T_Tensor(Ty.Shape dd), rest)
                             of SOME(items, rest) => (case SS.getc (skipWS rest)
                                   of SOME(#"]", rest) => if (length items = d)
                                        then SOME(Tensor(items, ty), rest)
                                        else NONE
                                    | _ => NONE
                                  (* end case *))
                              | NONE => NONE
                            (* end case *))
                        | _ => NONE
                      (* end case *))
                  | Ty.T_Error => SOME(Int 0, def) (* placeholder for error case *)
                  | _ => raise Fail "unexpected type for constant"
                (* end case *))
        (* parse a comma-separated sequence of values of the given type *)
          and parseVals (ty, ss) = let
                val parseVal = parseVal ty
                fun lp (ss, items) = (case SS.getc (skipWS ss)
                       of SOME(#",", rest) => (case parseVal (skipWS rest)
                             of SOME(v, rest) => lp (rest, v::items)
                              | NONE => NONE (* syntax error: extra comma *)
                            (* end case *))
                        | SOME _ => SOME(List.rev items, ss)
                        | NONE => NONE (* syntax error: expected comma, "}", or "]" *)
                      (* end case *))
                val ss = skipWS ss
                in
                  case parseVal ss
                   of SOME(v, rest) => lp(rest, [v])
                    | NONE => SOME([], ss)
                  (* end case *)
                end
          in
            case parseVal (TypeUtil.prune ty) (skipWS (SS.full def))
             of SOME(ast, rest) => if SS.isEmpty(skipWS rest)
                  then SOME ast
                  else NONE (* trailing junk *)
              | NONE => NONE
            (* end case *)
          end

  end
