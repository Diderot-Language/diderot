(* print-as-c.sml
 *
 * Print CLang syntax trees using C syntax.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure PrintAsC : sig

    val output : TextIOPP.stream * CLang.decl -> unit

  end = struct

    structure CL = CLang
    structure PP = TextIOPP

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent = (PP.Abs 4)     (* standard indentation amount *)

    fun numTyName rty = (case rty
           of RawTypes.RT_Int8 => "int8_t"
            | RawTypes.RT_UInt8 => "uint8_t"
            | RawTypes.RT_Int16 => "int16_t"
            | RawTypes.RT_UInt16 => "uint16_t"
            | RawTypes.RT_Int32 => "int32_t"
            | RawTypes.RT_UInt32 => "uint32_t"
            | RawTypes.RT_Int64 => "int64_t"
            | RawTypes.RT_UInt64 => "uint64_t"
            | RawTypes.RT_Float => "float"
            | RawTypes.RT_Double => "double"
          (* end case *))

  (* convert a type plus optional varaible to a string *)
    and tyToS (ty, optVar)= let
          fun prefix (s, "") = s
            | prefix (s1, s2) = concat[s1, " ", s2]
          fun wrapArray (tyOp, CL.T_Array(ty', sz), acc) = let
                val suffix = (case sz
                       of NONE => "[]"
                        | SOME n => concat["[", Int.toString n, "]"]
                      (* end case *))
                in
                  toS (ty', concat[tyOp, "(", acc, suffix, ")"])
                end
            | wrapArray (tyOp, ty, acc) = toS (ty, prefix(tyOp, acc))
          and toS (ty, acc) = (case ty
                 of CL.T_Num nty => prefix(numTyName nty, acc)
                  | CL.T_Const ty => toS (ty, prefix("const", acc))
                  | CL.T_Ptr ty => wrapArray ("*", ty, acc)
                  | CL.T_Ref ty => wrapArray ("&", ty, acc)
                  | CL.T_RestrictPtr _ => wrapArray ("* __restrict__", ty, acc)
                  | CL.T_Array(ty, NONE) => toS (ty, acc ^ "[]")
                  | CL.T_Array(ty, SOME n) =>
                      toS (ty, concat[acc, "[", Int.toString n, "]"])
                  | CL.T_Named ty => prefix(ty, acc)
                  | CL.T_Template(ty, tyArgs) => raise Fail "unexpected template type in C"
                  | CL.T_Qual(attr, ty) => concat[attr, " ", toS(ty, acc)]
                  | CL.T_Member _ => raise Fail "unexpected type member in C"
                (* end case *))
          in
            case optVar
             of SOME x => toS (ty, x)
              | NONE => toS (ty, "")
            (* end case *)
          end

  (* convert a type to a string *)
    and tyToS' ty = tyToS (ty, NONE)

    fun output (_, CL.D_Verbatim[]) = ()
      | output (strm, decl) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
          fun inHBox f = (PP.openHBox strm; f(); PP.closeBox strm)
          fun ppCom s = inHBox (fn () => (str "// "; str s))
          fun ppList {pp, sep, l} = let
                fun ppList' [] = ()
                  | ppList' [x] = pp x
                  | ppList' (x::xs) = (pp x; sep(); ppList' xs)
                in
                  ppList' l
                end
          fun ppCommaList {pp, l} = ppList {pp = pp, sep = fn () => (str ","; sp()), l = l}
          fun ppTyAndVar (ty, x) = str (tyToS (ty, SOME x))
          fun ppTy ty = str (tyToS (ty, NONE))
          fun ppAttrs [] = ()
            | ppAttrs attrs = (
                ppList {pp=str, sep=sp, l = attrs};
                sp())
          fun ppDecl dcl = (case dcl
                 of CL.D_Pragma l => (
                      inHBox (fn () => (
                        str "#pragma";
                        List.app (fn s => (sp(); str s)) l)))
                  | CL.D_Comment[] => ()
                  | CL.D_Comment(com::coms) => (
                      ppCom com;
                      List.app (fn com => (nl(); ppCom com)) coms)
                  | CL.D_Verbatim [] => ()
                  | CL.D_Verbatim l => List.app str l
                  | CL.D_Var(attrs, ty, [], x, optInit) => (
                      inHBox (fn () => (
                        ppAttrs attrs;
                        ppTyAndVar (ty, x);
                        case optInit
                         of SOME init => (sp(); str "="; sp(); ppInit init)
                          | NONE => ()
                        (* end case *);
                        str ";")))
                  | CL.D_Var _ => raise Fail "unexpected scope qualifiers in C code"
                  | CL.D_Proto(attrs, ty, f, params) => (
                      inHBox (fn () => (
                        ppAttrs attrs;
                        ppTyAndVar(ty, f);
                        sp(); str "(";
                        ppCommaList {pp=ppParam, l=params};
                        str ");")))
                  | CL.D_Func(attrs, ty, [], f, params, body) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (
                          ppAttrs attrs;
                          ppTyAndVar(ty, f);
                          sp(); str "(";
                          ppCommaList {pp=ppParam, l=params};
                          str ")"));
                        nl();
                        ppBlock (case body of CL.S_Block stms => stms | stm => [stm]);
                      PP.closeBox strm)
                  | CL.D_Func _ => raise Fail "unexpected scope qualifiers in C code"
                  | CL.D_StructDef(SOME name, fields, NONE) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "struct"; sp(); str name; sp(); str "{"));
                        PP.openVBox strm indent;
                          List.app (fn (ty, x) => (
                              nl();
                              inHBox (fn () => (ppTyAndVar(ty, x); str ";"))))
                            fields;
                        PP.closeBox strm;
                        nl();
                        str "};";
                      PP.closeBox strm)
                  | CL.D_StructDef(optStruct, fields, SOME tyName) => (
                      PP.openVBox strm indent0;
                        str "typedef struct {";
                        PP.openVBox strm indent;
                          List.app (fn (ty, x) => (
                              nl();
                              inHBox (fn () => (ppTyAndVar(ty, x); str ";"))))
                            fields;
                        PP.closeBox strm;
                        nl();
                        inHBox (fn () => (str "}"; sp(); str tyName; str ";"));
                      PP.closeBox strm)
                  | CL.D_StructDef(NONE, _, NONE) => raise Fail "unamed struct"
                  | CL.D_EnumDef{isClass=false, name, repTy=NONE, cons=con::conr} => let
                      fun ppCon (name, NONE) = str name
                        | ppCon (name, SOME e) = inHBox (fn () => (
                            str name; sp(); str "="; sp(); ppExp e))
                      in
                        PP.openHVBox strm indent0;
                          inHBox (fn () => (str "enum"; sp(); str name; sp(); str "{"));
                          PP.openHVBox strm indent;
			    PP.cut strm;
                            ppCon con;
                            List.app (fn c => (str ","; sp(); ppCon c)) conr;
                          PP.closeBox strm;
			  PP.cut strm;
                          str "};";
                        PP.closeBox strm
                      end
		  | CL.D_EnumDef _ => raise Fail "ill-formed enum definition in C code"
                  | CL.D_Constr _ => raise Fail "unexpected constructor decl in C code"
                  | CL.D_Destr _ => raise Fail "unexpected destructor decl in C code"
                  | CL.D_ClassDef _ => raise Fail "unexpected class decl in C code"
                  | CL.D_Typedef(name, ty) =>
                      inHBox (fn () => (str "typedef"; sp(); ppTyAndVar(ty, name); str ";"))
                  | CL.D_Template _ => raise Fail "unexpected template decl in C code"
                  | CL.D_Namespace _ => raise Fail "unexpected namespace decl in C code"
                (* end case *))
          and ppParam (CL.PARAM(attrs, ty, x)) = (
                ppAttrs attrs;
                ppTyAndVar(ty, CL.varToString x))
          and ppInit init = (case init
                 of CL.I_Exp e => ppExp e
                  | CL.I_Exps fields => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn init => (
                            PP.break strm;
                            inHBox (fn () => (ppInit init; str ","))))
                          fields;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Struct fields => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn (lab, init) => (
                            PP.break strm;
                            inHBox (fn () => (
                              str("." ^ lab); sp(); str "="; sp(); ppInit init; str ","))))
                          fields;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Array elems => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn (i, init) => (
                            PP.break strm;
                            inHBox (fn () => (
                              str(concat["[", Int.toString i, "]"]); sp(); str "="; sp();
                              ppInit init; str ","))))
                          elems;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Cons _ => raise Fail "unexpected constructor application in C code"
                (* end case *))
          and ppBlock stms = (
                str "{";
                PP.openVBox strm indent;
                  List.app (fn stm => (nl(); ppStm stm)) stms;
                PP.closeBox strm;
                nl();
                str "}")
          and ppStm stm = (case stm
                 of CL.S_Block stms => ppBlock stms
                  | CL.S_Comment l => List.app ppCom l
                  | CL.S_Verbatim [] => ()
                  | CL.S_Verbatim (stm::stms) => (
                      str stm;
                      List.app (fn stm => (nl(); str stm)) stms)
                  | CL.S_Decl(attrs, ty, x, NONE) => inHBox (fn () => (
                      ppAttrs attrs;
                      ppTyAndVar(ty, x); str ";"))
                  | CL.S_Decl(attrs, ty, x, SOME e) => inHBox (fn () => (
                      ppAttrs attrs;
                      ppTyAndVar(ty, x); sp(); str "="; sp(); ppInit e; str ";"))
                  | CL.S_Exp e => inHBox (fn () => (ppExp e; str ";"))
                  | CL.S_If(e, blk, CL.S_Block[]) =>
                      inHBox (fn () => (str "if"; sp(); ppExp e; ppStmAsBlock blk))
                  | CL.S_If(e, blk1, stm as CL.S_If _) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "if"; sp(); ppExp e; ppStmAsBlock blk1));
                        nl();
                      PP.closeBox strm;
                      inHBox (fn () => (str "else"; sp(); ppStm stm)))
                  | CL.S_If(e, blk1, blk2) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "if"; sp(); ppExp e; ppStmAsBlock blk1));
                        nl();
                        inHBox (fn () => (str "else"; ppStmAsBlock blk2));
                      PP.closeBox strm)
                  | CL.S_Switch(e, cases) => let
                      fun ppCase (labels, stms) = (
                            if List.null labels
                              then (nl(); str "default:")
                              else List.app
                                (fn lab => inHBox(fn () => (
                                    nl(); str "case"; sp(); str lab; str ":")))
                                  labels;
                            PP.openVBox strm indent2;
                              List.app (fn stm => (nl(); ppStm stm)) stms;
                            PP.closeBox strm)
                      in
                        PP.openVBox strm indent0;
                          inHBox (fn () => (str "switch"; sp(); ppExp e; sp(); str "{"));
                          PP.openVBox strm indent2;
                            List.app ppCase cases;
                          PP.closeBox strm;
                          nl (); str "}";
                        PP.closeBox strm
                      end
                  | CL.S_While(e, blk) =>
                      inHBox (fn () => (str "while"; sp(); ppExp e; ppStmAsBlock blk))
                  | CL.S_DoWhile(blk, e) =>
                      inHBox (fn () => (
                        str "do"; ppStmAsBlock blk; sp(); str "while"; sp(); ppExp e))
                  | CL.S_For(ty, dcls, cond, incrs, blk) => inHBox (fn () => (
                      str "for"; sp(); str "(";
                      case dcls
                       of (x, e)::rest => (
                            ppTyAndVar(ty, x); sp(); str "="; sp(); ppExp e;
                            List.app
                              (fn (x, e) => (str ","; sp(); str x; sp(); str "="; sp(); ppExp e))
                                rest)
                        | [] => ()
                      (* end case *);
                      str ";"; sp();
                      ppExp cond; str ";"; sp();
                      ppList {pp = ppExp, sep = fn () => str ",", l = incrs};
                      str ")";
                      ppStmAsBlock blk))
                  | CL.S_Return(SOME e) => inHBox (fn () => (str "return"; sp(); ppExp e; str ";"))
                  | CL.S_Return _ => str "return;"
                  | CL.S_Break => str "break;"
                  | CL.S_Continue => str "continue;"
                  | CL.S_Delete _ => raise Fail "unexpected 'delete' in C code"
                  | CL.S_KernCall _ => raise Fail "unexpected CUDA kernel call in C code"
                (* end case *))
        (* force printing "{" "}" around a statement *)
          and ppStmAsBlock (CL.S_Block stms) = (sp(); ppBlock stms)
            | ppStmAsBlock stm = (sp(); ppBlock [stm])
          and ppExp e = (case e
                 of CL.E_Grp e => (str "("; ppExp e; str ")")
                  | CL.E_AssignOp(lhs, rator, rhs) => (
                      ppExp lhs; sp(); str(CL.assignopToString rator); sp(); ppExp rhs)
                  | CL.E_Cond(e1, e2, e3) => (
                      ppExp e1; sp(); str "?"; sp(); ppExp e2; sp(); str ":"; sp(); ppExp e3)
                  | CL.E_BinOp(e1, rator, e2) => (
                      ppExp e1; sp(); str(CL.binopToString rator); sp(); ppExp e2)
                  | CL.E_UnOp(rator, e) => (str(CL.unopToString rator); ppExp e)
                  | CL.E_PostOp(e, rator) => (ppExp e; str(CL.postopToString rator))
                  | CL.E_Apply(e, args) => (ppExp e; ppArgs args)
                  | CL.E_Subscript(e1, e2) => (ppExp e1; str "["; ppExp e2; str "]")
                  | CL.E_Select(e, f) => (ppExp e; str "."; str f)
                  | CL.E_Indirect(e, f) => (ppExp e; str "->"; str f)
                  | CL.E_Cast(ty, e) => (str "("; ppTy ty; str ")"; ppExp e)
                  | CL.E_Vec(ty, args) => (
                    (* GCC vector syntax: "__extension__ (ty){a, b, ...}" *)
                      str "__extension__"; sp(); str "("; ppTy ty; str ")";
                      str "{";
                      PP.openHOVBox strm indent;
                        PP.cut strm;
                        ppCommaList {
                            pp = fn e => (PP.openHBox strm; ppExp e; PP.closeBox strm),
                            l = args
                          };
                        str "}";
                      PP.closeBox strm)
                  | CL.E_Array _ => raise Fail "unexpected array expression in C code"
                  | CL.E_Var x => str(CL.varToString x)
                  | CL.E_Int(n, CL.T_Num(RawTypes.RT_Int64)) =>
                      str(IntLit.toString n ^ "l")
                  | CL.E_Int(n, _) => str(IntLit.toString n)
                  | CL.E_Flt(f, ty) => let
                      val isDouble = (case ty
                             of CL.T_Num(RawTypes.RT_Float) => false
                              | _ => true
                            (* end case *))
                    (* NOTE: the CLang.mkFlt function guarantees that f is non-negative *)
                      val f = if RealLit.same(RealLit.posInf, f)
                              then if isDouble
                                then "HUGE_VAL"
                                else "HUGE_VALF"
                            else if RealLit.same(RealLit.nan, f)
                              then if isDouble
                                then "nan(\"\")"
                                else "nanf(\"\")"
                            else if isDouble
                              then RealLit.toString f
                              else RealLit.toString f ^ "f"
                      in
                        str f
                      end
                  | CL.E_Bool b => str(Bool.toString b)
                  | CL.E_Str s => str(concat["\"", String.toCString s, "\""])
                  | CL.E_Char c => str(concat["'", Char.toCString c, "'"])
                  | CL.E_Sizeof ty => (str "sizeof("; ppTy ty; str ")")
                  | CL.E_TApply _ => raise Fail "unexpected template application in C code"
                  | CL.E_QId _ => raise Fail "unexpected qualified ID in C code"
                  | CL.E_Cons _ => raise Fail "unexpected constructor application in C code"
                  | CL.E_New _ => raise Fail "unexpected new in C code"
                  | CL.E_XCast(c, _, _) =>
                      raise Fail(concat["unexpected ", c, " application in C code"])
                (* end case *))
          and ppArgs args = (
                str "(";
                PP.openHOVBox strm indent;
                  PP.cut strm;
                  ppCommaList {
                      pp = fn e => (PP.openHBox strm; ppExp e; PP.closeBox strm),
                      l = args
                    };
                  str ")";
                PP.closeBox strm)
          in
            PP.openVBox strm indent0;
              ppDecl decl;
              nl();
            PP.closeBox strm
          end

  end
