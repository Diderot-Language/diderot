(* ast-pp.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Pretty printing for the AST representation.
 *)

structure ASTPP : sig

    val output : TextIO.outstream * string * AST.program -> unit

  end = struct

    structure PP = TextIOPP
    structure TU = TypeUtil

    val indent = PP.Abs 2

    fun ppList ppFn (left, sep, right) (ppStrm, list) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          fun pp [] = string right
            | pp [x] = (ppFn(ppStrm, x); string right)
            | pp (x::xs) = (ppFn(ppStrm, x); string sep; sp(); pp xs)
          in
            string left; pp list
          end

  (* print type arguments; we use "#" to denote differentiation arguments, "$" to denote
   * shape arguments, and "%" to denote dimension arguments.
   *)
    fun ppTyArgs (ppStrm, mvs) = let
          val string = PP.string ppStrm
          fun ppTyArg (_, mv) = (case mv
                 of Types.TYPE tv => string(TU.toString(TU.resolve tv))
                  | Types.DIFF dv => string("#"^TU.diffToString(TU.resolveDiff dv))
                  | Types.SHAPE sv => string("$"^TU.shapeToString(TU.resolveShape sv))
                  | Types.DIM dv => string("%"^TU.dimToString(TU.resolveDim dv))
                (* end case *))
          in
            ppList ppTyArg ("<", ";", ">") (ppStrm, mvs)
          end

    fun ppExp (ppStrm, e) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          fun var x = string(Var.nameOf x)
          fun ppIndex (ppStrm, NONE) = PP.string ppStrm ":"
            | ppIndex (ppStrm, SOME e) = ppExp (ppStrm, e)
          fun pp e = (case e
                 of AST.E_Var(x, _) => var x
                  | AST.E_Lit lit => string (Literal.toString lit)
                  | AST.E_Kernel k => string (Kernel.name k)
                  | AST.E_Select(e, (field, _)) => (pp e; string "."; var field)
                  | AST.E_Prim(f, [], args, _) => (var f; sp(); ppArgs (ppStrm, args))
                  | AST.E_Prim(f, mvs, args, _) => (
                      var f; ppTyArgs (ppStrm, mvs); sp(); ppArgs (ppStrm, args))
                  | AST.E_Apply((f, _), args, _) => (var f; sp(); ppArgs (ppStrm, args))
                  | AST.E_Comprehension(e, iter, _) => (
                      string "{";
                        pp e; sp(); string "|"; sp();
                        ppIter (ppStrm, iter);
                      string "}")
                  | AST.E_ParallelMap(e, x, xs, _)=> (
                      string "{|";
                        pp e; sp(); string "|"; sp();
                        PP.openHBox ppStrm;
                          string(TU.toString(#2(Var.typeOf x))); sp(); var x;
                          sp(); string "in"; sp(); var xs;
                        PP.closeBox ppStrm;
                      string "|}")
                  | AST.E_Tensor(es, _) => (
                      ppList ppExp ("[", ",", "]") (ppStrm, es))
                  | AST.E_Seq(es, _) => (
                      ppList ppExp ("{", ",", "}") (ppStrm, es))
                  | AST.E_Slice(e, indices, _) => (
                      pp e;
                      ppList ppIndex ("[", ",", "]") (ppStrm, indices))
                  | AST.E_Cond(e1, e2, e3, _) => (
                      string "("; pp e2; sp(); string "if"; sp(); pp e1; sp(); string "else";
                      sp(); pp e3; string ")")
                  | AST.E_Orelse(e1, e2) => (
                      string "("; pp e1; sp(); string "||"; sp(); pp e2; string ")")
                  | AST.E_Andalso(e1, e2) => (
                      string "("; pp e1; sp(); string "&&"; sp(); pp e2; string ")")
                  | AST.E_LoadNrrd(mvs, name, ty) => (
                      case TU.pruneHead ty
                       of Types.T_Sequence _ => string "loadSeq"
                        | Types.T_Image _ => string "loadImage"
                        | _ => raise Fail "impossible"
                      (* end case *);
                      ppTyArgs (ppStrm, mvs); sp();
                      string(concat["(\"", name, "\")"]))
                  | AST.E_Coerce{dstTy, e, ...} => (
                      PP.openHBox ppStrm;
                        string "("; string(TU.toString dstTy); string ")";
                      PP.closeBox ppStrm;
                      case e
                       of AST.E_Var _ => pp e
                        | AST.E_Lit _ => pp e
                        | AST.E_Tensor _ => pp e
                        | AST.E_Seq _ => pp e
                        | _ => (string "("; pp e; string ")")
                      (* end case *))
                (* end case *))
          in
            pp e
          end

    and ppArgs (ppStrm, args) = ppList ppExp ("(", ",", ")") (ppStrm, args)

    and ppIter (ppStrm, (x, e)) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          in
            PP.openHBox ppStrm;
              string(TU.toString(#2(Var.typeOf x))); sp(); string(Var.nameOf x);
              sp(); string "in"; sp(); ppExp(ppStrm, e);
            PP.closeBox ppStrm
          end

    fun ppVarDecl ppStrm (x, e) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          in
            PP.openHBox ppStrm;
              case Var.kindOf x
               of AST.ConstVar => (string "const"; sp())
                | AST.InputVar => (string "input"; sp())
                | AST.StrandOutputVar => (string "output"; sp())
                | _ => ()
              (* end case *);
              string(TU.toString(#2(Var.typeOf x))); sp(); string(Var.nameOf x);
              case e
               of SOME e => (sp(); string "="; sp(); ppExp(ppStrm, e); string ";")
                | NONE => ()
              (* end case *);
            PP.closeBox ppStrm
          end

    fun ppBlock (ppStrm, stms) = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          fun ppStmt stmt = (case stmt
                 of AST.S_Block stms => ppBlock (ppStrm, stms)
                  | AST.S_Decl vdcl => (ppVarDecl ppStrm vdcl; nl())
                  | AST.S_IfThenElse(e, AST.S_Block stms, AST.S_Block[]) => (
                      PP.openHBox ppStrm;
                        string "if"; sp(); ppExp(ppStrm, e);
                        sp(); ppBlock (ppStrm, stms);
                      PP.closeBox ppStrm)
                  | AST.S_IfThenElse(e, s1, AST.S_Block[]) => (
                      PP.openVBox ppStrm indent;
                        PP.openHBox ppStrm;
                          string "if"; sp(); ppExp(ppStrm, e);
                        PP.closeBox ppStrm;
                        nl();
                        ppStmt s1;
                      PP.closeBox ppStrm)
                  | AST.S_IfThenElse(e, s1, s2) => (
                      PP.openHBox ppStrm;
                        string "if"; sp(); ppExp(ppStrm, e);
                        sp(); ppBlockStmt (ppStrm, s1);
                      PP.closeBox ppStrm;
                      PP.openHBox ppStrm;
                        string "else"; sp(); ppBlockStmt (ppStrm, s2);
                      PP.closeBox ppStrm)
                  | AST.S_Foreach((x, e), s) => (
                      PP.openHBox ppStrm;
                        string "foreach"; sp(); string "(";
                        ppIter (ppStrm, (x, e));
                        string ")"; sp();
                        ppBlockStmt (ppStrm, s);
                      PP.closeBox ppStrm)
                  | AST.S_Assign((x, _), e) => (
                      PP.openHBox ppStrm;
                        string(Var.nameOf x); sp(); string "="; sp(); ppExp(ppStrm, e); string ";";
                      PP.closeBox ppStrm;
                      nl())
                  | AST.S_New(strand, args) => (
                      PP.openHBox ppStrm;
                        string "new"; sp(); string(Atom.toString strand); sp();
                        ppArgs (ppStrm, args); string ";";
                      PP.closeBox ppStrm;
                      nl())
                  | AST.S_KillAll => string "kill_all;"
                  | AST.S_StabilizeAll => string "stabilize_all;"
                  | AST.S_Continue => (string "continue;"; nl())
                  | AST.S_Die => (string "die;"; nl())
                  | AST.S_Stabilize => (string "stabilize;"; nl())
                  | AST.S_Return e => (
                      PP.openHBox ppStrm;
                        string "return"; sp(); ppExp(ppStrm, e); string ";";
                      PP.closeBox ppStrm;
                      nl())
                  | AST.S_Print args => (
                      PP.openHBox ppStrm;
                        string "print"; sp(); ppArgs (ppStrm, args); string ";";
                      PP.closeBox ppStrm;
                      nl())
                (* end case *))
          in
            PP.openVBox ppStrm (PP.Abs 0);
              string "{";
              PP.openVBox ppStrm indent;
                nl();
                List.app ppStmt stms;
              PP.closeBox ppStrm;
              string "}";
            PP.closeBox ppStrm;
            nl()
          end

    and ppBlockStmt (ppStrm, AST.S_Block stms) = ppBlock (ppStrm, stms)
      | ppBlockStmt (ppStrm, stm) = ppBlock (ppStrm, [stm])

    fun ppParams (ppStrm, params) = let
          fun sp () = PP.space ppStrm 1
          val string = PP.string ppStrm
          in
            ppList
              (fn (_, x) => (string(TU.toString(#2(Var.typeOf x))); sp(); string(Var.nameOf x)))
              ("(", ",", ")")
              (ppStrm, params)
          end

    fun ppStrand (ppStrm, strand) = let
          val AST.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
(*
          fun ppMethod name (AST.S_Block stms) = (nl(); string name; nl(); ppBlock (ppStrm, stms))
            | ppMethod name stm = (nl(); string name; nl(); ppBlock (ppStrm, [stm]))
*)
          fun ppMethod name stm = (
                PP.openHBox ppStrm;
                  string name; sp();
                  ppBlockStmt (ppStrm, stm);
                PP.closeBox ppStrm)
          in
            PP.openHBox ppStrm;
              string "strand"; sp(); string(Atom.toString name); sp();
              ppParams (ppStrm, params);
              case spatialDim
               of SOME d => (sp(); string "in"; sp(); string(Int.toString d ^ "D"))
                | NONE => ()
              (* end case *);
            PP.closeBox ppStrm;
            nl();
            PP.openVBox ppStrm indent;
              string "{";
              List.app (fn vdcl => (nl(); ppVarDecl ppStrm vdcl)) state;
              nl();
              Option.app (fn stm => (
                  PP.openHBox ppStrm;
                    string "stateInit"; sp();
                    ppBlockStmt (ppStrm, stm);
                  PP.closeBox ppStrm))
                stateInit;
              Option.app (ppMethod "start") startM;
              ppMethod "update" updateM;
              Option.app (ppMethod "stabilize") stabilizeM;
            PP.closeBox ppStrm;
            nl();
            string "}";  nl()
          end

    fun ppInput ppStrm = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          fun var x = string(Var.nameOf x)
          fun ppDesc NONE = ()
            | ppDesc (SOME desc) = (
                string(concat["(\"", String.toString desc, "\")"]); sp())
          in
            fn ((x, SOME e), desc) => (
                PP.openHBox ppStrm;
                  string "input"; sp();
                  ppDesc desc;
                  string(TU.toString(#2(Var.typeOf x))); sp(); var x;
                  sp(); string "="; sp(); ppExp(ppStrm, e); string ";";
                PP.closeBox ppStrm;
                nl())
             | ((x, NONE), desc) => (
                PP.openHBox ppStrm;
                  string "input"; sp();
                  ppDesc desc;
                  string(TU.toString(#2(Var.typeOf x))); sp(); var x; string ";";
                PP.closeBox ppStrm;
                nl())
          end

    fun ppDecl ppStrm = let
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          fun var x = string(Var.nameOf x)
          fun ppDesc NONE = ()
            | ppDesc (SOME desc) = (
                string(concat["(\"", String.toString desc, "\")"]); sp())
          in
            fn AST.D_Var vdcl => (ppVarDecl ppStrm vdcl; nl())
             | AST.D_Func(f, params, body) => (
                PP.openHBox ppStrm;
                  string "function"; sp();
                  string(TU.toString(TU.rngOf(Var.monoTypeOf f)));
                  sp(); var f; sp(); ppParams (ppStrm, params);
                PP.closeBox ppStrm;
                nl();
                case body
                 of AST.S_Block stms => ppBlock (ppStrm, stms)
                  | stm => ppBlock (ppStrm, [stm])
                (* end case *))
             | AST.D_DiffFunc(f, params, body) => (
                PP.openHBox ppStrm;
                  string(TU.toString(Var.monoTypeOf f));
                  sp(); var f; sp(); ppParams (ppStrm, params);
                  sp(); string "="; sp(); ppExp(ppStrm, body); string ";";
                PP.closeBox ppStrm;
                nl())
          end

    fun ppCreate (ppStrm, cr) = (
          PP.openVBox ppStrm (PP.Abs 0);
            PP.openVBox ppStrm indent;
              case cr
               of Create.Create{dim=SOME d, code} => (
                    PP.string ppStrm (concat["grid(", Int.toString d, ") "]);
                    ppBlockStmt (ppStrm, code))
                | Create.Create{code, ...} => (
                    PP.string ppStrm "collection ";
                    ppBlockStmt (ppStrm, code))
              (* end case *);
            PP.closeBox ppStrm;
            PP.newline ppStrm;
          PP.closeBox ppStrm)

    fun output (outS, message, prog) = let
          val AST.Program{
                  props, const_dcls, input_dcls, globals,
                  globInit, strand, create, start, update
                } = prog
          val ppStrm = PP.openOut {dst = outS, wid = 120}
          fun sp () = PP.space ppStrm 1
          fun nl () = PP.newline ppStrm
          val string = PP.string ppStrm
          fun ppTopBlock (prefix, SOME stm) = (
                PP.openHBox ppStrm;
                  string prefix; sp();
                  ppBlockStmt (ppStrm, stm);
                PP.closeBox ppStrm)
            | ppTopBlock _ = ()
          in
            PP.openVBox ppStrm (PP.Abs 0);
              string (concat ["/* AST: ", message, " */"]); nl();
              PP.openHBox ppStrm;
                PP.string ppStrm "properties:";
                sp();
                string (Properties.propsToString props);
                nl();
              PP.closeBox ppStrm;
              List.app (fn dcl => (ppVarDecl ppStrm dcl; nl())) const_dcls;
              List.app (ppInput ppStrm) input_dcls;
              List.app (ppDecl ppStrm) globals;
              ppTopBlock ("initialize", globInit);
              ppStrand (ppStrm, strand);
              ppCreate (ppStrm, create);
              ppTopBlock ("start", start);
              ppTopBlock ("update", update);
              string "/* Program end */"; nl();
            PP.closeBox ppStrm;
            PP.closeStream ppStrm
          end

  end
