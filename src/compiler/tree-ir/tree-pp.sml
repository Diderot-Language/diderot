(* tree-pp.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Printing for the TreeIR
 *)

structure TreePP : sig

    val expToString : TreeIR.exp -> string

    val statement : TextIO.outstream * TreeIR.stm -> unit

    val block : TextIO.outstream * TreeIR.block -> unit

    val program : TextIO.outstream * TreeIR.program -> unit

    val output : TextIO.outstream * string * TreeIR.program -> unit

  end = struct

    structure IR = TreeIR
    structure Ty = TreeTypes
    structure Op = TreeOps
    structure Var = TreeVar
    structure GVar = TreeGlobalVar
    structure SVar = TreeStateVar

    fun indent (outS, i) = TextIO.output(outS, StringCvt.padLeft #" " i "")
    fun incIndent (outS, i) = (outS, i+2)
    fun decIndent (outS, i) = (outS, Int.max(0, i-2))
    fun pr ((outS, _), s) = TextIO.output(outS, s)
    fun prl (out, l) = pr(out, concat l)
    fun prln (out, l) = (indent out; prl(out, l))

    fun descToString NONE = ""
      | descToString (SOME desc) = String.toString desc

    fun expToString e = let
          fun argsToS (lp, args, rp, l) = let
                fun argToS ([], l) = l
                  | argToS ([e], l) = toS (e, l)
                  | argToS (e::es, l) = toS(e, "," :: argToS(es, l))
                in
                  lp :: argToS(args, rp :: l)
                end
          and toS (IR.E_Global x, l) = GVar.toString x :: l
            | toS (IR.E_State(NONE, x), l) = "self." :: SVar.toString x :: l
            | toS (IR.E_State(SOME e, fld), l) = toS (e, "." :: SVar.toString fld :: l)
            | toS (IR.E_Var x, l) = Var.name x :: l
            | toS (IR.E_Lit lit, l) = Literal.toString lit :: l
            | toS (IR.E_Op(rator, args), l) = Op.toString rator :: argsToS ("(", args, ")", l)
            | toS (IR.E_Apply(f, args), l) = TreeFunc.toString f :: argsToS ("(", args, ")", l)
            | toS (IR.E_Vec(w, pw, args), l) = let
                val args = argsToS ("(", args, ")", l)
                in
                  if (w = pw)
                    then "VEC" :: Int.toString w :: args
                    else "VEC" :: Int.toString w :: "{" :: Int.toString pw :: "}" :: args
                end
            | toS (IR.E_Cons(args, ty), l) =
                "<" :: Ty.toString ty :: ">" :: argsToS ("[", args, "]", l)
            | toS (IR.E_Seq(args, ty), l) =
                "<" :: Ty.toString ty :: ">" :: argsToS ("{", args, "}", l)
            | toS (IR.E_Pack(_, es), l) = "PACK" :: argsToS ("(", es, ")", l)
            | toS (IR.E_VLoad(layout, e, i), l) =
                "LOAD_"  :: Int.toString i :: "(" :: toS(e, ")" :: l)
          in
            String.concat (toS (e, []))
          end

    fun argsToString (prefix, es) = String.concat[
            prefix, "(", String.concatWith "," (List.map expToString es), ")"
          ]

    fun ppVarDecl out x = prln (out, [Ty.toString(Var.ty x), " ", Var.name x, ";\n"])

    fun ppParams (out, params) = let
          fun pp [] = ()
            | pp [x] = prl(out, [Ty.toString (Var.ty x), " ", Var.name x])
            | pp (x::r) = (
                prl(out, [Ty.toString (Var.ty x), " ", Var.name x, ","]);
                pp r)
          in
            pp params
          end

    fun ppStrand out strand = let
          val IR.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          val out' = incIndent out
          fun ppSVarDecl sv = let
                val v = if SVar.isVarying sv then "varying " else ""
                val out = if SVar.isOutput sv then "output " else ""
                in
                  prln (out', [v, out, Ty.toString(SVar.ty sv), " ", SVar.name sv, ";\n"])
                end
          in
            prln (out, ["strand ", Atom.toString name, " ("]);
            ppParams(out, params); pr(out, ")");
            case spatialDim
             of SOME d => prl(out, [" in ", Int.toString d, "D"])
              | NONE => ()
            (* end case *);
            pr (out, " {\n");
            List.app ppSVarDecl state;
            ppMethod (out', "initialize", SOME stateInit);
            ppMethod (out', "start", startM);
            ppMethod (out', "update", SOME updateM);
            ppMethod (out', "stabilize", stabilizeM);
            prln (out, ["}\n"])
          end

    and ppMethod (out, name, SOME(IR.Method{needsW, hasG, body})) = let
          val params = (case (needsW, hasG)
                 of (false, false) => " () "
                  | (false, true) => " (globals) "
                  | (true, false) => " (world) "
                  | (true, true) => " (world, globals) "
                (* end case *))
          in
            prln (out, [name, params]);
            ppBlock (out, body);
            pr (out, "\n")
          end
      | ppMethod (_, _, NONE) = ()

    and ppBlock (out, IR.Block{locals, body}) = let
          val out' = incIndent out
          in
            pr (out, "{\n");
            List.app (ppVarDecl out') (!locals);
            List.app (fn stm => ppStm(out', stm)) body;
            indent out; pr (out, "}")
          end

    and ppStm (out, stm) = (case stm
           of IR.S_Comment text => let
                val out = decIndent out
                in
                  List.app (fn s => prln(out, ["// ", s, "\n"])) text
                end
            | IR.S_Assign(true, x, e) =>
                prln(out, [Ty.toString(Var.ty x), " ", Var.name x, " = ", expToString e, ";\n"])
            | IR.S_Assign(false, x, e) => prln(out, [Var.name x, " = ", expToString e, ";\n"])
            | IR.S_MAssign([], e) => prln(out, [expToString e, ";\n"])
            | IR.S_MAssign([x], e) => prln(out, [Var.name x, " = ", expToString e, ";\n"])
            | IR.S_MAssign(x::xs, e) => (
                prln(out, ["(", Var.name x]);
                List.app (fn x => prl(out, [",", Var.name x])) xs;
                prl (out, [") = ", expToString e, ";\n"]))
            | IR.S_GAssign(x, e) => prln(out, [GVar.toString x, " = ", expToString e, ";\n"])
            | IR.S_IfThen(cond, blk) => (
                prln (out, ["if (", expToString cond, ") "]);
                ppBlock (out, blk);
                pr (out, "\n"))
            | IR.S_IfThenElse(cond, blk1, blk2) => (
                prln (out, ["if (", expToString cond, ") "]);
                ppBlock (out, blk1);
                pr (out, " else ");
                ppBlock (out, blk2);
                pr (out, "\n"))
            | IR.S_For(x, e1, e2, blk) => (
                prln (out, [
                    "for ", Var.name x, " = ", expToString e1, " to ", expToString e2, " "
                  ]);
                ppBlock (out, blk);
                pr (out, "\n"))
            | IR.S_Foreach(x, e, blk) => (
                prln (out, [
                    "foreach ", Ty.toString(Var.ty x), " ", Var.name x, " in ", expToString e, " "
                  ]);
                ppBlock (out, blk);
                pr (out, "\n"))
            | IR.S_MapReduce(mrs, src) => let
                val out' = incIndent out
                fun prMR (IR.MapReduce(x, r, f, args, set)) = prln (out', [
                        Ty.toString(Var.ty x), " ", Var.name x, " = ", Reductions.toString r,
                        " { ", TreeFunc.toString f, " ", argsToString (" ", args),
                        " | ", Var.name src, " in ", StrandSets.toString set, " }\n"
                      ])
                in
                  prln (out, ["mapreduce {\n"]);
                  List.app prMR mrs;
                  prln (out, ["}\n"])
                end
            | IR.S_LoadNrrd(x, ty, nrrd, proxy) =>
                prln (out, [
                    Var.name x, " = load<", APITypes.toString ty, "> (\"",
                    String.toString nrrd, "\");\n"
                  ])
            | IR.S_Input(x, name, desc, NONE) =>
                prln (out, [
                    GVar.name x, " = input<", Ty.toString(GVar.ty x), "> (\"",
                    String.toString name, "\",\"", descToString desc, "\");\n"
                  ])
            | IR.S_Input(x, name, desc, SOME dflt) =>
                prln (out, [
                    GVar.name x, " = input<", Ty.toString(GVar.ty x), "> (\"",
                    String.toString name, "\",\"", descToString desc, "\",",
                    expToString dflt, ");\n"
                  ])
            | IR.S_InputNrrd(x, name, desc, NONE) =>
                prln (out, [
                    GVar.name x, " = input-nrrd<", Ty.toString(GVar.ty x), "> (\"",
                    String.toString name, "\",\"", descToString desc, "\");\n"
                  ])
            | IR.S_InputNrrd(x, name, desc, SOME(dflt, proxy)) =>
                prln (out, [
                    GVar.name x, " = input-nrrd<", Ty.toString(GVar.ty x), "> (\"",
                    String.toString name, "\",\"", descToString desc, "\",\"",
                    String.toString dflt, "\");\n"
                  ])
            | IR.S_New(strand, args) =>
                prln (out, [argsToString("new "^Atom.toString strand, args), ";\n"])
            | IR.S_Save(x, rhs) =>
                prln (out, ["self.", SVar.toString x, " = ", expToString rhs, ";\n"])
            | IR.S_KillAll => prln (out, ["kill_all;\n"])
            | IR.S_StabilizeAll => prln (out, ["stabilize_all;\n"])
            | IR.S_Return NONE => prln (out, ["return\n"])
            | IR.S_Return(SOME e) => prln (out, ["return ", expToString e, "\n"])
            | IR.S_Print(_, es) => prln (out, [argsToString("print", es), ";\n"])
          (* return functions for methods *)
            | IR.S_Active => prln (out, ["active;\n"])
            | IR.S_Stabilize => prln (out, ["stabilize;\n"])
            | IR.S_Die => prln (out, ["die;\n"])
          (* end case *))

    fun statement (outS, stm) = ppStm((outS, 0), stm)

    fun block (outS, blk) = (ppBlock ((outS, 0), blk); pr ((outS, 0), "\n"))

    fun program (outS, prog) = let
          val IR.Program{
                props, target, consts, inputs, constInit,
                globals, funcs, globInit, strand,
                create=Create.Create{dim, code}, start, update
              } = prog
          val out = (outS, 0)
          val out' = incIndent out
          fun ppInputDecl (Inputs.INP{var, name, init, ...}) = prl (out, [
                  "input ", Ty.toString(GVar.ty var), " ", GVar.name var, ";\n"
                ])
          fun ppGlobalDecl x = let
                val ln = ["global ", Ty.toString(GVar.ty x), " ", GVar.name x, ";\n"]
                val ln = if GVar.isVarying x then "varying " :: ln else ln
                in
                  prln (out, ln)
                end
          fun ppFuncDef (IR.Func{name, params, body}) = let
                val (resTy, paramTys) = TreeFunc.ty name
                in
                  prln(out', ["function ", Ty.toString resTy, " ", TreeFunc.name name, " ("]);
                  case (TreeFunc.needsWorld name, TreeFunc.hasGlobals name)
                   of (false, false) => ()
                    | (false, true) => pr(out, if null params then "globals" else "globals, ")
                    | (true, false) => pr(out, if null params then "world" else "world, ")
                    | (true, true) =>
                        pr(out, if null params then "world, globals" else "world, globals, ")
                  (* end case *);
                  ppParams(out, params); pr(out, ") ");
                  ppBlock(out', body); pr(out, "\n")
                end
          in
            pr(out, "//***** PROPERTIES *****\n");
            case props
             of [] => prln(out', ["none\n"])
              | _ => prln(out', [String.concatWith " " (List.map Properties.toString props), "\n"])
            (* end case *);
            prln(out, ["//***** CONSTS *****\n"]);
            List.app ppGlobalDecl consts;
            prln(out, ["//***** INPUTS *****\n"]);
            List.app ppInputDecl inputs;
            prln(out, ["//***** CONST INIT *****\n"]);
            indent out'; ppBlock (out', constInit); pr (out, "\n");
            prln(out, ["//***** GLOBALS *****\n"]);
            List.app ppGlobalDecl globals;
            prln(out, ["//***** FUNCTIONS *****\n"]);
            List.app ppFuncDef funcs;
            prln(out, ["//***** GLOBAL VARIABLE INIT *****\n"]);
            indent out'; ppBlock (out', globInit); pr (out, "\n");
            prln(out, ["//***** STRAND *****\n"]);
            ppStrand out strand;
            case dim
             of SOME d => prln(out, ["//***** CREATE GRID(", Int.toString d, ") *****\n"])
              | NONE => prln(out, ["//***** CREATE COLLECTION *****\n"])
            (* end case *);
            indent out'; ppBlock (out', code); pr (out, "\n");
            case start
             of SOME blk => (
                  prln(out, ["//***** GLOBAL START *****\n"]);
                  indent out'; ppBlock (out', blk); pr (out, "\n"))
              | NONE => ()
            (* end case *);
            case update
             of SOME blk => (
                  prln(out, ["//***** GLOBAL UPDATE *****\n"]);
                  indent out'; ppBlock (out', blk); pr (out, "\n"))
              | NONE => ()
            (* end case *)
          end

    fun output (outS, msg, prog) = (
          pr ((outS, 0), concat["##### TreeIR: ", msg, " ####\n"]);
          program (outS, prog);
          pr ((outS, 0), "#### end program ####\n"))

  end
