(* ssa-pp-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Pretty printing for SSA representations
 *)

functor SSAPPFn (IR : SSA) : sig

    val assignToString : IR.assign -> string

    val output : TextIO.outstream * string * IR.program -> unit

    val outputCFG : TextIO.outstream * IR.cfg -> unit

  end = struct

    structure Op = IR.Op
    structure Var = IR.Var
    structure GVar = IR.GlobalVar
    structure Ty = IR.Ty

    local
      val {getFn, setFn} = IR.Node.newFlag()
    in
    val isMarked = getFn
    fun mark nd = setFn(nd, true)
    fun clear nd = setFn(nd, false)
    end

  (* if true, the output is in compact form *)
    val compact = Ctl.compactCFG

    fun indent (outS, i) = TextIO.output(outS, StringCvt.padLeft #" " i "")
    fun incIndent (outS, i) = (outS, i+2)
    fun pr ((outS, _), s) = TextIO.output(outS, s)
    fun prl (out, l) = pr(out, concat l)
    fun prln (out, l) = (indent out; prl(out, l))

    fun typedVar x = String.concat [
            Ty.toString(Var.ty x), " ", Var.toString x, "#", Int.toString(Var.useCount x)
          ]

    fun rhsToStringList rhs = (case rhs
           of IR.GLOBAL x => [GVar.toString x, ";"]
            | IR.STATE(NONE, x) => ["self.", IR.StateVar.toString x, ";"]
            | IR.STATE(SOME x, y) => [IR.Var.toString x, ".", IR.StateVar.toString y, ";"]
            | IR.VAR x => [Var.toString x, ";"]
            | IR.LIT lit => [Literal.toString lit, ";"]
            | IR.OP(rator, []) => [Op.toString rator, ";"]
            | IR.OP(rator, args) => [
                  Op.toString rator, "(",
                  String.concatWithMap "," Var.toString args, ");"
                ]
            | IR.CONS(xs, ty) => [
                  "<", Ty.toString ty, ">[",
                  String.concatWithMap "," Var.toString xs, "];"
                ]
            | IR.SEQ(xs, ty) => [
                  "<", Ty.toString ty, ">{",
                  String.concatWithMap "," Var.toString xs, "};"
                ]
            | IR.EINAPP(ein, args) => [
                  EinPP.toString ein, " (",
                  String.concatWithMap "," Var.toString args, ");"
                ]
            | IR.APPLY(f, args) => [
                  IR.Func.toString f, " (",
                  String.concatWithMap "," Var.toString args, ");"
                ]
            | IR.MAPREDUCE[(r, f, args)] => [
                    Reductions.toString r, "(MAP ", IR.Func.toString f, " (",
                      String.concatWithMap "," Var.toString args, "));"
                  ]
            | IR.MAPREDUCE mrs => let
                fun toS (r, f, args) = String.concat [
                        Reductions.toString r, "(MAP ", IR.Func.toString f, " (",
                        String.concatWithMap "," Var.toString args, "))"
                      ]
                in
                  ["{ ", String.concatWithMap " | " toS mrs, " };"]
                end
          (* end case *))

    fun assignToString (y, rhs) = String.concat(typedVar y :: " = " :: rhsToStringList rhs)

    fun massignToString ([], rhs) = String.concat(rhsToStringList rhs)
      | massignToString (ys, rhs) = String.concat(
          "(" :: String.concatWithMap "," typedVar ys :: ") = " :: rhsToStringList rhs)

    fun labelOf (IR.ND{id, ...}) = "L"^Stamp.toString id

    fun ppCFG (out, cfg as IR.CFG{entry, exit}) = let
          fun goto (out, nd) = (case IR.Node.kind nd
                 of IR.JOIN _ => (
                      prln(incIndent out, ["goto ", IR.Node.toString nd, "\n"]);
                      ppNd (out, false, nd))
                  | _ => ppNd (out, true, nd)
                (* end case *))
          and ppNd (out, noLabel, nd) = let
                val out1 = incIndent out
                fun prPhi out (y, xs) =
                      prln (out, [
                          typedVar y, " = phi(",
                          String.concatWith "," (List.mapPartial (Option.map Var.toString) xs), ")\n"
                        ])
                fun prJoin (nd, preds, mask) = let
                      val preds = ListPair.mapEq
                            (fn (false, nd) => IR.Node.toString nd
                              | (true, nd) => "*" ^ IR.Node.toString nd)
                            (mask, preds)
(* +DEBUG*)
handle ex => (
print(concat["**** Broken CFG at ", IR.Node.toString nd, "\n"]);
List.map IR.Node.toString preds)
(* -DEBUG*)
                      in
                        prln (out, [
                            IR.Node.toString nd, ":  preds = [",
                            String.concatWith "," preds, "]\n"
                          ])
                      end
                val kind = IR.Node.kind nd
                val noLabel = (case kind of IR.FOREACH _ => false | _ => noLabel)
                in
                  if isMarked nd
                    then ()
                    else (
                      mark nd;
                      if noLabel andalso (! compact)
                        then ()
                        else (case kind
                           of IR.JOIN{preds, mask, ...} => prJoin (nd, !preds, !mask)
                            | IR.FOREACH{pred, bodyExit, mask, ...} =>
                                prJoin (nd, [!pred, !bodyExit], !mask)
                            | _ => prln (out, [
                                  IR.Node.toString nd, ":  preds = [",
                                  String.concatWithMap "," IR.Node.toString (IR.Node.preds nd),
                                  "]\n"
                                ])
                          (* end case *));
                      case kind
                       of IR.NULL => ()
                        | IR.ENTRY{succ} => goto (out, !succ)
                        | IR.JOIN{phis, succ, ...} => (
                            List.app (prPhi out1) (!phis);
                            goto (out, !succ))
                        | IR.COND{cond, trueBranch, falseBranch, ...} => (
                            prln (out1, [
                                "if ", Var.toString (!cond),
                                " then goto ", IR.Node.toString(!trueBranch),
                                " else goto ", IR.Node.toString(!falseBranch), "\n"
                              ]);
                            ppNd (out, false, !trueBranch);
                            ppNd (out, false, !falseBranch))
                        | IR.FOREACH{phis, var, src, bodyEntry, succ, ...} => (
                            prln (out1, [
                                "foreach (", Var.toString var, " in ", Var.toString (!src), ")",
                                " on exit goto ", IR.Node.toString(!succ), "\n"
                              ]);
                            List.app (prPhi (incIndent out1)) (!phis);
                            goto (out, !bodyEntry);
                            ppNd (out, false, !succ))
                        | IR.NEXT{succ, ...} =>
                            prln (out1, ["goto ", IR.Node.toString(!succ), "\n"])
                        | IR.COM{text, succ, ...} => (
                            List.app (fn s => prln (out1, ["//", s, "\n"])) text;
                            goto (out, !succ))
                        | IR.ASSIGN{stm, succ, ...} => (
                            prln (out1, [assignToString stm, "\n"]);
                            goto (out, !succ))
                        | IR.MASSIGN{stm, succ, ...} => (
                            prln (out1, [massignToString stm, "\n"]);
                            goto (out, !succ))
                        | IR.GASSIGN{lhs, rhs, succ, ...} => (
                            prln (out1, [
                                GVar.toString lhs, " = ", Var.toString rhs, ";\n"
                              ]);
                            goto (out, !succ))
                        | IR.NEW{strand, args, succ, ...} => (
                            prln (out1, [
                                "new ", Atom.toString strand, "(",
                                String.concatWithMap "," Var.toString args, ");\n"
                              ]);
                            goto (out, !succ))
                        | IR.SAVE{lhs, rhs, succ, ...} => (
                            prln (out1, [
                                "self.", IR.StateVar.toString lhs, " = ", Var.toString rhs, ";\n"
                              ]);
                            goto (out, !succ))
                        | IR.EXIT{kind, succ, ...} => (
                            case kind
                             of ExitKind.RETURN NONE => prln (out1, ["return\n"])
                              | ExitKind.RETURN(SOME x) =>
                                  prln (out1, ["return ", Var.toString x, "\n"])
                              | ExitKind.ACTIVE => prln (out1, ["active\n"])
                              | ExitKind.STABILIZE => prln (out1, ["stabilize\n"])
                              | ExitKind.DIE => prln (out1, ["die\n"])
                              | ExitKind.NEXTSTEP => prln (out1, ["continue\n"])
                              | ExitKind.UNREACHABLE => prln (out1, ["unreachable\n"])
                            (* end case *);
                            case !succ
                             of SOME nd => goto (out, nd)
                              | _ => ()
                            (* end case *))
                      (* end case *))
                end
          in
            ppNd (out, false, entry);
          (* clear marks *)
            IR.CFG.apply clear cfg
          end

    fun ppFunc out (IR.Func{name, params, body}) = (
          prl (out, [
              "  function ", Ty.toString(#1(IR.Func.ty name)), " ", IR.Func.toString name,
              " (", String.concatWithMap ", " typedVar params, ") {\n"
            ]);
          ppCFG (incIndent out, body);
          prl (out, ["}\n"]))

    fun ppMethod (out, name) body = let
          val out1 = incIndent out
          in
            prln(out, ["method ", name, "\n"]);
            ppCFG (incIndent out1, body);
            prln(out, ["end ", name, "\n"])
          end

    and ppStrand (out, strand) = let
          val IR.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          val out1 = incIndent out
          val out2 = incIndent out1
          fun stateVar x = let
                val l = [Ty.toString(IR.StateVar.ty x), " ", IR.StateVar.toString x, ";\n"]
                val l = if IR.StateVar.isOutput x then "output " :: l else l
                in
                  prln (out2, l)
                end
          in
            indent out;
            prl(out, [
                "strand ", Atom.toString name, " (",
                String.concatWithMap ", " typedVar params, ")"
              ]);
            case spatialDim
             of SOME d => prl(out, [" in ", Int.toString d, "D\n"])
              | NONE => pr(out, "\n")
            (* end case *);
            indent out1; pr(out1, "state:\n"); List.app stateVar state;
            prln(out1, ["state init\n"]);
            ppCFG (out2, stateInit);
            prln(out1, ["end state init\n"]);
            Option.app (ppMethod (out1, "start")) startM;
            ppMethod (out1, "update") updateM;
            Option.app (ppMethod (out1, "stabilize")) stabilizeM;
            prln(out, ["end ", Atom.toString name, "\n"])
          end

    fun ppCreate (out, cr) = (
          indent out;
          case Create.arrayDim cr
           of NONE => pr (out, "COLLECTION\n")
            | SOME dim => prl (out, ["GRID(", Int.toString dim, ")\n"])
          (* end case *);
          ppCFG (incIndent out, Create.createCode cr))

    fun ppGlobal outS gv = let
          val ln = [
                  GlobalVarKind.toString(IR.GlobalVar.kind gv), " ", Ty.toString(GVar.ty gv),
                  " ", GVar.uniqueName gv, "\n"
                ]
          val ln = if GVar.isVarying gv then "varying " :: ln else ln
          in
            indent outS; prl(outS, ln)
          end

    fun ppInput outS (Inputs.INP{var, name, init, ...}) = prl (outS, [
            "  input ", GVar.uniqueName var, " (", name, ") = ", Inputs.initToString init, "\n"
          ])

    fun output (outS, msg, prog) = let
          val IR.Program{
                  props, consts, inputs, globals, funcs,
                  constInit, globInit, strand, create, start, update
                } = prog
          val out = (outS, 0)
          val out1 = incIndent out
          in
            pr (out, concat["##### ", IR.irName, ": ", msg, " ####\n"]);
            pr (out, "## properties\n");
            prln (out1, [
                String.concatWithMap " " Properties.toString props, "\n"
              ]);
            pr (out, "## globals\n");
            List.app (ppGlobal out1) consts;
            List.app (ppInput out1) inputs;
            List.app (ppGlobal out1) globals;
            pr (out, "## functions\n");
            List.app (ppFunc out1) funcs;
            pr (out, "## input initialization\n");
            ppCFG (out1, constInit);
            pr (out, "## global-variable initialization\n");
            ppCFG (out1, globInit);
            pr (out, "## strand\n");
            ppStrand (out1, strand);
            pr (out, "## initial strand creation\n");
            ppCreate (out1, create);
            case start
             of SOME cfg => (
                  pr (out, "## global start\n");
                  ppCFG (out1, cfg))
              | NONE => ()
            (* end case *);
            case update
             of SOME cfg => (
                  pr (out, "## global update\n");
                  ppCFG (out1, cfg))
              | NONE => ()
            (* end case *);
            pr (out, "#### end program ####\n")
          end

    fun outputCFG (outS, cfg) = ppCFG ((outS, 0), cfg)

  end
