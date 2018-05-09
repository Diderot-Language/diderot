(* parse-tree-pp.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure ParseTreePP : sig

    val output : Error.err_stream -> TextIO.outstream * string * ParseTree.program -> unit

  end = struct

    structure PT = ParseTree
    structure E = Error

    datatype strm = S of {
        indent : int,                   (* current indentation level *)
        span : E.span,
        info : strm_info
      }

    and strm_info = Info of {
        mark : bool,                    (* if true, print mark info *)
        errS : Error.err_stream,        (* for interpreting spans *)
        outS : TextIO.outstream         (* output I/O stream to print to *)
      }

    fun new {errS, outS, showMarks} = S{
            indent = 0, span = (0, 0),
            info = Info{mark = showMarks, errS = errS, outS = outS}
          }

  (* print text *)
    fun pr (S{info=Info{outS, ...}, ...}, txt) = TextIO.output(outS, txt)

  (* print a newline *)
    fun nl (S{info=Info{outS, ...}, ...}) = TextIO.output1 (outS, #"\n")

  (* print whitespace to indent the text *)
    fun prIndent (S{indent=n, info=Info{outS, ...}, ...}) = let
          fun lp 0 = ()
            | lp i = (TextIO.output(outS, "  "); lp(i-1))
          in
            lp n
          end

  (* increment indentation level *)
    fun inc (S{indent, span, info}) = S{indent=indent+1, span=span, info=info}

    fun nest strm f = f (inc strm)

  (* print location information *)
    fun prLoc (S{span, info, ...}) = (case info
           of Info{mark=true, errS, outS, ...} =>
                TextIO.output (outS, " @ " ^ Error.locToString (Error.location (errS, span)))
            | _ => ()
          (* end case *))

  (* update the current span *)
    fun mark (S{indent, info, ...}, {span, tree}) = (S{span=span, indent=indent, info=info}, tree)

    fun prStr (strm, s) = (prIndent strm; pr (strm, s); nl strm);
    fun prId (strm, id) = prStr (strm, Atom.toString id)

    fun prNode (strm, name) = (prIndent strm; pr (strm, name); prLoc strm; nl strm)
    fun prNode' (strm, name, data) = (
          prIndent strm; pr (strm, concat[name, ": ", data]); prLoc strm; nl strm)

  (* print a list of items enclosed in "[" ... "]" *)
    fun prList prItem (strm, []) = (prIndent strm; pr (strm, "[ ]\n"))
      | prList prItem (strm, items) = (
          prIndent strm; pr (strm, "[\n");
          nest strm
            (fn strm' => List.app (fn item => prItem (strm', item)) items);
          prIndent strm; pr (strm, "]\n"))

    fun prOpt prItem (strm, NONE) = ()
      | prOpt prItem (strm, SOME item) = prItem (strm, item)

    fun program (strm, PT.Program m) = let
          val (strm, prog) = mark (strm, m)
          in
            prNode (strm, "Program");
            nest strm (fn strm => (
              prList globalDcl (strm, #globals prog);
              prOpt stmt (strm, #globInit prog);
              strandDcl (strm, #strand prog);
              create (strm, #create prog);
              prOpt stmt (strm, #start prog);
              prOpt stmt (strm, #update prog)))
          end

    and globalDcl (strm, obj) = (case obj
           of PT.GD_Mark m => globalDcl (mark (strm, m))
            | PT.GD_Const(t, {tree=x, ...}, NONE) =>
                prNode' (strm, "Const", Atom.toString x)
            | PT.GD_Const(t, {tree=x, ...}, SOME e) => (
                prNode' (strm, "Const", Atom.toString x ^ "=");
                nest strm (fn strm => expr (strm, e)))
            | PT.GD_Input(t, {tree=x, ...}, optDesc, NONE) => (
                prNode' (strm, "Input", Atom.toString x);
                case optDesc
                 of SOME desc => nest strm (
                      fn strm => prStr(strm, concat["\"", String.toString desc, "\""]))
                  | NONE => ()
                (* end case *))
            | PT.GD_Input(t, {tree=x, ...}, optDesc, SOME e) => (
                prNode' (strm, "Input", Atom.toString x ^ "=");
                nest strm (fn strm => (
                case optDesc
                   of SOME desc => prStr(strm, concat["\"", String.toString desc, "\""])
                    | NONE => ()
                  (* end case *);
                  expr (strm, e))))
            | PT.GD_Var vd => (
                prNode (strm, "Var");
                nest strm (fn strm => varDcl (strm, vd)))
            | PT.GD_Func(t, {tree=f, ...}, params, body) => (
                prNode' (strm, "Func", Atom.toString f);
                nest strm (fn strm => (
                  ty (strm, t); prList param (strm, params); funBody (strm, body))))
          (* end case *))

    and strandDcl (strm, obj) = (case obj
           of PT.SD_Mark m => strandDcl (mark (strm, m))
            | PT.SD_Strand{name={tree=name, ...}, params, state, stateInit, methods} => (
                prNode' (strm, "Strand", Atom.toString name);
                nest strm (fn strm => (
                  prList param (strm, params);
                  prList stateVarDcl (strm, state);
                  prOpt stmt (strm, stateInit);
                  prList method (strm, methods))))
          (* end case *))

    and stateVarDcl (strm, obj) = (case obj
           of PT.SVD_Mark m => stateVarDcl (mark (strm, m))
            | PT.SVD_VarDcl(isOutput, vd) =>
                if isOutput
                  then prNode' (strm, "VarDcl", "output")
                  else prNode (strm, "VarDcl")
          (* end case *))

    and create (strm, obj) = (case obj
           of PT.CR_Mark m => create (mark (strm, m))
            | PT.CR_Collection comp => (
                prNode (strm, "Collection");
                nest strm (fn strm => comprehension (strm, comp)))
            | PT.CR_Array(e, comp) => (
                prNode (strm, "Grid");
                nest strm (fn strm => (prOpt expr (strm, e); comprehension (strm, comp))))
          (* end case *))

    and param (strm, obj) = (case obj
           of PT.P_Mark m => param (mark (strm, m))
            | PT.P_Param(t, {tree=x, ...}) => (
                prNode' (strm, "Param", Atom.toString x);
                nest strm (fn strm => ty (strm, t)))
          (* end case *))

    and ty (strm, obj) = (case obj
           of PT.T_Mark m => ty (mark (strm, m))
            | PT.T_Bool => prNode (strm, "Bool")
            | PT.T_Int => prNode (strm, "Int")
            | PT.T_Real => prNode (strm, "Real")
            | PT.T_String => prNode (strm, "String")
            | PT.T_Id id => prNode' (strm, "Id", Atom.toString id)
            | PT.T_Kernel diff => prNode' (strm, "Kernel", "#" ^ IntInf.toString diff)
            | PT.T_Field{diff=NONE, dim, shape} => (
                prNode' (strm, "Field", "#∞");
                nest strm (fn strm => (expr (strm, dim); prList expr (strm, shape))))
            | PT.T_Field{diff=SOME k, dim, shape} => (
                prNode' (strm, "Field", "#" ^ IntInf.toString k);
                nest strm (fn strm => (expr (strm, dim); prList expr (strm, shape))))
            | PT.T_Tensor shp => (
                prNode (strm, "Tensor");
                nest strm (fn strm => prList expr (strm, shp)))
            | PT.T_Image{dim, shape} => (
                prNode (strm, "Image");
                nest strm (fn strm => (expr (strm, dim); prList expr (strm, shape))))
            | PT.T_Seq(t, e) => (
                prNode (strm, "Seq");
                nest strm (fn strm => (ty (strm, t); expr (strm, e))))
            | PT.T_DynSeq t => (
                prNode (strm, "DynSeq");
                nest strm (fn strm => ty (strm, t)))
          (* end case *))

    and funBody (strm, obj) = (case obj
           of PT.FB_Expr e => (
                prNode (strm, "Expr");
                nest strm (fn strm => expr (strm, e)))
            | PT.FB_Stmt stm => (
                prNode (strm, "Stmt");
                nest strm (fn strm => stmt (strm, stm)))
          (* end case *))

    and varDcl (strm, obj) = (case obj
           of PT.VD_Mark m => varDcl (mark (strm, m))
            | PT.VD_Decl(t, {tree=x, ...}, NONE) =>
                prNode' (strm, "Decl", Atom.toString x)
            | PT.VD_Decl(t, {tree=x, ...}, SOME e) => (
                prNode' (strm, "Decl", Atom.toString x ^ "=");
                nest strm (fn strm => expr (strm, e)))
          (* end case *))

    and method (strm, obj) = (case obj
           of PT.M_Mark m => method (mark (strm, m))
            | PT.M_Method(name, stm) => (
                prNode' (strm, "Method", StrandUtil.nameToString name);
                nest strm (fn strm => stmt(strm, stm)))
          (* end case *))

    and stmt (strm, obj : PT.stmt) = (case obj
           of PT.S_Mark m => stmt (mark (strm, m))
            | PT.S_Block(stms) => (
                prNode (strm, "Block");
                nest strm (fn strm => prList stmt (strm, stms)))
            | PT.S_IfThen(e, stm) => (
                prNode (strm, "IfThen");
                nest strm (fn strm => (expr (strm, e); stmt (strm, stm))))
            | PT.S_IfThenElse(e, stm1, stm2) => (
                prNode (strm, "IfThenElse");
                nest strm (fn strm => (expr (strm, e); stmt (strm, stm1); stmt (strm, stm2))))
            | PT.S_Foreach(t, iter, stm) => (
                prNode (strm, "Foreach");
                nest strm (fn strm => (ty (strm, t); iterator (strm, iter); stmt (strm, stm))))
            | PT.S_Print(es) => (
                prNode (strm, "Print");
                nest strm (fn strm => prList expr (strm, es)))
            | PT.S_New(id, es) => (
                prNode' (strm, "New", Atom.toString id);
                nest strm (fn strm => prList expr (strm, es)))
            | PT.S_Stabilize => prNode (strm, "Stabilize")
            | PT.S_Die => prNode (strm, "Die")
            | PT.S_Continue => prNode (strm, "Continue")
            | PT.S_Return e => (
                prNode (strm, "Return");
                nest strm (fn strm => expr (strm, e)))
            | PT.S_Decl vd => (
                prNode (strm, "Decl");
                nest strm (fn strm => varDcl (strm, vd)))
            | PT.S_Assign({tree=x, ...}, NONE, e) => (
                prNode' (strm, "Assign", Atom.toString x ^ "=");
                nest strm (fn strm => expr (strm, e)))
            | PT.S_Assign({tree=x, ...}, SOME rator, e) => (
                prNode' (strm, "Assign", Atom.toString x ^ Atom.toString rator);
                nest strm (fn strm => expr (strm, e)))
          (* end case *))

    and comprehension (strm, obj) = (case obj
           of PT.COMP_Mark m => comprehension (mark (strm, m))
            | PT.COMP_Comprehension(e, iters) => (
                prNode (strm, "Comprehension");
                nest strm (fn strm => (
                  expr (strm, e);
                  prList iterator (strm, iters))))
          (* end case *))

    and iterator (strm, obj) = (case obj
           of PT.I_Mark m => iterator (mark (strm, m))
            | PT.I_Iterator({tree=x, ...}, e) => (
                prNode' (strm, "Iterator", Atom.toString x);
                nest strm (fn strm => expr (strm, e)))
          (* end case *))

    and expr (strm, obj) = (case obj
           of PT.E_Mark m => expr (mark (strm, m))
            | PT.E_Cond(e1, e2, e3) => (
                prNode (strm, "Cond");
                nest strm (fn strm => (expr (strm, e1); expr (strm, e2); expr (strm, e3))))
            | PT.E_Range(e1, e2) => (
                prNode (strm, "Range");
                nest strm (fn strm => (expr (strm, e1); expr (strm, e2))))
            | PT.E_OrElse(e1, e2) => (
                prNode (strm, "OrElse");
                nest strm (fn strm => (expr (strm, e1); expr (strm, e2))))
            | PT.E_AndAlso(e1, e2) => (
                prNode (strm, "AndAlso");
                nest strm (fn strm => (expr (strm, e1); expr (strm, e2))))
            | PT.E_BinOp(e1, rator, e2) => (
                prNode' (strm, "BinOp", Atom.toString rator);
                nest strm (fn strm => (expr (strm, e1); expr (strm, e2))))
            | PT.E_UnaryOp(rator, e) => (
                prNode' (strm, "UnaryOp", Atom.toString rator);
                nest strm (fn strm => expr (strm, e)))
            | PT.E_Apply(e, args) => (
                prNode (strm, "Apply");
                nest strm (fn strm => (expr (strm, e); prList expr (strm, args))))
            | PT.E_Subscript(e, indices) => (
                prNode (strm, "Subscript");
                nest strm (fn strm => (
                  expr (strm, e);
                  prList (fn (strm, SOME e) => expr (strm, e) | (strm, NONE) => prStr (strm, ":"))
                    (strm, indices))))
            | PT.E_Select(e, f) => (
                prNode (strm, "Select");
                nest strm (fn strm => (expr (strm, e); prId (strm, f))))
            | PT.E_Real e => (
                prNode (strm, "Real");
                nest strm (fn strm => expr (strm, e)))
            | PT.E_LoadSeq e => (
                prNode (strm, "Load");
                nest strm (fn strm => expr (strm, e)))
            | PT.E_LoadImage e => (
                prNode (strm, "Image");
                nest strm (fn strm => expr (strm, e)))
            | PT.E_Var x => prNode' (strm, "Var", Atom.toString x)
            | PT.E_Kernel(kern, dim) =>
                prNode' (strm, "Kernel", concat[Atom.toString kern, "#", IntInf.toString dim])
            | PT.E_Lit lit => prNode' (strm, "Lit", Literal.toString lit)
            | PT.E_Id e => (
                prNode (strm, "Id");
                nest strm (fn strm => expr (strm, e)))
            | PT.E_Zero es => (
                prNode (strm, "Zero");
                nest strm (fn strm => prList expr (strm, es)))
            | PT.E_NaN es => (
                prNode (strm, "NaN");
                nest strm (fn strm => prList expr (strm, es)))
            | PT.E_Sequence es => (
                prNode (strm, "Sequence");
                nest strm (fn strm => prList expr (strm, es)))
            | PT.E_SeqComp comp => (
                prNode (strm, "SeqComp");
                nest strm (fn strm => comprehension (strm, comp)))
            | PT.E_Cons es => (
                prNode (strm, "Cons");
                nest strm (fn strm => prList expr (strm, es)))
          (* end case *))

    fun output errS (outS, message, prog) = let
          val strm = new{outS = outS, errS = errS, showMarks = true}
          in
            pr (strm, concat ["/* ParseTree: ", message, " */\n"]);
            program (inc strm, prog);
            pr (strm, "/* end program */\n");
            TextIO.flushOut outS
          end

  end
