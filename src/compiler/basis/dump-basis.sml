(* dump-basis.sml
 *
 * Support for dumping a human-readable description of the Diderot Basis.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure DumpBasis : sig

    val dump : string * ParseTree.version -> unit

  end = struct

    structure Op = Operators
    structure Ty = Types
    structure ATbl = AtomTable

  (* mapping from operator names to LaTeX *)
    val opToTeX = let
          val tbl = ATbl.mkTable (64, Fail "operators")
          in
            List.app (ATbl.insert tbl) [
                (Op.op_lt, "\\lstinline!<!"),
                (Op.op_lte, "\\lstinline!<=!"),
                (Op.op_equ, "\\lstinline!==!"),
                (Op.op_neq, "\\lstinline!!=!"),
                (Op.op_gte, "\\lstinline!>=!"),
                (Op.op_gt, "\\lstinline!>!"),
                (Op.op_add, "\\lstinline!+!"),
                (Op.op_sub, "\\lstinline!-!"),
                (Op.op_mul, "\\lstinline!*!"),
                (Op.op_dot, "\\lstinline[mathescape]!$\\bullet$!"),
                (Op.op_cross, "\\lstinline[mathescape]!${\\times}$!"),
                (Op.op_convolve, "\\lstinline[mathescape]!${\\circledast}$!"),
                (Op.op_outer, "\\lstinline[mathescape]!${\\otimes}$!"),
                (Op.op_colon, "\\lstinline!:!"),
                (Op.op_div, "\\lstinline!/!"),
                (Op.op_mod, "\\lstinline!%!"),
                (Op.op_pow, "\\lstinline!^!"),
                (Op.op_at, "\\lstinline!@!"),
                (Op.op_neg, "\\lstinline!-!"),
                (Op.op_not, "\\lstinline|!|"),
                (Op.op_D, "\\lstinline[mathescape]!$\\nabla$!"),
                (Op.op_Ddot, "\\lstinline[mathescape]!$\\nabla{\\bullet}$!"),
                (Op.op_Dotimes, "\\lstinline[mathescape]!$\\nabla{\\otimes}$!"),
                (Op.op_curl, "\\lstinline[mathescape]!$\\nabla{\\times}$!"),
                (Op.op_norm, "\\lstinline[mathescape]!| $\\cdot$ |!")
              ];
            ATbl.lookup tbl
          end

  (* an environment that maps meta variables to indices *)
    type env = {
        tvs : Ty.ty_var list,
        dfvs : Ty.diff_var list,
        svs : Ty.shape_var list,
        dvs : Ty.dim_var list
      }

    fun withScheme (mvs, ty) f = let
          fun mkEnv ([], tvs, dfvs, svs, dvs) = {
                  tvs = rev tvs, dfvs = rev dfvs, svs = rev svs, dvs = rev dvs
                }
            | mkEnv (mv::mvs, tvs, dfvs, svs, dvs) = (case mv
                 of Ty.TYPE tv => mkEnv (mvs, tv::tvs, dfvs, svs, dvs)
                  | Ty.DIFF dfv => mkEnv (mvs, tvs, dfv::dfvs, svs, dvs)
                  | Ty.SHAPE sv => mkEnv (mvs, tvs, dfvs, sv::svs, dvs)
                  | Ty.DIM dv => mkEnv (mvs, tvs, dfvs, svs, dv::dvs)
                (* end case *))
          in
            f (mkEnv(mvs, [], [], [], []), ty)
          end

  (* return a meta-variable's index.  We return 0 if the variable is the only one
   * of its kind and otherwise return its position in the list + 1.
   *)
    fun lookupMetaVar (sameVar, proj) (env : env, x) = (case proj env
           of [_] => 0
            | xs => (case List.findi (fn (_, y) => sameVar(x, y)) xs
                 of SOME(i, _) => i+1
                  | NONE => raise Fail "bogus meta variable"
                (* end case *))
          (* end case *))

    val lookupTyVar = lookupMetaVar (MetaVar.sameTyVar, #tvs)
    val lookupDiffVar = lookupMetaVar (MetaVar.sameDiffVar, #dfvs)
    val lookupShapeVar = lookupMetaVar (MetaVar.sameShapeVar, #svs)
    val lookupDimVar = lookupMetaVar (MetaVar.sameDimVar, #dvs)

  (* convert a Diderot type to LaTeX *)
    fun tyToTeX (env, ty) = let
          fun diff2s (Ty.DiffConst NONE) = ""
            | diff2s (Ty.DiffConst(SOME k)) = "\\#" ^ Int.toString k
            | diff2s (Ty.DiffVar(dfv, off)) = let
                val off = if off = 0 then [] else ["+", Int.toString off]
                in
                  case lookupDiffVar(env, dfv)
                   of 0 => concat("\\#$k$" :: off)
                    | k => concat("\\#$k_{" :: Int.toString k :: "}$" :: off)
                  (* end case *)
                end
          fun dim2s' (Ty.DimConst d) = Int.toString d
            | dim2s' (Ty.DimVar dv) = (case lookupDimVar(env, dv)
                 of 0 => "d"
                  | d => concat["d_{", Int.toString d, "}"]
                (* end case *))
          fun shp2s' (Ty.Shape dvs) = String.concatWithMap "," dim2s' dvs
            | shp2s' (Ty.ShapeVar sv) = (case lookupShapeVar(env, sv)
                 of 0 => "\\sigma"
                  | i => concat["\\sigma_{", Int.toString i, "}"]
                (* end case *))
            | shp2s' (Ty.ShapeExt(shp, dim)) = concat[shp2s' shp, ",", dim2s' dim]
          fun shp2s (Ty.Shape[]) = ""
            | shp2s shp = concat["$", shp2s' shp, "$"]
          fun dim2s dim = concat["$", dim2s' dim, "$"]
          fun toTeX ty = (case ty
                 of Ty.T_Var tv => (case lookupTyVar (env, tv)
                       of 0 => "$\\tau$"
                        | i => concat["$\\tau_{", Int.toString i, "}$"]
                      (* end case *))
                  | Ty.T_Bool => "bool"
                  | Ty.T_Int => "int"
                  | Ty.T_String => "string"
                  | Ty.T_Sequence(ty, NONE) => toTeX ty ^ "[]"
                  | Ty.T_Sequence(ty, SOME dim) =>
                      String.concat[toTeX ty, "[", dim2s dim, "]"]
                  | Ty.T_Kernel diff => "kernel" ^ diff2s diff
                  | Ty.T_Tensor(Ty.Shape[]) => "real"
                  | Ty.T_Tensor(Ty.Shape[Ty.DimConst 2]) => "vec2"
                  | Ty.T_Tensor(Ty.Shape[Ty.DimConst 3]) => "vec3"
                  | Ty.T_Tensor(Ty.Shape[Ty.DimConst 4]) => "vec4"
                  | Ty.T_Tensor(Ty.Shape[Ty.DimConst 2, Ty.DimConst 2]) => "mat2"
                  | Ty.T_Tensor(Ty.Shape[Ty.DimConst 3, Ty.DimConst 3]) => "mat3"
                  | Ty.T_Tensor(Ty.Shape[Ty.DimConst 4, Ty.DimConst 4]) => "mat4"
                  | Ty.T_Tensor shp => String.concat["tensor[", shp2s shp, "]"]
                  | Ty.T_Image{dim, shape} => String.concat[
                        "image(", dim2s dim, ")[", shp2s shape, "]"
                      ]
                  | Ty.T_Field{diff, dim, shape} => String.concat[
                        "field", diff2s diff, "(", dim2s dim, ")[", shp2s shape, "]"
                      ]
                  | Ty.T_Fun([], ty) => "() $\\rightarrow$ " ^ toTeX ty
                  | Ty.T_Fun(tys, ty) => String.concat[
                        String.concatWithMap " * " toTeX tys,
                        " $\\rightarrow$ ", toTeX ty
                      ]
                  | _ => raise Fail "tyToTeX: unexpected type"
                (* end case *))
          in
            toTeX ty
          end

    fun dump (file, vers) = let
          val outS = TextIO.openOut file
          val {ops, fns, kerns} = Basis.getBasis vers
          fun pr s = TextIO.output(outS, s)
          fun prl ss = pr(String.concat ss)
        (* dump an operator *)
          fun prOp [rator] = prl [
                  "  ", opToTeX(Atom.atom(Var.nameOf rator)), " & ",
                  withScheme (Var.typeOf rator) tyToTeX, "\\\\\n"
                ]
            | prOp (rator::rators) = (
                prl ["% overload ", Var.nameOf rator, "\n"];
                prl [
                    "  ", opToTeX(Atom.atom(Var.nameOf rator)), " & ",
                    withScheme (Var.typeOf rator) tyToTeX, " \\\\\n"
                  ];
                List.app (fn rator => prl [
                      "    & ", withScheme (Var.typeOf rator) tyToTeX, " \\\\\n"
                    ])
                  rators)
        (* dump functions *)
          fun prFn [f] = prl [
                  "  ", Var.nameOf f, " & ",
                  withScheme (Var.typeOf f) tyToTeX, " \\\\\n"
                ]
            | prFn (f::fs) = (
                prl ["% overload ", Var.nameOf f, " \\\\\n"];
                prl [
                    "  \\lstinline!", Var.nameOf f, "! & ",
                    withScheme (Var.typeOf f) tyToTeX, " \\\\\n"
                  ];
                List.app (fn f => prl [
                      "    & ", withScheme (Var.typeOf f) tyToTeX, " \\\\\n"
                    ])
                  fs)
        (* dump kernels *)
          fun prKern h = prl [
                  "  \\lstinline!kernel\\#", Int.toString(Kernel.continuity h), " ",
                  Kernel.name h, "! \\\\\n"
                ]
          in
          (* dump operators *)
            pr "\\begin{longtable}{r{:}l}\n";
            pr "  \\caption{Operators}\n";
            List.app prOp ops;
            pr "\\end{longtable}%\n";
          (* dump functions *)
            pr "\\begin{longtable}{r{:}l}\n";
            pr "  \\caption{Functions}\n";
            List.app prFn fns;
            pr "\\end{longtable}%\n";
          (* dump kernels *)
            pr "\\begin{longtable}{l}\n";
            pr "  \\caption{Kernels}\n";
            List.app prKern kerns;
            pr "\\end{longtable}%\n";
            TextIO.closeOut outS
          end
handle ex => (print(concat["!!!!! uncaught exception ", exnMessage ex, "\n"]); raise ex)

  end
