(* type-of.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure TypeOf : sig

  (* return the type of a literal *)
    val literal : Literal.t -> Types.ty

  (* return the type of an AST expression *)
    val expr : AST.expr -> Types.ty

  end = struct

    structure Ty = Types

    fun literal lit = (case lit
           of (Literal.Int _) => Ty.T_Int
            | (Literal.Real _) => Ty.realTy
            | (Literal.String s) => Ty.T_String
            | (Literal.Bool _) => Ty.T_Bool
          (* end case *))

    fun varUse (x, _) = Var.monoTypeOf x

    fun expr e = (case e
           of AST.E_Var x => varUse x
            | AST.E_Lit lit => literal lit
            | AST.E_Kernel h => Ty.T_Kernel(Ty.DiffConst(Kernel.continuity h))
            | AST.E_Select(_, f) => varUse f
            | AST.E_Prim(_, _, _, ty) => ty
            | AST.E_Apply(_, _, ty) => ty
            | AST.E_Comprehension(_, _, ty) => ty
            | AST.E_ParallelMap(_, _, _, ty) => ty
            | AST.E_Tensor(_, ty) => ty
            | AST.E_Seq(_, ty) => ty
            | AST.E_Slice(_, _, ty) => ty
            | AST.E_Cond(_, _, _, ty) => ty
            | AST.E_Orelse _ => Ty.T_Bool
            | AST.E_Andalso _ => Ty.T_Bool
            | AST.E_LoadNrrd(_, _, ty) => ty
            | AST.E_Coerce{dstTy, ...} => dstTy
          (* end case *))

  end
