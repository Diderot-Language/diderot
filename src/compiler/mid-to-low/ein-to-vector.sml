(* ein-to-vector.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure EinToVector : sig

  (* an environment that maps De Bruijn indices to their iteration-index value *)
    type index_env = int IntRedBlackMap.map

    datatype param_kind = Indx | Proj of Ein.index_id

    datatype param = Param of {
        id : int,               (* the parameter's DeBruijn index *)
        arg : LowIR.var,        (* the corresponding argument *)
        ix : Ein.alpha,         (* the multi-index used to subscript the argument *)
        kind : param_kind       (* are we extracting a scalar (Indx) or projecting out
                                 * a vector from the last dimension (Proj)?
                                 *)
     }

  (* vector negation *)
    val negV : AvailRHS.t * index_env * param -> LowIR.var

  (* vector sum of a sequence of vectors *)
    val addV : AvailRHS.t * index_env * param list -> LowIR.var

  (* vector dot product *)
    val dotV : AvailRHS.t * index_env * param * param -> LowIR.var

  (* `sumDotV (avail, mapp, (ix, lb, ub, vecA, vecB))`
   * generates code to compute the sum of the dot products between vecA and vecB, for
   * lb <= ix <= ub.
   *)
    val sumDotV : AvailRHS.t * index_env * Ein.index_id * int * int * param * param -> LowIR.var

  (* generic binary operations on vectors *)
    val binopV : LowOps.rator * int -> AvailRHS.t * index_env * param * param -> LowIR.var

  end = struct

    structure IR = LowIR
    structure Ty = LowTypes
    structure Op = LowOps
    structure E = Ein
    structure Mk = MkLowIR
    structure IMap = IntRedBlackMap

  (* an environment that maps De Bruijn indices to their iteration-index value *)
    type index_env = int IMap.map

    datatype param_kind = Indx | Proj of Ein.index_id

    datatype param = Param of {
        id : int,               (* the parameter's DeBruijn index *)
        arg : IR.var,           (* the corresponding argument *)
        ix : E.alpha,           (* ?? *)
        kind : param_kind
      }

    fun alphaToIdxs (mapp, mus) = let
          fun lookup (E.V id) = (case IMap.find (mapp, id)
                 of SOME n => n
                  | NONE => raise Fail(concat["alphaToIdxs(_, V ", Int.toString id, "): out of bounds"])
                (* end case *))
            | lookup (E.C i) = i
          in
            List.map lookup mus
          end

  (* convert a parameter to its LowIL variable equivalent *)
    fun paramToVar (_, _, Param{arg, ix = [], ...}) = arg
      | paramToVar (avail, mapp, Param{id, arg, ix, kind=Indx}) =
          Mk.tensorIndex (avail, mapp, arg, ix)
      | paramToVar (avail, mapp, Param{id, arg, ix, kind=Proj dim}) =
          AvailRHS.addAssign (
            avail, "projParam", Ty.TensorTy[dim],
            IR.OP(Op.ProjectLast(LowIR.Var.ty arg, alphaToIdxs (mapp, ix)), [arg]))

  (* vector negation *)
    fun negV (avail, mapp, vec) = let
          val v = paramToVar (avail, mapp, vec)
          val ty as Ty.TensorTy[dim] = IR.Var.ty v
          in
            AvailRHS.addAssign (avail, "negV", ty, IR.OP(Op.VNeg dim, [v]))
          end

  (* vector addition for a sequence of vectors *)
    fun addV (avail, mapp, vs) = let
          val vs as (v :: _) = List.map (fn v => paramToVar (avail, mapp, v)) vs
          val Ty.TensorTy[dim] = IR.Var.ty v
          in
            Mk.reduce (avail, fn (avail, u, v) => Mk.vecAdd(avail, dim, u, v), vs)
          end

  (* dot product of two vectors *)
    fun dotV (avail, mapp, u, v) = let
          val u = paramToVar (avail, mapp, u)
          val v = paramToVar (avail, mapp, v)
          val ty as Ty.TensorTy[dim] = IR.Var.ty u
          in
            Mk.vecDot (avail, dim, u, v)
          end

  (* `sumDotV (avail, mapp, (ix, lb, ub, vecA, vecB))`
   *)
    fun sumDotV (avail, mapp, ix, lb, ub, vecA, vecB) = let
          fun dotp i = dotV (avail, IMap.insert(mapp, ix, i), vecA, vecB)
        (* compute the sum of dot products for lb <= i <= ub, where sum is initialized
         * to the dot produce for lb.
         *)
          fun sum (i, acc) =
                if (i <= ub)
                  then sum (i+1, Mk.realAdd (avail, acc, dotp i))
                  else acc
          in
            sum (lb+1, dotp lb)
          end

  (* generic binary operations on vectors *)
    fun binopV (rator, dim) (avail, mapp, u, v) = let
          val u = paramToVar (avail, mapp, u)
          val v = paramToVar (avail, mapp, v)
          in
            AvailRHS.addAssign (avail, "binopV", Ty.TensorTy[dim], IR.OP(rator, [u, v]))
          end

  end
