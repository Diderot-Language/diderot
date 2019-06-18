(*
 *This file does world to image space Transformations
 * Take a look at tex file for examples
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)



structure CoordSpaceTransform : sig

  (* `worldToImage {info, img, pos}` returns (M, x, stms), where M is a variable
   * bound to the world-space to image-space linear transformation matrix, x is
   * a variable bound to the transformed position (pos), and stms is a list of
   * assignments that computes the world-space to -image-space transformation.
   *)
    val worldToImage : {
            avail : AvailRHS.t,
            info : ImageInfo.t,         (* the image info for the image *)
            img : MidIR.var,            (* the variable bound to the image *)
            pos : MidIR.var             (* the variable bound to the world-space position *)
          } -> MidIR.var * MidIR.var

    val worldToIndex : {
            avail : AvailRHS.t,
            info : ImageInfo.t,         (* the image info for the image *)
            img : MidIR.var,            (* the variable bound to the image *)
            pos : MidIR.var             (* the variable bound to the world-space position *)
          } -> MidIR.var * MidIR.var * MidIR.var

    val imageToWorld : Ein.param_id * int * Ein.mu list * Ein.param_id
          -> (Ein.mu list * Ein.mu list * Ein.sumrange list * Ein.ein_exp list)

  end = struct

    structure E = Ein
    structure IR = MidIR
    structure Ty = MidTypes
    structure Op = MidOps
    structure V = IR.Var
    structure Mk = MkOperators

    fun assign (x, rator, args) = (x, IR.OP(rator, args))
    fun assignEin (x, rator, args) = ((x, IR.EINAPP(rator, args)))

    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)

    (*getTys:int->ty*int list *int list
    *Returns types based on field dimension
    * Really, just done literally so that 1-d fields pass the type-checker
    *)
    fun getTys 1 = (Ty.intTy, [], [])
      | getTys dim = (Ty.iVecTy dim, [dim], [dim, dim])

    (* WorldToImagespace:dim* imageinfo* mid-il var* mid-il var
        ->mid-il var * mid-il var *code
    *transform image-space position x to world space position
    *)
    fun worldToImage {avail, info, img, pos} = let
          val dim = ImageInfo.dim info
          val (_, fty, pty) = getTys dim
          val mty = Ty.TensorTy pty
          val rty = Ty.TensorTy fty
          val M = AvailRHS.addAssign (avail, "Mtransform", mty, IR.OP(Op.Transform info, [img]))
          val (mulOp, addOp) = if (dim = 1)
                then (Mk.mulRR, Mk.addRR)
                else (Mk.innerTT([dim, dim], [dim]), Mk.addTT[dim])
          val x0 = AvailRHS.addAssign (avail, "MxPos", rty, IR.EINAPP(mulOp, [M, pos])) (*Image-Space position*)
          val T = AvailRHS.addAssign (avail, "Ttranslate", rty, IR.OP(Op.Translate info, [img]))
          val x = AvailRHS.addAssign (avail, "imgPos", rty, IR.EINAPP(addOp, [x0, T]))
          in
            incUse M; incUse x0; incUse T;
            (M, x)
          end

  (* Create fractional, and integer position vectors *)
    fun worldToIndex (arg as {avail, info, img, pos}) = let
          val dim = ImageInfo.dim info
          val (ity, fty, pty) = getTys dim
          val mty = Ty.TensorTy  pty
          val rty = Ty.TensorTy fty
          val (M, x) = worldToImage arg
          val nd = AvailRHS.addAssign (avail, "nd", rty, IR.OP(Op.Floor dim, [x]))      (* real position *)
          val f = AvailRHS.addAssign (avail, "f", rty, IR.EINAPP(Mk.subTT fty, [x, nd])) (* fractional *)
          val n = AvailRHS.addAssign (avail, "n", ity, IR.OP(Op.RealToInt dim, [nd]))   (* integer position *)
          val P = if (dim = 1)
                then M
                else AvailRHS.addAssign (avail, "P", mty, IR.EINAPP(Mk.transposeT pty, [M]))
          in
            (n, f, P)
          end

    (*transformToIndexSpace:int*int*mu list * param_id
    *Make Transform Matrices, and returns new indices as mu and sum_indexids
    *)
  (* nextIdx    -- next available variable ID
   * dim        -- dimension of world space
   * dx         -- list of indices from the differentiation of the field that was probed
   * Pid        -- variable ID of the transformation-matrix parameter

   * #1 new derivative indices applied to kernel
   * #2 tshape, shape of tensor representing probe
   * #3 summation indices creating from transformation multiplcation
   * #4 expressions for transformation matrix P
   *)
    fun  imageToWorld (nextIdx, 1, dx, Pid) = let
        fun iter ([], newdx, ps) = (rev newdx, [], [], rev ps)
        | iter (i::es, newdx, ps) = let
            val p = E.Tensor(Pid, [])
            in
                iter (es, i::newdx, p::ps)
            end
        in
            iter (dx, [], [])
        end
    | imageToWorld (nextIdx, dim, dx, Pid) = let
          fun iter ([], _, newdx, newsx, ps) = (rev newdx, rev newdx, rev newsx, rev ps)
            | iter (i::es, n, newdx, newsx, ps) = let
                val j = E.V n
                val sx = (n, 0, dim-1)
                val p = E.Tensor(Pid, [i, j])
                in
                  iter (es, n+1, j::newdx, sx::newsx, p::ps)
                end
          in
            iter (dx, nextIdx, [], [], [])
          end

  end
