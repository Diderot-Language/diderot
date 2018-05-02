(* load-voxels.sml
 *
 * This module handles the expansion of the Mir IR operators LoadVoxels and
 * LoadVoxelsWithCtl into Low IR code.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure LoadVoxels : sig

  (* expand the assignment
   *
   *    lhs = LoadVoxels<info,s>(img, idx)
   *
   * into Low IR code.  If the image has type `image(d)[dd]`, then the resulting lhs tensor
   * has type `tensor[dd,s^d]`, where `s^d` means `d` occurrences of `s`.
   *)
    val expand : {
            lhs : LowIR.var,
            info : ImageInfo.t,
            s : int,
            img : LowIR.var,
            idx : LowIR.var
          } -> LowIR.assign list

  (* expand the assignment
   *
   *    lhs = LoadVoxelsWithCtl<info,s,ctl>(img, idx)
   *
   * into Low IR code.
   *)
    val expandWithCtl : {
            lhs : LowIR.var,
            info : ImageInfo.t,
            s : int,
            ctl : IndexCtl.t,
            img : LowIR.var,
            idx : LowIR.var
          } -> LowIR.assign list

  end = struct

    structure IR = LowIR
    structure V = IR.Var
    structure Ty = LowTypes
    structure Op = LowOps

    fun assignOp (avail, pre, ty, opss, args) =
          AvailRHS.addAssign(avail, pre, ty, IR.OP(opss, args))
    fun assignCons (_, _, []) = raise Fail "empty cons"
      | assignCons (avail, pre, args as x::_) = let
          val ty = Ty.TensorTy(List.length args :: Ty.tensorShape(V.ty x))
          in
            AvailRHS.addAssign(avail, pre, ty, IR.CONS(args, ty))
          end

    fun imul' (avail, pre, a, b) = assignOp (avail, pre, Ty.IntTy, Op.IMul, [a, b])
    fun iadd' (avail, pre, a, b) = assignOp (avail, pre, Ty.IntTy, Op.IAdd, [a, b])
    fun ilit' (avail, pre, n) =
          AvailRHS.addAssign (avail, pre, Ty.IntTy, IR.LIT(Literal.Int(IntLit.fromInt n)))
    fun imul (avail, a, b) = imul' (avail, "mulRes", a, b)
    fun iadd (avail, a, b) = iadd' (avail, "addRes", a, b)
    fun ilit (avail, n) = ilit' (avail, "ilit", n)

  (* the name of a voxel or tensor of voxels. *)
    fun voxName idxs = concat("v" :: List.rev ("_" :: List.map Int.toString idxs))

  (* load the indices into individual variables; return the number of indices and
   * the list of variables.
   *)
    fun loadIndices (avail, idx) = (case V.ty idx
           of Ty.IntTy => (1, [idx])
            | ty as Ty.SeqTy(Ty.IntTy, SOME n) => let
                fun loadIdx i =
                      assignOp (avail, "idx", Ty.IntTy, Op.Subscript ty, [idx, ilit (avail, i)])
                in
                  (n, List.tabulate (n, loadIdx))
                end
            | ty => raise Fail(concat["loadIndices: bogus type " ^ Ty.toString ty])
          (* end case *))

  (* load the sizes of the axes into individual variables (except the slowest).
   * The result will be a list of variables holding the axis sizes ordered from
   * fastest to slowest.
   *)
    fun loadSizes (avail, info, img) = (case ImageInfo.sizes info
           of [] => let (* no proxy, so we need load sizes from image object *)
                fun load i = assignOp (avail, "sz", Ty.IntTy, Op.ImageDim(info, i), [img])
                in
                  List.tabulate (ImageInfo.dim info - 1, load)
                end
            | sz::szs => let
                fun load (_, [], xs) = List.rev xs (* drop last size, since it is the slowest *)
                  | load (sz, sz'::szs, xs) = load (sz', szs, ilit (avail, sz) :: xs)
                in
                  load (sz, szs, [])
                end
          (* end case *))

  (* `adjustForStrideAndOffset avail stride offset ix` add code to compute the total
   * offset from the image base address, which is
   *
   *    offp = stride * ix + offset
   *)
    fun adjustForStrideAndOffset (avail, 1, 0, ix) = ix
      | adjustForStrideAndOffset (avail, stride, 0, ix) =
          imul' (avail, "offp", ilit' (avail, "stride", stride), ix)
      | adjustForStrideAndOffset (avail, 1, offset, ix) =
          iadd' (avail, "offp", ix, ilit' (avail, "offset", offset))
      | adjustForStrideAndOffset (avail, stride, offset, ix) =
          iadd' (avail, "offp",
            imul (avail, ilit' (avail, "stride", stride), ix),
            ilit' (avail, "offset", offset))

  (* compute the voxel address for a sample.  For the call
   *
   *    voxelAddress (avail, info, img) (offset, [i_1, ..., i_d])
   *
   * the address is given by
   *
   *    base + stride * (i_1 + N_1 * (i_2 + N_2 * (... + N_{d-1} * i_d) ...)) + offset
   *
   * where
   *    base    -- base address of the image data
   *    stride  -- number of samples per voxel
   *    offset  -- offset of sample being addressed
   *    N_i     -- size of ith axis in elements
   *
   * Note that we are following the Nrrd convention that the axes are ordered
   * in fastest to slowest order.  We are also assuming the C semantics of address
   * arithmetic, where the offset will be automatically scaled by the size of the
   * elements.
   *)
    fun voxelAddress (avail, info, img, chkIndex) = let
          val stride = ImageInfo.stride info
        (* get N_1 ... N_{d-1} *)
          val sizes = loadSizes (avail, info, img)
          fun index (axis, bix, ix) = chkIndex (axis, iadd(avail, bix, ilit (avail, ix)))
          fun voxelAddr (offset, [bix], [ix]) = let
              (* 1D image *)
                val offp = adjustForStrideAndOffset (avail, stride, offset, index (0, bix, ix))
                in
                  [img, offp]
                end
            | voxelAddr (offset, baseIxs, ixs) = let
                fun gen (axis, [], [bix], [ix]) = index (axis, bix, ix)
                  | gen (axis, sz::szs, bix::bixs, ix::ixs) =
                      iadd (avail,
                        index (axis, bix, ix),
                        imul (avail, sz, gen (axis+1, szs, bixs, ixs)))
                  | gen _ = raise Fail "voxelAddress: arity mismatch"
                val t = gen (0, sizes, baseIxs, ixs)
                val offp = adjustForStrideAndOffset (avail, stride, offset, t)
                in
                  [img, offp]
                end
          in
            voxelAddr
          end

  (* iterate over an N-dimensional grid of s^n locations.  For each location,
   * the function f gets called with a list of indices [idx1, ..., idxn],
   * where idx1 varies the fastest and idxn varies the slowest.
   *)
    fun gridIterate (avail, n, s) (f : int list -> IR.var) = let
          fun iter (0, idxs) = f idxs
            | iter (i, idxs) = let
                fun lp (j, xs) = if (j < s)
                      then lp (j+1, iter(i-1, j::idxs) :: xs)
                      else assignCons (avail, voxName idxs, List.rev xs)
                in
                  lp (0, [])
                end
          in
            iter (n, [])
          end

  (* `shapeIterate avail shp f` generates code to construct a tensor with
   * shape `shp`.  For each element, the offset of the element is passed
   * to the function `f`, which returns a variable that has been assigned
   * to the corresponding tensor of samples.
   *)
    fun shapeIterate avail shp (f : int -> IR.var) = let
          fun iter ([], offset) = (f offset, offset+1)
            | iter (d::dd, offset) = let
                fun lp (i, offset, xs) = if (i < d)
                      then let
                        val (x, offset) = iter (dd, offset)
                        in
                          lp (i+1, offset, x::xs)
                        end
                      else (assignCons (avail, "voxels", List.rev xs), offset)
                in
                  lp (0, offset, [])
                end
          val (y, _) = iter (shp, 0)
          in
            y
          end

    fun expand {lhs, info, s, img, idx} = let
          val avail = AvailRHS.new ()
          val (dim, baseIdxs) = loadIndices (avail, idx)
          val voxelAddr = voxelAddress (avail, info, img, fn (axis, ix) => ix)
          fun load offset idxs = assignOp (
                avail, voxName idxs, Ty.realTy,
                Op.LoadVoxel info,
                voxelAddr (offset, baseIdxs, idxs))
          val result = shapeIterate avail (ImageInfo.voxelShape info)
                (fn offset => gridIterate (avail, dim, s) (load offset))
          in
            AvailRHS.addAssignToList (avail, (lhs, IR.VAR result));
            List.rev (AvailRHS.getAssignments avail)
          end

    fun expandWithCtl {lhs, info, s, ctl, img, idx} = let
          val avail = AvailRHS.new ()
          val (dim, baseIdxs) = loadIndices (avail, idx)
        (* add check of index for border control *)
          fun checkIndex (axis, ix) = assignOp (
                avail, "ix", Ty.IntTy,
                Op.ControlIndex(info, ctl, axis), [img, ix])
          val voxelAddr = voxelAddress (avail, info, img, checkIndex)
          fun load offset idxs = assignOp (
                avail, voxName idxs, Ty.realTy,
                Op.LoadVoxel info,
                voxelAddr (offset, baseIdxs, idxs))
          val result = shapeIterate avail (ImageInfo.voxelShape info)
                (fn offset => gridIterate (avail, dim, s) (load offset))
          in
            AvailRHS.addAssignToList (avail, (lhs, IR.VAR result));
            List.rev (AvailRHS.getAssignments avail)
          end

  end
