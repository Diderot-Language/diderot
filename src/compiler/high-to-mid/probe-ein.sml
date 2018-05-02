(* probe-ein.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* This file expands probed fields
 * Take a look at ProbeEin tex file for examples
 * Note that the original field is an EIN operator in the form <V_alpha * H^(deltas)>(midIR.var list )
 * Param_ids are used to note the placement of the argument in the midIR.var list
 * Index_ids  keep track of the shape of an Image or differentiation.
 * Mu  bind Index_id
 * Generally, we will refer to the following
 * dim:dimension of field V
 * s: support of kernel H
 * alpha: The alpha in <V_alpha * H^(deltas)>
 * dx: The dx in <V_alpha * nabla_dx H>
 * deltas: The deltas in <V_alpha * h^(deltas) h^(deltas)>
 * Vid:param_id for V
 * hid:param_id for H
 * nid: integer position param_id
 * fid :fractional position param_id
 * img-imginfo about V
 *)

structure ProbeEin : sig

    val expand : AvailRHS.t -> MidIR.var * MidIR.rhs -> unit

  end = struct

    structure IR = MidIR
    structure Op = MidOps
    structure V = IR.Var
    structure Ty = MidTypes
    structure E = Ein
    structure T = CoordSpaceTransform

    fun mkEin (params, index, body) = Ein.EIN{params = params, index = index, body = body}

    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)

    fun getRHSDst x = (case IR.Var.getDef x
           of IR.OP(rator, args) => (rator, args)
            | rhs => raise Fail(concat[
                  "expected rhs operator for ", IR.Var.toString x,
                  " but found ", IR.RHS.toString rhs
                ])
          (* end case *))

    fun axis dir = (case dir of 0 => "X" | 1 => "Y" | 2 => "Z" | _ => "dir" ^ Int.toString dir)

    fun checkImg imgArg = (case IR.Var.getDef imgArg
           of IR.OP(Op.LoadImage _, _) => imgArg
            | rhs => raise Fail (String.concat[
                  "expected image for ", IR.Var.toString imgArg,
                  " but found ", IR.RHS.toString rhs
                ])
          (* end case *))

  (* get the image referenced on a RHS and its border control (if any) *)
    fun getImagInfo x = (case V.getDef x
           of IR.GLOBAL gv => let
                val Ty.ImageTy info = IR.GlobalVar.ty gv
                in
                  (x, info, NONE)
                end
            | IR.OP(Op.BorderCtlDefault info, [img, v]) =>
                (img, info, raise Fail "Default boarder control")
            | IR.OP(Op.BorderCtlClamp info, [img]) => (img, info, SOME IndexCtl.Clamp)
            | IR.OP(Op.BorderCtlMirror info, [img]) => (img, info, SOME IndexCtl.Mirror)
            | IR.OP(Op.BorderCtlWrap info, [img]) => (img, info, SOME IndexCtl.Wrap)
            | IR.OP(Op.LoadImage(Ty.ImageTy info, _), _) => (x, info, NONE)
            | rhs => raise Fail (String.concat[
                  "expected image for ", V.toString x, " but found ", IR.RHS.toString rhs
                ])
          (* end case *))

    fun getKernelDst hArg = (case IR.Var.getDef hArg
           of IR.OP(Op.Kernel(h, _), _) => h
            | rhs => raise Fail (String.concat[
                  "expected kernel for ", IR.Var.toString hArg,
                  " but found ", IR.RHS.toString rhs
                ])
          (* end case *))

  (* handleArgs- returns image arguments, info, and border
   * uses the Param_ids for the image, kernel, and tensor
   * and gets the mid-IR vars for each.
   * Transforms the position to index space
   * P is the mid-il var for the (transformation matrix)transpose
   *)
    fun handleArgs (avail, Vid, hid, tid, args) = let
          val vI = List.nth (args, Vid)
          val (vI, info, border) = getImagInfo vI
          val vH = List.nth(args, hid)
          val (vN, vF, vP) = T.worldToIndex{
                  avail = avail, info = info, img = vI, pos = List.nth(args, tid)
                }
          in
            (vI, vH, vN, vF, vP, info, border, ImageInfo.dim info)
          end

  (* lifted Kernel expressions
   * args are axis, ein index_ids that represent differentiation, image dimension,
   * kernel, fractional position, support
   *)
    fun liftKrn (avail, dir, dx, dim, h, vF, s) = let
          val axis = axis dir
          val range = 2*s
        (* build position vector for EvalKernel *)
          val vX = if (dim = 1)
                then vF   (* position is a real type*)
                else AvailRHS.addAssign (
                  avail, concat["v", axis, "_"],
                  Ty.realTy, IR.OP(Op.TensorIndex(Ty.TensorTy[dim], [dir]), [vF]))
          val vPos =  AvailRHS.addAssign (
                avail, concat["kern", axis, "_"],
                Ty.TensorTy[range], IR.OP(Op.BuildPos s, [vX]))
          val nKernEvals = List.length dx + 1
          fun mkEval k = AvailRHS.addAssign (
                avail, concat["keval", axis, "_d", Int.toString k, "_"],
                Ty.TensorTy[range], IR.OP(Op.EvalKernel(range, h, k), [vPos]))
          val vKs = List.tabulate(nKernEvals, fn k => mkEval k)
          in
            case vKs
             of [v] => v (* scalar result *)
              | _ => let
                  val consTy = Ty.TensorTy[nKernEvals, range]
                  in
                    AvailRHS.addAssign (
                      avail, concat["kcons", axis, "_"],
                      consTy, IR.CONS(vKs, consTy))
                  end
            (* end case *)
          end
          
  (* `mkLdVoxel (avail, vI, vN, info, alpha, shape, dim, s, border)`
   *    creates load voxel operator to represent image addressing.  The parameters are
   *        avail       -- available assignments
   *        vI          -- image argument
   *        vN          -- the integer indices into the image (IntTy for 1D, SeqTy for 2D+)
   *        info        -- image info
   *        alpha       -- ein variable indices that represent the shape of a tensor field
   *        shape       -- binding of alpha
   *        dim         -- the dimension of the image
   *        s           -- half the support of the reconstruction kernel
   *        border      -- optional border control
   *)
    fun mkLdVoxel (avail, vI, vN, info, alpha, shape, dim, s, border) = let
       (* creates lb int *)
          val vLb = AvailRHS.addAssign (avail, "lit", Ty.intTy,
                IR.LIT(Literal.Int(IntInf.fromInt(1 - s))))
        (* the index argument to LoadVoxels; this is a single integer for 1D images *)
          val idxs = if (dim = 1)
                then AvailRHS.addAssign (avail, "idx", Ty.intTy, IR.OP(Op.IAdd, [vN, vLb]))
                else let
                  val seqTy = Ty.SeqTy(Ty.intTy, SOME dim)
               (* create sequence n_0 +lb .. n_1+lb *)
                  fun f i = let
                        val vA = AvailRHS.addAssign (
                              avail, "idx", Ty.intTy, IR.LIT(Literal.Int (IntInf.fromInt i)))
                        val vB = AvailRHS.addAssign (
                              avail, concat["n", axis i, "_"],
                              Ty.intTy, IR.OP(Op.Subscript seqTy, [vN, vA]))
                        in
                          AvailRHS.addAssign (avail, "idx", Ty.intTy, IR.OP(Op.IAdd, [vB, vLb]))
                        end
                  val vNs = List.tabulate (dim, f)
                  in
                    AvailRHS.addAssign (avail, "seq", seqTy, IR.SEQ(vNs, seqTy))
                  end
        (* image positions *)
          val s'= 2*s
(* DEBUG
         fun f es = String.concatWithMap "," Int.toString es
         fun g es = String.concatWithMap ","
                (fn (E.V e) => "v"^Int.toString e | E.C(c) => "c"^Int.toString c) es
*)
          val supportshape = List.tabulate (dim, fn _ => s')
          val ldty = Ty.TensorTy(shape @ supportshape)
          val op1 = (case border
                 of NONE => Op.LoadVoxels (info, s')
                  | SOME b => Op.LoadVoxelsWithCtl (info, s', b)
                (* end case *))
          in
            AvailRHS.addAssign (avail, "ldvox", ldty, IR.OP(op1, [vI, idxs]))
          end

  (* fieldReconstruction expands the body for the probed field *)
    fun fieldReconstruction (avail, sx, alpha, shape, dx,  Vid, Vidnew, kid, hid, tid, args) = let
          val (vI, vH, vN, vF, vP, info, border, dim) = handleArgs (avail, Vid, hid, tid, args)
          val h = getKernelDst vH
          val s = Kernel.support h
        (* creating summation Index *)
          val vs = List.tabulate (dim, fn i => (i +sx))
          val esum = List.map (fn i => (i, 1-s, s)) vs
        (* represent image in ein expression with tensor *)
          val imgexp = E.Img(Vidnew, alpha, List.map (fn i=> E.Value i)  vs, s)
        (* create load voxel operator for image *)
          val vLd = mkLdVoxel (avail, vI, vN, info, alpha, shape, dim, s, border)
(* DEBUG
          fun f es = String.concatWithMap "," Int.toString es
          fun g es = String.concatWithMap ","
                (fn (E.V e) => "v"^Int.toString e | E.C(c) => "c"^Int.toString c) es
          val Ty.TensorTy cat = V.ty vLd
          val _ = print(String.concat[
                  "\n","after load voxel ", f cat, " = ", V.name(vLd),
                  " alpha = ", g alpha, " dim:", f[dim]," support: ", f[s]
                ])
*)
        (* create kernel body *)
          fun createKrn (0,  krnexp, vAs) = (krnexp, vAs)
            | createKrn (dir, krnexp, vAs) = let
                val dir' = dir-1
              (* ein expression *)
                val deltas = List.map (fn e => (E.C dir', e)) dx
                val kexp0 = E.Krn(kid+dir, deltas, dir)
              (* evalkernel operators *)
                val vA = liftKrn (avail, dir', dx, dim, h, vF, s)
                in
                  createKrn (dir', kexp0::krnexp, vA::vAs)
                end
        (* final ein expression body to represent field reconstruction *)
          val (krnexp, vKs) = createKrn (dim, [], [])
          val exp =  E.Sum(esum, E.Opn(E.Prod, imgexp::krnexp))
          in
           (vLd::vKs, vP,  exp)
          end

   (* getsumshift:sum_indexid list* int list-> int
    * get fresh/unused index_id, returns int
    *)
    fun getsumshift ([], n) = n
      | getsumshift (sx, n) = let
          val (v, _, _) = List.hd(List.rev sx)
          in
            v+1
          end

  (* formBody:ein_exp->ein_exp *)
    fun formBody (E.Sum([],e)) = formBody e
      | formBody (E.Sum(sx,e)) = E.Sum(sx,formBody e)
      | formBody (E.Opn(E.Prod, [e])) = e
      | formBody e = e

  (* silly change in order of the product to match vis branch WorldtoSpace functions *)
    fun multiPs (Ps, sx, body) = let
          val exp = (case Ps
                 of [P0, P1, P2] => [P0, P1, P2, body]
                  | [P0, P1, P2, P3] => [P0, P1, P2, P3, body]
                  | _ => body::Ps
                (* end case *))
          in
            formBody(E.Sum(sx, E.Opn(E.Prod, exp)))
          end

  (* arrangeBody - function changes the ordering of multiplication
   * to match vis12 branch and pass regression tests
   *)
    fun arrangeBody (body, Ps, newsx, exp) = (case body
           of E.Sum(sx, E.Probe _ ) => (true, multiPs(Ps, sx@newsx,exp))
            | E.Sum(sx, E.Opn(E.Prod,[eps0,E.Probe _ ])) =>
                (false, E.Sum(sx, E.Opn(E.Prod, [eps0, multiPs(Ps, newsx,exp)])))
            | E.Probe _ => (true, multiPs(Ps, newsx, exp))
            | _ => raise Fail "impossible"
          (* end case *))

    (* replaceProbe:ein_exp* params *midIR.var list * int list* sum_id list
            -> ein_exp* *code
    * Transforms position to world space
    * transforms result back to index_space
    * rewrites body
    * replace probe with expanded version
    *)
    fun replaceProbe (avail, (y, IR.EINAPP(Ein.EIN{params, index, body}, args)), probe, sx) = let
        (* tensor ids for position, transform matrix P, and kernel terms*)

          val pid = length params
          val Vidnew = pid+1
          val kid = Vidnew
          val E.Probe(E.Conv(Vid, alpha, hid, dx), E.Tensor(tid, _)) = probe
          val E.IMG(dim, shape) = List.nth(params, Vid)
          val freshIndex = getsumshift (sx, length index)
          val (dx', tshape, sx', Ps) = T.imageToWorld (freshIndex, dim, dx, pid)
          val sxn = freshIndex + length dx' (*next available index id *)
          val (args', vP, probe') = fieldReconstruction (
                avail, sxn, alpha, shape, dx', Vid, Vidnew, kid, hid, tid, args)
        (* add new params transformation matrix (Pid), image param, and kernel ids *)
          val pP = E.TEN(true, [dim, dim])
          val pV = List.nth(params, Vid)
          val pK = List.tabulate(dim,fn _=> E.KRN)
          val params' = params @ (pP::pV::pK)
          val (_, body') = arrangeBody (body, Ps, sx', probe')
          val einapp = (y, IR.EINAPP(mkEin(params', index, body'), args @ (vP::args')))
          in
            AvailRHS.addAssignToList (avail, einapp)
          end

    (* multply probe by transformation matrix and split product operation
    input is a differentiated field of the form
    eF = ∇_dx F_α
    variable differentiation indices are transformed
    (dx′,dx_tshape, Ps) = Transform(dx)
    et is a tensor that represents probe
    et =  T_tshape
    tshape(e_F) = {α+dx_tshape}
    multply probe (et) by transformation matrix (Ps)
    eout = et *Ps
    ein0 = λ(T,P)⟨eout⟩ij(Tx,P)
    The next part of the compler reconstructs probed field ec to lower IR
    ec = Clean(∇dx′Fα)−→∇dx′′Fα′′
    Tx = λ(F,x)⟨ec(x)⟩ij(F,x)
    *)
    fun createEinApp (body, alpha, index, freshIndex, dim, dx, sx) = let
          val Pid = 0
          val tid = 1
          val (dx', dx_tshape, newsx, Ps) = T.imageToWorld(freshIndex, dim, dx, Pid)
        (* need to rewrite dx *)
          val sxx = sx@newsx
          fun filterAlpha [] = []
            | filterAlpha (E.C _::es) = filterAlpha es
            | filterAlpha (e1::es) = e1::(filterAlpha es)
          val tshape = (filterAlpha (alpha))@ dx_tshape
          val et = E.Tensor(tid, tshape)
          val (splitvar, eout) = arrangeBody(body, Ps, newsx, et)
          val (_, sizes, ec) = (case sxx
                of [] => ([], index, E.Conv(0, alpha, 1, dx'))
                | _ => CleanIndex.clean(E.Conv(0, alpha, 1, dx'), index, sxx)
                (* end case *))
          val params = [E.TEN(true, [dim,dim]), E.TEN(true, sizes)]
          val ein0 = mkEin(params, index, eout)
          (* clean index *)
          val E.Conv(_, alpha', _, dx) = ec
          in
            (splitvar, ein0, sizes, dx, alpha')
          end

  (* floats the reconstructed field term *)
    fun liftProbe (avail, (y, IR.EINAPP(Ein.EIN{params, index, body}, args)), probe, sx) = let
          val E.Probe(E.Conv(Vid, alpha, hid, dx), E.Tensor(tid, _)) = probe
          val freshIndex = getsumshift(sx, length(index))
          val E.IMG(dim, shape) = List.nth(params, Vid)
        (* transform T*P*P..Ps *)
          val (splitvar, ein0, sizes, dx, alpha') =
                createEinApp (body, alpha, index, freshIndex, dim, dx, sx)
          val vT = V.new ("TPP", Ty.tensorTy sizes)
        (* reconstruct the lifted probe *)
        (* making params args: image, position, and kernel ids *)
          val kid = 0 (* params used *)
          val params' = List.nth(params,Vid)::(List.tabulate(dim,fn _=> E.KRN))
        (* create body for ein expression *)
          val sxn = length sizes (*next available index id *)
          val Vidnew = 0
          val (args', vP, probe') =
                fieldReconstruction (avail, sxn, alpha', shape, dx,  Vid, Vidnew, kid, hid, tid, args)
          val einApp1 = IR.EINAPP(mkEin(params', sizes, probe'), args')
          val einr= mkEin(params', sizes, probe')
        (* transform T*P*P..Ps *)
          val rtn0 = if splitvar
                then FloatEin.transform(y, EinSums.transform ein0, [vP, vT])
                else [(y, IR.EINAPP(ein0, [vP, vT]))]
          in
            List.app (fn e => AvailRHS.addAssignToList(avail, e)) (((vT, einApp1)::(rtn0)))
          end

  (* expandEinOp: code->  code list
   * A this point we only have simple ein ops
   * Looks to see if the expression has a probe. If so, replaces it.
   *)
    fun expand avail (e as (_, IR.EINAPP(Ein.EIN{body, ...}, _))) = (case body
           of (E.Probe(E.Conv(_, _, _, []) ,_)) =>
                replaceProbe (avail, e, body, [])
            | (E.Probe(E.Conv(_, alpha, _, dx) ,_)) =>
                liftProbe (avail, e, body, []) (*scans dx for contant*)
            | (E.Sum(sx, p as E.Probe(E.Conv(_, _, _, []), _))) =>
                replaceProbe (avail, e, p, sx)  (*no dx*)
            | (E.Sum(sx, p as E.Probe(E.Conv(_, [], _, dx), _))) =>
                liftProbe (avail, e, p, sx) (*scalar field*)
            | (E.Sum(sx, E.Probe p)) =>
                replaceProbe (avail, e, E.Probe p, sx)
            | (E.Sum(sx, E.Opn(E.Prod, [eps, E.Probe p]))) =>
                replaceProbe (avail, e, E.Probe p, sx)
            | _ => AvailRHS.addAssignToList (avail, e)
          (* end case *))

  end (* ProbeEin *)
