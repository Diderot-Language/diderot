(* field-to-low.sml
 *
 * NOTE: this code will need to be changed if we ever want to support different kernels
 * for different axes
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure FieldToLow : sig

  (* expand a MidIR probe to LowIR code.  The arguments are:
   *
   *    avail   -- available LowIR assignments
   *    mapp    -- mapping from iteration indices to deBruijn indices
   *    sx      -- summation bounds
   *    img     -- Image expression
   *    krnargs -- Kernel arguments
   *    args    -- the actual arguments of the enclosing Ein expression
   *)
    val expand : {
            avail : AvailRHS.t,
            mapp : int IntRedBlackMap.map,
            sx : Ein.sumrange list,
            img : Ein.ein_exp,
            krnargs : Ein.ein_exp list,
            args : LowIR.var list
          }  -> LowIR.var

  end = struct

    structure IR = LowIR
    structure Ty = LowTypes
    structure Op = LowOps
    structure Var = LowIR.Var
    structure E = Ein
    structure Mk = MkLowIR
    structure IMap = IntRedBlackMap

  (* Index cons at piece *)
    fun getHolder (args, id, piece) = let
          val t = List.nth(args, id)
          in
            case IR.Var.getDef t
             of IR.CONS(eargs, _) => List.nth(eargs, piece)
              | IR.OP(_, _) => t
              | rhs => raise Fail(String.concat[
                    "getHolder found ", Var.name t, "=", IR.RHS.toString rhs,
                    " at ", Int.toString id
                  ])
            (* end case *)
          end
    (* DEBUG
    fun f es =String.concatWith"," (List.map (fn e => Int.toString(e)) es)
    fun m  e = let
        val x = IR.Var.ty e
        in (case x
            of  Ty.TensorTy(alpha)=> f(alpha)
            | _ => "other type"
        (**) )end
    *)
 
  (* evaluate image Ein expression *)
    fun imgToArgs (avail, mapp, sx, (Vid, alpha, vs, s), args) = let
          val vI = List.nth(args, Vid)
          val beta = List.map (fn id => Mk.lookupMu(mapp, id)) alpha
          val range = 2*s
        (* Index tensor with image shape and position v_beta[n0,n1,n2]*)
        (* change here depending on data layout created by load voxel*)
          fun getIX idxs = (case beta @ idxs
                 of [] => vI
                  | idxs' =>
                    AvailRHS.addAssign (
                      avail, "projIx", Ty.TensorTy[range],
                      IR.OP(Op.ProjectLast(IR.Var.ty vI, idxs'), [vI]))
                (* end case *))
        (* fold f over the integers from 0 to 2*s-1 *)
          fun loop f init = let
                fun lp (i, acc) = if (i < range) then lp (i+1, f(i, acc)) else acc
                in
                  lp (0, init)
                end
          in
          (* note that the length of vs is the dimension of the image *)
            case vs
             of [_] => [getIX []]
              | [_,_] => loop (fn (i, acc) => getIX [i] :: acc) []
              | [_, _, _] => loop (fn (i, acc) => loop (fn (j, acc) => getIX [i, j] :: acc) acc) []
              | _ => raise Fail "unsupported image dimension (not 1, 2, or 3)"
           (* end case *)
          end

  (* Convolution product of Image and Kernel *)
    fun prodImgKrn (avail, imgArg, krnArg, s) = let
        (* Number of arguments for Cons *)
          val range1 = 2*s
          val range0 = range1-1
          fun mkConsInt args = let
                val ty = Ty.TensorTy [range1]
                val rhs = IR.CONS(args, ty)
                in
                  AvailRHS.addAssign (avail, "cons"^"_", ty, rhs)
                end
          fun mkDotVec (a, b) = Mk.vecDot(avail, range1, a, b)
          fun mul2d ([], rest, hy) = mkConsInt (List.rev rest)
            | mul2d (e::es, rest, hy) = let
                val vA = mkDotVec (hy, e)
                in
                  mul2d (es, vA::rest, hy)
                end
          fun mul3d ([], _ , _, rest, hz) = rest
            | mul3d (e1::es, rest, 0, consrest, hz) = let
                val vA = mkDotVec (hz, e1)
                val vD = mkConsInt (rest@[vA])
                in
                  mul3d (es, [], range0, consrest@[vD], hz)
                end
            | mul3d (e1::es, rest, n, consrest, hz) = let
                val vA = mkDotVec (hz, e1)
                in
                  mul3d (es, rest@[vA], n-1, consrest, hz)
                end
        (* create Product by doing case analysis of the dimension*)
          in
            case (krnArg, imgArg)
             of ([h0], [i]) => mkDotVec (i, h0)   (*1-D case*)
              | ([h0, h1], _) => let
                  val vA = mul2d (imgArg, [], h0)
                  in
                    mkDotVec (h1, vA)
                  end
              | ([h0, h1, h2], _) => let
                  val restZ = mul3d (imgArg, [], range0, [], h0)
                  val restY = mul2d (restZ, [], h1)
                  in
                    mkDotVec (h2, restY)
                  end
              | _ =>  raise Fail "Kernel dimensions not between 1-3"
           (* end case *)
          end

  (* expand a MidIR probe to LowIR code. *)
    fun expand {avail, mapp, sx, img = E.Img e1, krnargs, args} = let
          val imgArgs = imgToArgs(avail, mapp, sx, e1, args)
          val imgArgs = List.rev imgArgs
        (* get piece for each kernel *)
          fun getf (E.Krn(id, dels, _)) = let
              (* evaluate dels to integer *)
                val delta = List.foldl (fn((i, j), y) => Mk.evalDelta(mapp, i, j) + y) 0 dels
                in
                  getHolder (args, id, delta)  (* selects variable in holder/cons list *)
                end
            | getf _ = raise Fail "expected kernel"
        (* evaluate kernel expression(s) *)
          val krnArgs = List.map getf krnargs  (* doesn't create code *)
          val krnArgs = krnArgs
          val rtn = prodImgKrn (avail, imgArgs, krnArgs, #4 e1)
          in
            rtn
          end

  end
