(* translate-basis.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * Translation for basis operations in Simple AST to HighIR code
 *)

structure TranslateBasis : sig

  (* translate(lhs, f, mvs, args) translates the application of f (specialized
   * to the instantiated meta variables mvs) to a list of SSA assignments in
   * reverse order.
   *)
    val translate : (HighIR.var * Var.t * SimpleTypes.meta_arg list * HighIR.var list)
          -> HighIR.assignment list

  end = struct

    structure BV = BasisVars
    structure IR = HighIR
    structure DstTy = HighTypes
    structure Op = HighOps
    structure Ty = SimpleTypes
    structure VTbl = Var.Tbl
    structure Mk = MkOperators

    fun trType (Ty.TY ty) = TranslateTy.tr ty
      | trType _ = raise Fail "expected type"
    fun dimVarToInt (Ty.DIM d) = d
      | dimVarToInt _ = raise Fail "expected dim"
    fun dimVarToTensor dv = DstTy.tensorTy[dimVarToInt dv]
    fun dimVarToMatrix dv = let
          val d = dimVarToInt dv
          in
            DstTy.tensorTy[d, d]        (* square matrix type *)
          end
    fun shapeVarToTensor (Ty.SHAPE shp) = DstTy.tensorTy shp
      | shapeVarToTensor _ = raise Fail "expected shape"

    fun assign (y, rator, xs) = [IR.ASSGN(y, IR.OP(rator, xs))]

    fun simpleOp rator (y, [], xs) = assign (y, rator, xs)
      | simpleOp rator (y, mvs, xs) = raise Fail(concat[
            "simpleOp (", Op.toString rator, ") (", IR.Var.toString y, ", [",
            String.concatWithMap "," Ty.metaArgToString mvs, "], [",
            String.concatWithMap "," IR.Var.toString xs, "])"
          ])

    fun tensorOp rator (y, [sv], xs) = assign (y, rator(shapeVarToTensor sv), xs)

    fun vectorOp rator (y, [dv], xs) = assign (y, rator(dimVarToTensor dv), xs)

  (* utility functions for synthesizing eigenvector/eigenvalue code *)
    fun eigenVec (rator, dim) = let
          val ty = DstTy.SeqTy(DstTy.realTy, SOME dim)
          in
            fn (y, _, [m]) => let
                val v = IR.Var.new("evals", ty)
                in
                  [IR.MASSGN([v, y], IR.OP(rator, [m]))]
                end
          end
    fun eigenVal (rator, dim) = let
          val ty = DstTy.SeqTy(DstTy.vecTy dim, SOME dim)
          in
            fn (y, _, [m]) => let
                val v = IR.Var.new("evecs", ty)
                in
                  [IR.MASSGN([y, v], IR.OP(rator, [m]))]
                end
          end

    fun assignEin (y, rator, xs) = IR.ASSGN(y, IR.EINAPP(rator, xs))

    fun simpleEOp rator (y, _, xs) = [assignEin(y, rator, xs)]

  (* build a table that maps Basis variables to their translation functions *)
    val tbl : ((IR.var * Ty.meta_arg list * IR.var list) -> IR.assignment list) VTbl.hash_table = let
          val tbl = VTbl.mkTable (128, Fail "Translate table")
          fun insert (id, def) = (case VTbl.find tbl id
                 of NONE => VTbl.insert tbl (id, def)
                  | SOME _ => raise Fail("duplicate definition of " ^ Var.nameOf id)
                (* end case *))
          in
            List.app insert [
                (BV.lt_ii,              simpleOp(Op.LT DstTy.IntTy)),
                (BV.lt_rr,              simpleOp(Op.LT DstTy.realTy)),
                (BV.lte_ii,             simpleOp(Op.LTE DstTy.IntTy)),
                (BV.lte_rr,             simpleOp(Op.LTE DstTy.realTy)),
                (BV.gte_ii,             simpleOp(Op.GTE DstTy.IntTy)),
                (BV.gte_rr,             simpleOp(Op.GTE(DstTy.realTy))),
                (BV.gt_ii,              simpleOp(Op.GT DstTy.IntTy)),
                (BV.gt_rr,              simpleOp(Op.GT(DstTy.realTy))),
                (BV.equ_bb,             simpleOp(Op.EQ DstTy.BoolTy)),
                (BV.equ_ii,             simpleOp(Op.EQ DstTy.IntTy)),
                (BV.equ_ss,             simpleOp(Op.EQ DstTy.StringTy)),
                (BV.equ_rr,             simpleOp(Op.EQ(DstTy.realTy))),
                (BV.neq_bb,             simpleOp(Op.NEQ DstTy.BoolTy)),
                (BV.neq_ii,             simpleOp(Op.NEQ DstTy.IntTy)),
                (BV.neq_ss,             simpleOp(Op.NEQ DstTy.StringTy)),
                (BV.neq_rr,             simpleOp(Op.NEQ(DstTy.realTy))),
                (BV.add_ii,             simpleOp Op.IAdd),
                (BV.sub_ii,             simpleOp Op.ISub),
                (BV.mul_ii,             simpleOp Op.IMul),
                (BV.div_ii,             simpleOp Op.IDiv),
                (BV.op_mod,             simpleOp Op.IMod),
                (BV.neg_i,              simpleOp Op.INeg),
                (BV.add_tt,             fn (y, [shp], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          val rator = Mk.addTT(dd1)
                                          in
                                            [assignEin(y, rator, xs)]
                                          end),
                (BV.add_ff,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.addFF(d, dd), xs)]),
                (BV.add_ft,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], [f, s]) =>
                                          [assignEin(y, Mk.addTF(d, dd), [s, f])]),
                (BV.add_tf,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.addTF(d, dd), xs)]),
                (BV.sub_tt,             fn (y, [shp], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          val rator = Mk.subTT dd1
                                          in
                                            [assignEin(y, rator, xs)]
                                          end),
                (BV.sub_ff,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.subFF(d, dd), xs)]),
                (BV.sub_ft,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], [x1, x2]) =>
                                          [assignEin(y, Mk.subFT(d, dd), [x2, x1])]),
                (BV.sub_tf,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.subTF(d, dd), xs)]),
                (BV.mul_rr,             fn (y, _, args) => [assignEin(y, Mk.mulRR, args)]),
                (BV.mul_rt,             fn (y, [shp], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          val rator = Mk.mulRT(dd1)
                                          in
                                            [assignEin(y, rator, xs)]
                                          end),
                (BV.mul_tr,             fn (y, [shp], [t, r]) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          val rator = Mk.mulRT dd1
                                          in
                                            [assignEin(y, rator, [r, t])]
                                          end),
                (BV.mul_rf,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.mulRF(d, dd), xs)]),
                (BV.mul_fr,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], [f, s]) =>
                                          [assignEin(y, Mk.mulRF(d, dd), [s, f])]),
                (BV.mul_ss,             fn (y, [_, Ty.DIM d], xs) =>
                                          [assignEin(y, Mk.mulSS d, xs)]),
                (BV.mul_sf,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.mulSF(d, dd), xs)]),
                (BV.mul_fs,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], [s, f]) =>
                                          [assignEin(y, Mk.mulSF(d, dd), [f, s])]),
                (BV.mul_st,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.mulST(d, dd), List.rev xs)]),
                (BV.mul_ts,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.mulST(d, dd), xs)]),
                (BV.div_rr,             fn (y, _, args) => [assignEin(y, Mk.divRR, args)]),
                (BV.div_tr,             fn (y, [shp], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          val rator = Mk.divTR dd1
                                          in
                                            [assignEin(y, rator, xs)]
                                          end),
                (BV.div_fr,             fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.divFR(d, dd), xs)]),
                (BV.div_fs,             fn (y, [_, _, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.divFS(d, dd), xs)]),
                (BV.div_ss,             fn (y, [_, Ty.DIM d], xs) =>
                                          [assignEin(y, Mk.divSS d, xs)]),
                (BV.div_ts,             fn (y, [_,Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.divTS(d, dd), xs)]),
                (BV.pow_ri,             fn (y, _, [f, n]) => case IR.Var.getDef n
                                           of IR.LIT(Literal.Int n) =>
                                                [assignEin(y, Mk.powTI( IntInf.toInt n), [f])]
                                            | _ => [IR.ASSGN(y, IR.OP(Op.Power, [f, n]))]
                                          (* end case *)),  
                (BV.pow_rr,             fn (y, _, args) => assign(y, Op.MathFn MathFns.POW, args)),
                (BV.pow_si,             fn (y, [_, Ty.DIM d1], [f, n]) => let
                                          fun getN x  = (case IR.Var.getDef x
                                                 of IR.LIT(Literal.Int n) => IntInf.toInt n
(* FIXME: there is no guarantee that n will be constant! *)
                                                  | _ => raise Fail "impossible"
                                                (* end case *))
                                          in
                                            [assignEin(y, Mk.powFI(d1, getN n), [f])]
                                          end),
                (BV.curl2D,             simpleEOp Mk.curl2d),
                (BV.curl3D,             simpleEOp Mk.curl3d),
                (BV.convolve_vk,        fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.conv(d, dd), xs)]),
                (BV.convolve_kv,        fn (y, [_, Ty.DIM d, Ty.SHAPE dd], [k, v]) =>
                                          [assignEin(y, Mk.conv(d, dd), [v, k])]),
                (BV.neg_t,              fn (y, [shp], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          val rator = Mk.negTT dd1
                                          in
                                            [assignEin(y, rator, xs)]
                                          end),
                (BV.neg_f,              fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.negFF(d, dd), xs)]),
                (BV.op_probe,           fn (y, [_, Ty.DIM d, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, (Mk.probe(dd, d)), xs)]),
                (BV.op_D,               fn (y, [_, Ty.DIM d], xs) =>
                                            if (d = 2) orelse (d = 3)
                                            then [assignEin(y, Mk.grad [d], xs)]
                                            else [assignEin(y, Mk.gradConstant [d], xs)]),
                (BV.op_Dotimes,         fn (y, [_, Ty.DIM d1, Ty.SHAPE dd, Ty.DIM d2], xs) =>
                                          [assignEin(y, Mk.dotimes(d1, dd@[d2]), xs)]),
                (BV.op_Ddot,            fn (y, [_, Ty.DIM d1,  Ty.SHAPE dd, Ty.DIM d2], xs) =>
                                          [assignEin(y, Mk.divergence(d1, dd), xs)] ),
                (BV.op_norm_i,          simpleOp Op.IAbs),
                (BV.op_norm_t,          fn (y, [sv], xs) => (case sv
                                           of Ty.SHAPE dd => [assignEin(y, Mk.normT dd, xs)]
                                          (* end case *))),
                (BV.op_norm_f,          fn (y, [ _,Ty.DIM d1, Ty.SHAPE dd1], xs) =>
                                           [assignEin(y, Mk.normF(d1, dd1), xs)]),
                (BV.op_not,             simpleOp Op.BNot),
                (BV.op_cross3_tt,       simpleEOp Mk.cross3TT),
                (BV.op_cross2_tt,       simpleEOp Mk.cross2TT),
                (BV.op_cross2_ff,       simpleEOp Mk.cross2FF),
                (BV.op_cross3_ff,       simpleEOp Mk.cross3FF),
                (BV.op_cross2_ft,       simpleEOp Mk.cross2FT),
                (BV.op_cross3_ft,       simpleEOp Mk.cross3FT),
                (BV.op_cross2_tf,       simpleEOp Mk.cross2TF),
                (BV.op_cross3_tf,       simpleEOp Mk.cross3TF),
                (BV.op_outer_tt,        fn (y, [sh1, sh2, _], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor sh1
                                          val ty2 as DstTy.TensorTy dd2 = shapeVarToTensor sh2
                                          in
                                            [assignEin(y, (Mk.outerTT(dd1, dd2)), xs)]
                                          end),
                (BV.op_outer_tf,        fn (y, [_, Ty.DIM d, sh1, Ty.SHAPE dd2, _], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor sh1
                                          in
                                            [assignEin(y, Mk.outerTF(d, dd1, dd2), xs)]
                                          end),
                (BV.op_outer_ft,        fn (y, [_, Ty.DIM d, Ty.SHAPE dd1, sh2, _], xs) => let
                                          val ty1 as DstTy.TensorTy dd2 = shapeVarToTensor sh2
                                          in
                                            [assignEin(y, Mk.outerFT(d, dd1, dd2), xs)]
                                          end),
                (BV.op_outer_ff,        fn (y, [_, _, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, _], xs) =>
                                          [assignEin(y, Mk.outerFF(d, dd1, dd2), xs)]),
                (BV.op_inner_tt,        fn (y, [sh1, sh2, _], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor sh1
                                          val ty2 as DstTy.TensorTy dd2 = shapeVarToTensor sh2
                                          in
                                            [assignEin(y, (Mk.innerTT(dd1, dd2)), xs)]
                                          end),
                (BV.op_inner_tf,        fn (y, [_, Ty.DIM d, sh1, Ty.SHAPE dd2, _], xs) =>let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor sh1
                                          in
                                            [assignEin(y, Mk.innerTF(dd1, d, dd2), xs)]
                                          end),
                (BV.op_inner_ft,        fn (y, [_, Ty.DIM d, Ty.SHAPE dd1, sh2, _], xs) =>let
                                          val ty1 as DstTy.TensorTy dd2 = shapeVarToTensor sh2
                                          in
                                            [assignEin(y, Mk.innerFT(dd1, d, dd2), xs)]
                                          end),
                (BV.op_inner_ff,        fn (y,  [_, _, Ty.DIM d, Ty.SHAPE dd1, Ty.SHAPE dd2, _], xs) =>
                                          [assignEin(y, Mk.innerFF(dd1, d, dd2), xs)]),
                (BV.op_colon_tt,        fn (y, [sh1, sh2, _], xs) => let
                                          val ty1 as DstTy.TensorTy dd1 = shapeVarToTensor sh1
                                          val ty2 as DstTy.TensorTy dd2 = shapeVarToTensor sh2
                                          in
                                            [assignEin(y, Mk.colonTT(dd1, dd2), xs)]
                                          end),
                (BV.op_colon_ft,        fn (y, [_, Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE dd2, _], xs) =>
                                          [assignEin(y, Mk.colonFT(d, dd1, dd2), xs)]),
                (BV.op_colon_tf,        fn (y, [_, Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE dd2, _], xs) =>
                                          [assignEin(y, Mk.colonTF(d, dd1, dd2), xs)]),
                (BV.op_colon_ff,        fn (y, [_, Ty.SHAPE dd1, Ty.DIM d, Ty.SHAPE dd2, _], xs) =>
                                          [assignEin(y, Mk.colonFF(d, dd1, dd2), xs)]),
                (*  modulate is vector * vector pointwise multiplication *)
                (BV.fn_modulate_tt,     fn (y, [Ty.DIM dd1], xs) => let
                                          in [assignEin(y, (Mk.modulateTT [dd1]),xs)]
                                          end),
                (BV.fn_modulate_ff,     fn (y,[ _,Ty.DIM d1, Ty.DIM dd], xs) =>
                                          [assignEin(y, (Mk.modulateFF ([dd],d1)),xs)]),
                (BV.fn_modulate_tf,     fn (y,[ _,Ty.DIM d1, Ty.DIM dd], xs) =>
                                          [assignEin(y, (Mk.modulateTF([dd], d1)),xs)]),
                (BV.fn_modulate_ft,     fn (y,[ _,Ty.DIM d1, Ty.DIM dd], xs) =>
                                          [assignEin(y, (Mk.modulateFT ([dd],d1)),xs)]),
                (BV.fn_normalize_t,     fn (y, [shp], xs) => let
                                          val DstTy.TensorTy dd1 = shapeVarToTensor shp
                                          in
                                            case (dd1, xs)
                                             of ([], [x]) => raise Fail "fn_normalize_t on scalar"
                                              | (_, [_]) => [assignEin(y, Mk.normalizeTT dd1, xs@xs)]
                                            (* end case *)
                                          end),
                (BV.fn_normalize_f,     fn (y, [ _,Ty.DIM d1, Ty.SHAPE dd1], xs) => (case (dd1, xs)
                                           of ([], [arg0]) => raise Fail "fn_normalize_f on scalar"
                                            | (_, [arg0]) => [assignEin(y, Mk.normalizeFF(d1, dd1), xs@xs)]
                                          (* end case *))),
                (BV.fn_trace_t,         fn (y, [Ty.DIM d], xs) =>
                                          [assignEin(y, (Mk.traceT d), xs)]),
                (BV.fn_trace_f,         fn (y, [_, Ty.DIM d, Ty.DIM d2, Ty.SHAPE dd], xs) =>
                                          [assignEin(y, Mk.traceF(d, d2, dd), xs)]),
                (BV.fn_transpose_t,     fn (y, [Ty.DIM d1, Ty.DIM d2], xs) =>
                                          [assignEin(y, (Mk.transposeT [d1, d2]), xs)]),
                (BV.fn_transpose_f,     fn (y, [_, Ty.DIM d1, Ty.DIM d2, Ty.DIM d3], xs) =>
                                          [assignEin(y, (Mk.transposeF (d1, d2, d3)), xs)]),
(*
                (BV.fn_concat_f2,       fn (y, [_, Ty.DIM d1, Ty.DIM dd1, Ty.DIM dd2, Ty.SHAPE dd3], xs) =>
                                            [assignEin(y, (Mk.concatField2(dd1, dd2)), xs)]),
*)
                (BV.fn_det2_t,          simpleEOp Mk.det2T),
                (BV.fn_det3_t,          simpleEOp Mk.det3T),
                (BV.fn_det2_f,          fn (y, [_,Ty.DIM d1], xs) =>
                                          [assignEin(y, (Mk.det2F (d1)), xs)]),
                (BV.fn_det3_f,          fn (y, [_,Ty.DIM d1], xs) =>
                                          [assignEin(y, (Mk.det3F (d1)), xs)]),
                (BV.fn_inv1_t,          simpleEOp Mk.invR),
                (BV.fn_inv2_t,          simpleEOp Mk.inv2T),
                (BV.fn_inv3_t,          fn (y, _ , [x]) => let
                                          val shape = [3, 3]
                                          val trace = IR.Var.new("t1", DstTy.TensorTy [])
                                          val trace_sq = IR.Var.new("t2", DstTy.TensorTy[])
                                          val dotT = IR.Var.new("t3", DstTy.TensorTy shape)
                                          val dot_trace = IR.Var.new("t4", DstTy.TensorTy [])
                                          val t5 = IR.Var.new("t5", DstTy.TensorTy [])
                                          val t6 = IR.Var.new("t6", DstTy.TensorTy shape)
                                          val t7 = IR.Var.new("t7", DstTy.TensorTy shape)
                                          val t8 = IR.Var.new("t8", DstTy.TensorTy shape)
                                          val t9 = IR.Var.new("t9", DstTy.TensorTy shape)
                                          val id = IR.Var.new("t9", DstTy.TensorTy shape)
                                          val num = IR.Var.new("t10", DstTy.TensorTy shape)
                                          val denom = IR.Var.new("t11", DstTy.TensorTy [])
                                          val trace_op = Mk.traceT 3
                                          val scaleRT = Mk.mulRT shape
                                          val scaleRR = Mk.mulRR
                                          val subRR  = Mk.subRR
                                          in [
                                            assignEin(trace, trace_op, [x]),
                                            assignEin(trace_sq, scaleRR, [trace, trace]),
                                            assignEin(dotT, Mk.innerTT(shape, shape), [x, x]),
                                            assignEin(dot_trace, trace_op, [dotT]),
                                            assignEin(t5, subRR, [trace_sq, dot_trace]),
                                            assignEin(t6, Mk.scaleIdT 3, [t5]),
                                            assignEin(t7,  Mk.halfT 3, [t6]),
                                            assignEin(t8, scaleRT, [trace, x]),
                                            assignEin(t9, Mk.subTT shape, [t7, t8]),
                                            assignEin(num, Mk.addTT shape, [t9, dotT]),
                                            assignEin(denom, Mk.det3T, [x]),
                                            assignEin(y, Mk.divTR shape, [num, denom])
                                          ] end),
                (BV.fn_inv1_f,          fn (y, [_, Ty.DIM dim], xs) =>
                                            [assignEin(y, Mk.invS dim, xs)]),
                (BV.fn_inv2_f,          fn (y, [_, Ty.DIM dim], xs) =>
                                          [assignEin(y, Mk.inv2F dim, xs)]),
                (BV.fn_inv3_f,          fn (y, [_, Ty.DIM dim], [x]) => let
                                          val shape = [3, 3]
                                          val l = 3
                                          val fty = DstTy.FieldTy
                                          val trace = IR.Var.new("t1", fty)
                                          val trace_sq = IR.Var.new("t2", fty)
                                          val dotT = IR.Var.new("t3", fty)
                                          val dot_trace = IR.Var.new("t4", fty)
                                          val t5 = IR.Var.new("t5", fty)
                                          val t6 = IR.Var.new("t6", fty)
                                          val t7 = IR.Var.new("t7", fty)
                                          val t8 = IR.Var.new("t8", fty)
                                          val t9 = IR.Var.new("t9", fty)
                                          val id = IR.Var.new("t9", fty)
                                          val num = IR.Var.new("t10", fty)
                                          val denom = IR.Var.new("t11", fty)
                                          val trace_op = Mk.traceF(dim, l, [])
                                          val scaleSF = Mk.mulSF(dim, shape)
                                          val scaleSS = Mk.mulSS dim
                                          val subSS  = Mk.subFF(dim, [])
                                          in [
                                            assignEin(trace, trace_op, [x]),
                                            assignEin(trace_sq, scaleSS, [trace, trace]),
                                            assignEin(dotT, Mk.innerFF(shape,dim, shape), [x, x]),
                                            assignEin(dot_trace, trace_op, [dotT]),
                                            assignEin(t5, subSS, [trace_sq, dot_trace]),
                                            assignEin(t6, Mk.scaleIdF(dim, l), [t5]),
                                            assignEin(t7, Mk.halfF(dim, l), [t6]),
                                            assignEin(t8, scaleSF, [trace, x]),
                                            assignEin(t9, Mk.subFF(dim, shape), [t7, t8]),
                                            assignEin(num, Mk.addFF(dim, shape), [t9, dotT]),
                                            assignEin(denom, Mk.det3F dim, [x]),
                                            assignEin(y, Mk.divFS(dim, shape), [num, denom])
                                          ] end),
                (BV.comp,               fn (y, [_, Ty.DIM d0, Ty.SHAPE s0, Ty.DIM d1, Ty.SHAPE s1], xs) =>
                                          [assignEin(y, Mk.composition(d0, s0, d1, s1), xs)]),
                (BV.fn_sqrt_r,          fn (y, _, xs) =>
                                          [assignEin(y, Mk.sqrtR, xs)]),
                (BV.fn_sqrt_s,          fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.sqrtF d1, xs)]),
                (BV.fn_cos_r,           fn (y, _, xs) =>
                                          [assignEin(y, Mk.cosR, xs)]),
                (BV.fn_cos_s,           fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.cosF d1, xs)]),
                (BV.fn_acos_r,           fn (y, _, xs) =>
                                          [assignEin(y, Mk.acosR, xs)]),
                (BV.fn_acos_s,          fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.acosF d1, xs)]),
                (BV.fn_sin_r,           fn (y, _, xs) =>
                                          [assignEin(y, Mk.sinR, xs)]),
                (BV.fn_sin_s,           fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.sinF d1, xs)]),
                (BV.fn_asin_r,          fn (y, _, xs) =>
                                          [assignEin(y, Mk.asinR, xs)]),
                (BV.fn_asin_s,          fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.asinF d1, xs)]),
                (BV.fn_tan_r,           fn (y, _, xs) =>
                                          [assignEin(y, Mk.tanR, xs)]),
                (BV.fn_tan_s,           fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.tanF d1, xs)]),
                (BV.fn_atan_r,          fn (y, _, xs) =>
                                          [assignEin(y, Mk.atanR, xs)]),
                (BV.fn_atan_s,          fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.atanF d1, xs)]),
                (BV.fn_exp_r,           fn (y, _, xs) =>
                                          [assignEin(y, Mk.expT, xs)]),
                (BV.fn_exp_s,           fn (y, [_, Ty.DIM d1], xs) =>
                                          [assignEin(y, Mk.expF d1, xs)]),
                (BV.clamp_rrt,          fn (y, [sv, dv], xs) => let
                                          val Ty.SHAPE shp = sv
                                          val d = dimVarToInt dv
                                          val alpha =  shp @ [d]
                                          in [assignEin(y, Mk.clampRRT alpha, xs)] end),
                (BV.clamp_ttt,          fn (y, [Ty.SHAPE shp], xs) =>
                                          [assignEin(y, Mk.clampTTT shp, xs)]),
                (BV.lerp3,              fn (y, [Ty.SHAPE sv], xs) =>
                                          [assignEin(y, Mk.lerp3 sv, xs)]),
                (BV.lerp5,              fn (y, [Ty.SHAPE sv], xs) =>
                                          [assignEin(y, Mk.lerp5 sv, xs)]),
                (BV.clerp3,              fn (y, [Ty.SHAPE sv], xs) =>
                                          [assignEin(y, Mk.clerp3 sv, xs)]),
                (BV.clerp5,              fn (y, [Ty.SHAPE sv], xs) =>
                                          [assignEin(y, Mk.clerp5 sv, xs)]),
                (BV.evals2x2,           eigenVal (Op.Eigen2x2, 2)),
                (BV.evals3x3,           eigenVal (Op.Eigen3x3, 3)),
                (BV.evecs2x2,           eigenVec (Op.Eigen2x2, 2)),
                (BV.evecs3x3,           eigenVec (Op.Eigen3x3, 3)),
                (BV.fn_max_i,           simpleOp (Op.Max DstTy.IntTy)),
                (BV.fn_max_r,           simpleOp (Op.Max DstTy.realTy)),
                (BV.fn_min_i,           simpleOp (Op.Min DstTy.IntTy)),
                (BV.fn_min_r,           simpleOp (Op.Min DstTy.realTy)),
                (BV.i2r,                simpleOp Op.IntToReal),
                (BV.identity,           fn (y, [Ty.DIM d], xs) =>
                                          [assignEin(y, Mk.identity d, xs)]),
                (BV.zero,               fn (y, [Ty.SHAPE dd], []) =>
                                          [assignEin(y, Mk.zeros dd, [])]),
(*
                (BV.zero,               fn (y, [sv], []) =>
                                          assign(y, Op.Zero(shapeVarToTensor sv), [])),
*)
                (BV.nan,                fn (y, [Ty.SHAPE dd], []) => let
                                            val nan = IR.LIT(Literal.Real(RealLit.nan))
                                            fun mk (y, [], stms) = IR.ASSGN(y, nan) :: stms
                                              | mk (y, d::dd, stms) = let
                                                  val ty = shapeVarToTensor(Ty.SHAPE dd)
                                                  val zs = List.tabulate(d, fn _ => IR.Var.new("_nan", ty))
                                                  in
                                                    IR.ASSGN(y, IR.CONS(zs, IR.Var.ty y)) ::
                                                      List.foldl (fn (z, stms) => mk(z, dd, stms)) stms zs
                                                  end
                                            in
                                              List.rev (mk (y, dd, []))
                                            end),
              (* logical connectives *)
                (BV.and_b,              simpleOp Op.BAnd),
              (* sequence operations *)
                (BV.subscript,          fn (y, [tv, Ty.DIM d], args) =>
                                          assign(y,
                                            Op.Subscript(DstTy.SeqTy(trType tv, SOME d)),
                                            args)),
                (BV.dynSubscript,       fn (y, [tv], args) =>
                                          assign(y, Op.Subscript(DstTy.SeqTy(trType tv, NONE)), args)),
                (BV.at_Td,              fn (y, [tv], args) => assign(y, Op.Prepend(trType tv), args)),
                (BV.at_dT,              fn (y, [tv], args) => assign(y, Op.Append(trType tv), args)),
                (BV.at_dd,              fn (y, [tv], args) => assign(y, Op.Concat(trType tv), args)),
                (BV.range,              fn (y, _, args) => assign(y, Op.Range, args)),
                (BV.fn_length,          fn (y, [tv], [s]) => assign(y, Op.Length(trType tv), [s])),
              (* image operations *)
                (BV.fn_size,            fn (y, [Ty.DIM d, _], [img]) => let
                                          val DstTy.ImageTy info = IR.Var.ty img
                                        (* we extract each dimension separately and then build
                                         * the sequence value
                                         *)
                                          val dims = List.tabulate(d,
                                                fn i => IR.Var.new("i"^Int.toString i, DstTy.IntTy))
                                          fun mkStms ([], _, stms) = stms (* in reverse order! *)
                                            | mkStms (d::dr, i, stms) = mkStms (dr, i+1,
                                                IR.ASSGN(d, IR.OP(Op.ImageDim(info, i), [img])) ::
                                                stms)
                                          in
                                            List.revAppend (mkStms (dims, 0, []), [
                                                IR.ASSGN(y,
                                                  IR.SEQ(dims, DstTy.SeqTy(DstTy.intTy, SOME d)))
                                              ])
                                          end),
              (* spatial queries *)
                (BV.fn_sphere1_r,       fn (y, [tv], args as [p, s]) =>
                                          assign(y, Op.SphereQuery(1, trType tv), args)),
                (BV.fn_sphere2_t,       fn (y, [tv], args as [p, s]) =>
                                          assign(y, Op.SphereQuery(2, trType tv), args)),
                (BV.fn_sphere3_t,       fn (y, [tv], args as [p, s]) =>
                                          assign(y, Op.SphereQuery(3, trType tv), args)),
              (* strand related operations *)
                (BV.fn_numActive,       fn (y, [], []) =>
                                          assign(y, Op.NumStrands StrandSets.ACTIVE, [])),
                (BV.fn_numStable,       fn (y, [], []) =>
                                          assign(y, Op.NumStrands StrandSets.STABLE, [])),
                (BV.fn_numStrands,      fn (y, [], []) =>
                                          assign(y, Op.NumStrands StrandSets.ALL, [])),
              (* math functions that have not been lifted *)
                (BV.fn_atan2_rr,        fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.ATAN2, args)),
                (BV.fn_ceil_r,          fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.CEIL, args)),
                (BV.fn_erf_r,           fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.ERF, args)),
                (BV.fn_erfc_r,          fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.ERFC, args)),
                (BV.fn_floor_r,         fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.FLOOR, args)),
                (BV.fn_fmod_rr,         fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.FMOD, args)),
                (BV.fn_log_r,           fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.LOG, args)),
                (BV.fn_log10_r,         fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.LOG10, args)),
                (BV.fn_log2_r,          fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.LOG2, args)),
                (BV.fn_pow_rr,          fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.POW, args)),
                (BV.fn_round_r,         fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.ROUND, args)),
                (BV.fn_trunc_r,         fn (y, _, args) =>
                                          assign(y, Op.MathFn MathFns.TRUNC, args))
              ];
            tbl
          end

    fun translate (y, f, mvs, xs) = (case VTbl.find tbl f
           of SOME transFn => transFn(y, mvs, xs)
            | NONE => raise Fail("TranslateBasis.translate: unknown basis function " ^ Var.uniqueNameOf f)
          (* end case *))
handle ex => (print(concat["exeption in translate (", IR.Var.toString y, ", ",
Var.uniqueNameOf f, ", ...)\n"]); raise ex)

  end
