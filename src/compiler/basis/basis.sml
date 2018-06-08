(* basis.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *
 * Defining the Diderot Basis environment.
 *)

structure Basis : sig

    val env : ParseTree.version -> GlobalEnv.t

  (* operations that are allowed in constant expressions *)
    val allowedInConstExp : AST.var -> bool

  (* spatial queries *)
    val isSpatialQueryOp : AST.var -> bool

  (* reduction operators *)
    val isReductionOp : AST.var -> bool

  (* global sets of strands *)
    val isStrandSet : AST.var -> bool

  (* border-control operations *)
    val isBorderCtl : AST.var -> bool

  (* is a function a number-of-strands query function? *)
    val isStrandsQueryFn : AST.var -> bool

  (* get access to lists of basis functions that we can use to dump the basis
   * environment.  Variables are grouped in lists of overloaded definitions
   * (singleton lists for non-overloaded variables).
   *)
    val getBasis : ParseTree.version -> {
            ops : AST.var list list,    (* operators *)
(* QUESTION: what about assignment operators? *)
            fns : AST.var list list,    (* functions (includes reductions) *)
            kerns : Kernel.t list       (* kernels *)
          }

  end = struct

    structure N = BasisNames
    structure BV = BasisVars
    structure ATbl = AtomTable
    structure GEnv = GlobalEnv

  (* non-overloaded operators *)
    val basisOps = [
            BV.op_mod,
            BV.op_D,
            BV.op_Dotimes,
            BV.op_Ddot,
            BV.op_not
          ]

  (* non-overloaded functions *)
    val basisFunctions = [
            BV.image_border,
            BV.fn_inside,
            BV.fn_length,
            BV.image_mirror,
            BV.fn_numActive,
            BV.fn_numStable,
            BV.fn_numStrands,
            BV.fn_size,
            BV.image_wrap
          ]

  (* non-overloaded reductions *)
    val basisReductions = [
            BV.red_all,
            BV.red_exists,
            BV.red_mean
(* FIXME: variance not yet supported
            BV.red_variance,
*)
          ]

  (* Math functions that have not yet been lifted to work on fields *)
    val basisMathFns = [
            BV.fn_atan2_rr,
            BV.fn_ceil_r,
            BV.fn_erf_r,
            BV.fn_erfc_r,
            BV.fn_floor_r,
            BV.fn_fmod_rr,
            BV.fn_log_r,
            BV.fn_log10_r,
            BV.fn_log2_r,
            BV.fn_pow_rr,
            BV.fn_round_r,
            BV.fn_trunc_r
          ]

  (* kernels *)
    val basisKernels = [
            Kernel.bspln3,
            Kernel.bspln5,
            Kernel.c4hexic,
            Kernel.ctmr,
            Kernel.tent,
            Kernel.c1tent,      (* for backwards compatibility to vis12 *)
            Kernel.c2tent,      (* for backwards compatibility to vis12 *)
            Kernel.c2ctmr       (* for backwards compatibility to vis12 *)
          ]

    val overloadOps = [
            (N.op_at, [BV.at_Td, BV.at_dT, BV.at_dd]),
            (N.op_lte, [BV.lte_ii, BV.lte_rr]),
            (N.op_equ, [BV.equ_bb, BV.equ_ii, BV.equ_ss, BV.equ_rr]),
            (N.op_neq, [BV.neq_bb, BV.neq_ii, BV.neq_ss, BV.neq_rr]),
            (N.op_gte, [BV.gte_ii, BV.gte_rr]),
            (N.op_gt, [BV.gt_ii, BV.gt_rr]),
            (N.op_add, [BV.add_ii, BV.add_tt, BV.add_ff, BV.add_ft, BV.add_tf]),
            (N.op_sub, [BV.sub_ii, BV.sub_tt, BV.sub_ff, BV.sub_ft, BV.sub_tf]),
            (N.op_mul, [
                BV.mul_ii, BV.mul_rr, BV.mul_rt, BV.mul_tr, BV.mul_rf, BV.mul_fr,
                BV.mul_ss, BV.mul_sf, BV.mul_fs, BV.mul_st, BV.mul_ts
              ]),
            (N.op_div, [BV.div_ii, BV.div_rr, BV.div_tr, BV.div_tr, BV.div_fr, BV.div_ss, BV.div_fs, BV.div_ts]),
            (N.op_pow, [BV.pow_ri, BV.pow_rr, BV.pow_si]),
            (N.op_curl, [BV.curl2D, BV.curl3D]),
            (N.op_convolve, [BV.convolve_vk, BV.convolve_kv]),
            (N.op_lt, [BV.lt_ii, BV.lt_rr]),
            (N.op_neg, [BV.neg_i, BV.neg_t, BV.neg_f]),
            (N.op_cross, [
                BV.op_cross2_tt, BV.op_cross3_tt, BV.op_cross2_ff, BV.op_cross3_ff,
                BV.op_cross2_tf, BV.op_cross3_tf, BV.op_cross2_ft, BV.op_cross3_ft
              ]),
            (N.op_norm, [BV.op_norm_i, BV.op_norm_t, BV.op_norm_f])
          ]

    val overloadFunctions = [
          (N.fn_acos, [BV.fn_acos_r, BV.fn_acos_s]),
          (N.fn_asin, [BV.fn_asin_r, BV.fn_asin_s]),
          (N.fn_atan, [BV.fn_atan_r, BV.fn_atan_s]),
          (N.fn_clamp, [BV.clamp_rrt, BV.clamp_ttt, BV.image_clamp]),
          (N.fn_cos, [BV.fn_cos_r, BV.fn_cos_s]),
          (N.fn_det, [BV.fn_det2_t, BV.fn_det3_t, BV.fn_det2_f, BV.fn_det3_f]),
          (N.fn_evals, [BV.evals2x2, BV.evals3x3]),
          (N.fn_evecs, [BV.evecs2x2, BV.evecs3x3]),
          (N.fn_exp, [BV.fn_exp_r, BV.fn_exp_s]),
          (N.fn_inv, [BV.fn_inv1_t, BV.fn_inv2_t, BV.fn_inv3_t, BV.fn_inv1_f, BV.fn_inv2_f, BV.fn_inv3_f]),
          (N.fn_lerp, [BV.lerp5, BV.lerp3]),
          (N.fn_clerp, [BV.clerp5, BV.clerp3]),
          (N.fn_max, [BV.fn_max_i, BV.fn_max_r, BV.red_max_i, BV.red_max_r]),
          (N.fn_min, [BV.fn_min_i, BV.fn_min_r, BV.red_min_i, BV.red_min_r]),
          (N.fn_normalize, [BV.fn_normalize_t, BV.fn_normalize_f]),
          (N.fn_modulate, [BV.fn_modulate_tt, BV.fn_modulate_ff, BV.fn_modulate_tf, BV.fn_modulate_ft]),
          (N.fn_product, [BV.red_product_i, BV.red_product_r]),
          (N.fn_sin, [BV.fn_sin_r, BV.fn_sin_s]),
          (N.fn_sphere, [BV.fn_sphere_im, BV.fn_sphere1_r, BV.fn_sphere2_t, BV.fn_sphere3_t]),
          (N.fn_sqrt, [BV.fn_sqrt_r, BV.fn_sqrt_s]),
          (N.fn_sum, [BV.red_sum_i, BV.red_sum_r]),
          (N.fn_tan, [BV.fn_tan_r, BV.fn_tan_s]),
          (N.fn_trace, [BV.fn_trace_t, BV.fn_trace_f]),
          (N.fn_transpose, [BV.fn_transpose_t, BV.fn_transpose_f]),
          (N.op_compose, [BV.comp])
          ]

  (* assignment operators are bound to the corresponding binary operator *)
    val overloadAssigOps = [
          (N.asgn_add, [BV.add_ii, BV.add_tt, BV.add_ff, BV.add_ft]),
          (N.asgn_sub, [BV.sub_ii, BV.sub_tt, BV.sub_ff, BV.sub_ft]),
          (N.asgn_mul, [BV.mul_ii, BV.mul_rr, BV.mul_tr, BV.mul_fr]),
          (N.asgn_div, [BV.div_ii, BV.div_rr, BV.div_tr, BV.div_tr]),
          (N.asgn_mod, [BV.op_mod])
        ]

  (* seed the basis environment *)
    fun env vers = let
          val gEnv = GEnv.new()
          fun insF x = GEnv.insertFunc(gEnv, Atom.atom(Var.nameOf x), GEnv.PrimFun[x])
          fun insK k = GEnv.insertKernel(gEnv, Atom.atom(Kernel.name k), k)
          fun insOvld (f, fns) = GEnv.insertFunc(gEnv, f, GEnv.PrimFun fns)
          in
            List.app insF basisOps;
            List.app insF basisFunctions;
            List.app insF basisReductions;
            List.app insF basisMathFns;
            List.app insK basisKernels;
            List.app insOvld overloadOps;
            List.app insOvld overloadFunctions;
            List.app insOvld overloadAssigOps;
            gEnv
          end

  (* operations that are allowed in constant expressions; we basically allow any operations
   * on integers, booleans, or tensors.  Operations on fields, images, sequences, or kernels
   * are not allowed.
   *)
   local
      val allowed = List.foldl Var.Set.add' Var.Set.empty [
              BV.op_mod,
              BV.op_cross2_tt, BV.op_cross3_tt,
              BV.op_outer_tt,
              BV.op_norm_i, BV.op_norm_t,
              BV.op_not,
              BV.fn_max_i, BV.fn_max_r,
              BV.fn_min_i, BV.fn_min_r,
              BV.fn_modulate_tt,
              BV.fn_normalize_t,
              BV.fn_trace_t,
              BV.fn_transpose_t,
              BV.lte_ii, BV.lte_rr,
              BV.equ_bb, BV.equ_ii, BV.equ_ss, BV.equ_rr,
              BV.neq_bb, BV.neq_ii, BV.neq_ss, BV.neq_rr,
              BV.gte_ii, BV.gte_rr,
              BV.lt_ii, BV.lt_rr,
              BV.gt_ii, BV.gt_rr,
              BV.add_ii, BV.add_tt,
              BV.sub_ii, BV.sub_tt,
              BV.mul_ii, BV.mul_rr, BV.mul_rt, BV.mul_tr,
              BV.div_ii, BV.div_rr, BV.div_tr, BV.div_tr,
              BV.pow_ri, BV.pow_rr,
              BV.neg_i, BV.neg_t,
              BV.clamp_rrt, BV.clamp_ttt,
              BV.lerp5, BV.lerp3,
              BV.fn_acos_r,
              BV.fn_asin_r,
              BV.fn_atan_r,
              BV.fn_atan2_rr,
              BV.fn_ceil_r,
              BV.fn_cos_r,
              BV.fn_erf_r,
              BV.fn_erfc_r,
              BV.fn_exp_r,
              BV.fn_floor_r,
              BV.fn_fmod_rr,
              BV.fn_log_r,
              BV.fn_log10_r,
              BV.fn_log2_r,
              BV.fn_round_r,
              BV.fn_sin_r,
              BV.fn_sqrt_r,
              BV.fn_tan_r,
              BV.fn_trunc_r
            ]
    in
    fun allowedInConstExp x = Var.Set.member (allowed, x)
    end (* local *)

  (* spatial queries *)
    local
      val qOps = List.foldl Var.Set.add' Var.Set.empty [
              BV.fn_sphere_im,
              BV.fn_sphere1_r,
              BV.fn_sphere2_t,
              BV.fn_sphere3_t
            ]
    in
    fun isSpatialQueryOp x = Var.Set.member (qOps, x)
    end

  (* the reduction operators *)
    local
      val redOps = List.foldl Var.Set.add' Var.Set.empty [
              BV.red_all,
              BV.red_exists,
              BV.red_max_i,
              BV.red_max_r,
              BV.red_mean,
              BV.red_min_i,
              BV.red_min_r,
              BV.red_product_i,
              BV.red_product_r,
              BV.red_sum_i,
              BV.red_sum_r
(* FIXME: variance not supported yet
              BV.red_variance
*)
            ]
    in
    fun isReductionOp x = Var.Set.member (redOps, x)
    end (* local *)

  (* the sets of strands are only allowed in global initialization and update blocks *)
    local
      val strandSets = Var.Set.fromList [BV.set_active, BV.set_all, BV.set_stable]
    in
    fun isStrandSet x = Var.Set.member (strandSets, x)
    end (* end local *)

  (* border-control operations *)
    local
      val borderCtl = Var.Set.fromList [
              BV.image_border, BV.image_clamp, BV.image_mirror, BV.image_wrap
            ]
    in
    fun isBorderCtl x = Var.Set.member (borderCtl, x)
    end

  (* is a function a number-of-strands query function? *)
    local
      val strandsQueryFns = Var.Set.fromList [
              BV.fn_numActive, BV.fn_numStable, BV.fn_numStrands
            ]
    in
    fun isStrandsQueryFn x = Var.Set.member (strandsQueryFns, x)
    end (* local *)

    fun getBasis vers = let
          val vl2vll = List.foldl (fn (x : AST.var, xss) => [x] :: xss)
          in {
            ops = vl2vll (List.map #2 overloadOps) basisOps,
            kerns = basisKernels,
            fns = vl2vll
                  (vl2vll
                    (vl2vll (List.map #2 overloadFunctions) basisMathFns)
                    basisReductions)
                  basisFunctions
          } end

  end
