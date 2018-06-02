(* util.sml
 *
 * Utility code for Simplification.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Util : sig

  (* the standard reductions extended with pseudo reductions *)
    datatype reduction = MEAN | VARIANCE | RED of Reductions.t

  (* identify a basis variable that specifies a reduction *)
    val identifyReduction : Var.t -> reduction

  (* return information about how to compute a reduction operator *)
    val reductionInfo : Reductions.t -> {
            rator : Var.t,                      (* primitive operator *)
            init : Literal.t,                   (* identity element to use for initialization *)
            mvs : SimpleTypes.meta_arg list     (* meta-variable arguments for primitive application *)
          }

  (* convert a block, which is the map part of a map-reduce, into a function by closing over
   * its free variables.  The simple variable argument is the strand object, which should always
   * be the first parameter (even if it is not referenced in the block).
   *)
    val makeFunction : string * Simple.var * Simple.block * SimpleTypes.ty
          -> Simple.func_def * Simple.var list

  (* return true if an AST constant expression is "small" *)
    val isSmallExp : AST.expr -> bool

  end = struct

    structure S = Simple
    structure BV = BasisVars
    structure L = Literal
    structure R = RealLit
    structure VMap = SimpleVar.Map

    datatype reduction = MEAN | VARIANCE | RED of Reductions.t

    fun identifyReduction rator =
          if Var.same(BV.red_all, rator)
            then RED(Reductions.ALL)
          else if Var.same(BV.red_exists, rator)
            then RED(Reductions.EXISTS)
          else if Var.same(BV.red_max_i, rator)
            then RED(Reductions.IMAX)
          else if Var.same(BV.red_max_r, rator)
            then RED(Reductions.RMAX)
          else if Var.same(BV.red_mean, rator)
            then MEAN
          else if Var.same(BV.red_min_i, rator)
            then RED(Reductions.IMIN)
          else if Var.same(BV.red_min_r, rator)
            then RED(Reductions.RMIN)
          else if Var.same(BV.red_product_i, rator)
            then RED(Reductions.IPRODUCT)
          else if Var.same(BV.red_product_r, rator)
            then RED(Reductions.RPRODUCT)
          else if Var.same(BV.red_sum_i, rator)
            then RED(Reductions.ISUM)
          else if Var.same(BV.red_sum_r, rator)
            then RED(Reductions.RSUM)
(* FIXME: variance not supported yet
          else if Var.same(BV.red_variance, rator)
            then VARIANCE
*)
            else raise Fail(Var.uniqueNameOf rator ^ " is not a reduction operator")

    val mvsReal = [SimpleTypes.SHAPE[]]

    fun reductionInfo redOp = let
          val id = Reductions.identity redOp
          in
            case redOp
             of Reductions.ALL => {rator = BV.op_and, init = id, mvs = []}
              | Reductions.EXISTS => {rator = BV.op_or, init = id, mvs = []}
              | Reductions.IMAX => {rator = BV.fn_max_i, init = id, mvs = []}
              | Reductions.RMAX => {rator = BV.fn_max_r, init = id, mvs = []}
              | Reductions.IMIN => {rator = BV.fn_min_i, init = id, mvs = []}
              | Reductions.RMIN => {rator = BV.fn_min_r, init = id, mvs = []}
              | Reductions.IPRODUCT => {rator = BV.mul_ii, init = id, mvs = []}
              | Reductions.RPRODUCT => {rator = BV.mul_rr, init = id, mvs = mvsReal}
              | Reductions.ISUM => {rator = BV.add_ii, init = id, mvs = []}
              | Reductions.RSUM => {rator = BV.add_tt, init = id, mvs = mvsReal}
            (* end case *)
          end

    local
      val n = ref 0
      fun mkFuncId (name, ty) = let val id = !n
            in
              n := id + 1;
              SimpleVar.new(name ^ Int.toString id, SimpleVar.FunVar, ty)
            end
    in
    fun makeFunction (name, strand, blk, resTy) = let
          val freeVars = ref []
          fun cvtVar (env, x) = (case VMap.find(env, x)
                 of SOME x' => (env, x')
                  | NONE => let
                      val x' = SimpleVar.copy(x, SimpleVar.FunParam)
                      in
                        freeVars := (x, x') :: !freeVars;
                        (VMap.insert(env, x, x'), x')
                      end
                (* end case *))
          fun cvtVars (env, xs) = let
                fun cvt (x, (env, xs')) = let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, x'::xs')
                      end
                in
                  List.foldr cvt (env, []) xs
                end
          fun newVar (env, x) = let
                val x' = SimpleVar.copy(x, SimpleVar.LocalVar)
                in
                  (VMap.insert(env, x, x'), x')
                end
          fun cvtBlock (env, S.Block{props, code}) = let
                fun cvtStms (env, [], stms') = (env, S.Block{props = props, code = List.rev stms'})
                  | cvtStms (env, stm::stms, stms') = let
                      val (env, stm') = cvtStm (env, stm)
                      in
                        cvtStms (env, stms, stm'::stms')
                      end
                in
                  cvtStms (env, code, [])
                end
          and cvtStm (env, stm) = (case stm
                 of S.S_Var(x, NONE) => let
                      val (env, x') = newVar (env, x)
                      in
                        (env, S.S_Var(x', NONE))
                      end
                  | S.S_Var(x, SOME e) => let
                      val (env, e') = cvtExp (env, e)
                      val (env, x') = newVar (env, x)
                      in
                        (env, S.S_Var(x', SOME e'))
                      end
                  | S.S_Assign(x, e) => let
                      val (env, e') = cvtExp (env, e)
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.S_Assign(x', e'))
                      end
                  | S.S_IfThenElse(x, b1, b2) => let
                      val (env, x') = cvtVar (env, x)
                      val (env, b1') = cvtBlock (env, b1)
                      val (env, b2') = cvtBlock (env, b2)
                      in
                        (env, S.S_IfThenElse(x', b1', b2'))
                      end
                  | S.S_Foreach(x, xs, b) => let
                      val (env, x') = cvtVar (env, x)
                      val (env, xs') = cvtVar (env, xs)
                      val (env, b') = cvtBlock (env, b)
                      in
                        (env, S.S_Foreach(x', xs', b'))
                      end
                  | S.S_New(name, args) => let
                      val (env, args') = cvtVars (env, args)
                      in
                        (env, S.S_New(name, args'))
                      end
                  | S.S_KillAll => (env, stm)
                  | S.S_StabilizeAll => (env, stm)
                  | S.S_Continue => (env, stm)
                  | S.S_Die => (env, stm)
                  | S.S_Stabilize => (env, stm)
                  | S.S_Return x => let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.S_Return x')
                      end
                  | S.S_Print xs => let
                      val (env, xs') = cvtVars (env, xs)
                      in
                        (env, S.S_Print xs')
                      end
                  | S.S_MapReduce _ => raise Fail "unexpected nested MapReduce"
                (* end case *))
          and cvtExp (env, exp) = (case exp
                 of S.E_Var x => let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.E_Var x')
                       end
                  | S.E_Lit _ => (env, exp)
                  | S.E_Kernel _ => (env, exp)
                  | S.E_Select(x, fld) => let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.E_Select(x', fld))
                       end
                  | S.E_Apply(f, args) => let
                      val (env, args') = cvtVars (env, args)
                      in
                        (env, S.E_Apply(f, args'))
                      end
                  | S.E_Prim(f, mvs, args, ty) => let
                      val (env, args') = cvtVars (env, args)
                      in
                        (env, S.E_Prim(f, mvs, args', ty))
                      end
                  | S.E_Tensor(args, ty) => let
                      val (env, args') = cvtVars (env, args)
                      in
                        (env, S.E_Tensor(args', ty))
                      end
                  | S.E_Seq(args, ty) => let
                      val (env, args') = cvtVars (env, args)
                      in
                        (env, S.E_Seq(args', ty))
                      end
                  | S.E_Tuple xs => let
                      val (env, xs') = cvtVars (env, xs)
                      in
                        (env, S.E_Tuple xs')
                      end
                  | S.E_Project(x, i) => let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.E_Project(x', i))
                      end
                  | S.E_Slice(x, indices, ty) => let
                      fun cvt (NONE, (env, idxs)) = (env, NONE::idxs)
                        | cvt (SOME x, (env, idxs)) = let
                            val (env, x') = cvtVar (env, x)
                            in
                              (env, SOME x' :: idxs)
                            end
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.E_Slice(x', indices, ty))
                      end
                  | S.E_Coerce{srcTy, dstTy, x} => let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.E_Coerce{srcTy=srcTy, dstTy=dstTy, x=x'})
                       end
                  | S.E_BorderCtl(BorderCtl.Default x, y) => let
                      val (env, x') = cvtVar (env, x)
                      val (env, y') = cvtVar (env, y)
                      in
                        (env, S.E_BorderCtl(BorderCtl.Default x', y'))
                      end
                  | S.E_BorderCtl(ctl, x) => let
                      val (env, x') = cvtVar (env, x)
                      in
                        (env, S.E_BorderCtl(ctl, x'))
                      end
                  | S.E_LoadSeq _ => (env, exp)
                  | S.E_LoadImage _ => (env, exp)
                  | S.E_InsideImage(pos, img, s) => let
                      val (env, pos') = cvtVar (env, pos)
                      val (env, img') = cvtVar (env, img)
                      in
                        (env, S.E_InsideImage(pos', img', s))
                      end
                  | S.E_FieldFn _ => (env, exp)
                (* end case *))
        (* the initial environment always includes the strand variable *)
          val (env, _) = cvtVar (VMap.empty, strand)
          val (env, blk) = cvtBlock (env, blk)
          val (args, params) = ListPair.unzip (List.rev (! freeVars))
          val f = SimpleFunc.new (name, resTy, List.map SimpleVar.typeOf params)
          in
            (S.Func{f=f, params=params, body=blk}, args)
          end
    end (* local *)

    fun isSmallExp (AST.E_Lit _) = true
      | isSmallExp (AST.E_Tensor(exps, _)) = (List.length exps <= 4)
      | isSmallExp _ = false

  end
