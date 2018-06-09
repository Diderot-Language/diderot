(* mk-operators.sml
 *
 * Functions to create the various Ein operators.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure MkOperators : sig

    type dim = int
    type shape = dim list
    type ids = Ein.index_id list

    val addRR : Ein.ein
    val addTT : shape -> Ein.ein
    val addTF : dim * shape -> Ein.ein
    val addFF : dim * shape -> Ein.ein

    val subRR : Ein.ein
    val subTT : shape -> Ein.ein
    val subTF : dim * shape -> Ein.ein
    val subFT : dim * shape -> Ein.ein
    val subFF : dim * shape -> Ein.ein

    val mulRT : shape -> Ein.ein
    val mulRR : Ein.ein
    val mulRF : dim * shape -> Ein.ein
    val mulST : dim * shape -> Ein.ein
    val mulSS : dim -> Ein.ein
    val mulSF : dim * shape -> Ein.ein

    val divTR : shape -> Ein.ein
    val divRR : Ein.ein
    val divFR : dim * shape -> Ein.ein
    val divSS : dim -> Ein.ein
    val divFS : dim * shape -> Ein.ein
    val divTS : dim * shape -> Ein.ein

    val halfT : dim -> Ein.ein
    val halfF : dim * dim -> Ein.ein
    val scaleIdT : dim -> Ein.ein
    val scaleIdF : dim * dim -> Ein.ein


    val negTT : shape -> Ein.ein
    val negFF : dim * shape -> Ein.ein

    val cross2TT : Ein.ein
    val cross3TT : Ein.ein
    val cross2FF : Ein.ein
    val cross3FF : Ein.ein
    val cross2TF : Ein.ein
    val cross3TF : Ein.ein
    val cross2FT : Ein.ein
    val cross3FT : Ein.ein

    val outerTT : shape * shape -> Ein.ein
    val outerFF : dim * shape * shape -> Ein.ein
    val outerTF : dim * shape * shape -> Ein.ein
    val outerFT : dim * shape * shape -> Ein.ein

    val innerTT : shape * ids -> Ein.ein
    val innerFF : shape * dim * ids -> Ein.ein
    val innerFT : shape * dim * ids -> Ein.ein
    val innerTF : shape * dim * ids -> Ein.ein

    val colonTT : shape * ids -> Ein.ein
    val colonFF : dim * shape * ids -> Ein.ein
    val colonFT : dim * shape * ids -> Ein.ein
    val colonTF : dim * shape * ids -> Ein.ein

    val normT : shape -> Ein.ein
    val normF : dim * shape -> Ein.ein

    val normalizeTT : shape -> Ein.ein
    val normalizeFF : dim * shape -> Ein.ein

    val traceT : dim -> Ein.ein
    val traceF : dim * dim * shape -> Ein.ein

    val transposeT : shape -> Ein.ein
(* QUESTION: should these be index_kind? *)
    val transposeF : dim * Ein.index_id * Ein.index_id -> Ein.ein

    val det2T : Ein.ein
    val det3T : Ein.ein
    val det2F : dim -> Ein.ein
    val det3F : dim -> Ein.ein

    val invR : Ein.ein
    val invS : dim -> Ein.ein
    val inv2T : Ein.ein
    val inv2F : dim -> Ein.ein

    val expF : dim -> Ein.ein
    val expT : Ein.ein

    val powFI : dim * int -> Ein.ein
    val powTI : int -> Ein.ein
    val sqrtR : Ein.ein
    val sqrtF : dim -> Ein.ein
    val cosR  : Ein.ein
    val cosF  : dim -> Ein.ein
    val acosR : Ein.ein
    val acosF : dim -> Ein.ein
    val sinR  : Ein.ein
    val sinF  : dim -> Ein.ein
    val asinR : Ein.ein
    val asinF : dim -> Ein.ein
    val tanR  : Ein.ein
    val tanF  : dim -> Ein.ein
    val atanR : Ein.ein
    val atanF : dim -> Ein.ein

    val modulateTT : shape -> Ein.ein
    val modulateTF : shape * dim -> Ein.ein
    val modulateFT : shape * dim -> Ein.ein
    val modulateFF : shape * dim -> Ein.ein

    val identity : dim -> Ein.ein
    val zeros : shape -> Ein.ein
    val sliceT : bool list * int list * Ein.index_bind list * int list -> Ein.ein
    val sliceF : bool list * int list * Ein.index_bind list * int -> Ein.ein
    val concatTensor : shape * int -> Ein.ein
    val concatField : dim * shape * int -> Ein.ein
    val composition: int * shape * int * shape -> Ein.ein
    
    val lerp3 : shape -> Ein.ein
    val lerp5 : shape -> Ein.ein
    val clampRRT : shape -> Ein.ein
    val clampTTT : shape -> Ein.ein
    val clerp3 : shape -> Ein.ein
    val clerp5 : shape -> Ein.ein

    val conv : dim * shape -> Ein.ein
    val probe : shape * dim -> Ein.ein

    val curl2d : Ein.ein
    val curl3d : Ein.ein
    val grad : shape -> Ein.ein
    val gradConstant : shape -> Ein.ein
    val dotimes : dim * shape -> Ein.ein (* ?? *)
    val divergence : dim * shape -> Ein.ein

    val cfexpMix :  shape * shape list * shape list -> Ein.ein

  end = struct

    structure E = Ein

    type dim = int
    type shape = dim list
    type ids = int list

  (* controls whether tensor operations should be *)
    val canSubst = true

  (* A constructor function for tensor variables that (by default) can be substituted for;
   * this behavior is controlled by the canSubst flag.
   *)
    fun mkTEN alpha = E.TEN(canSubst, alpha)

  (* a constructor function for tensor parameters that should never be substituted for.  *)
    fun mkNoSubstTEN alpha = E.TEN(false, alpha)

    fun specialize (alpha, inc) = List.mapi (fn (i, _) => E.V(i + inc)) alpha

    fun sumIds (n, inc, alpha) = let
          val vs = List.tabulate(n, fn v => (v+inc))
          in
            ListPair.map (fn(v, i) => (v, 0, i-1)) (vs, alpha)
          end

    fun sumIds2 (n, i) = List.tabulate(n, fn v => (v, 0, i))

  (******************************* Addition *****************************************)

  (* Adding tensors : < X{\alpha} + Y_{\alpha}>_{\alpha} *)
    fun addTT alpha = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha],
                index = alpha,
                body = E.Opn(E.Add, [E.Tensor(0, expindex), E.Tensor(1, expindex)])
              }
          end

    val addRR = addTT []

  (* Tensor and Fields *)
    fun addTF (dim, shape) =let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [mkTEN shape, E.FLD dim],
                index = shape,
                body = E.Opn(E.Add, [E.Lift(E.Tensor(0, expindex)), E.Field(1, expindex)])
              }
          end

  (* Adding Fields : < F{\alpha} + G_{\alpha}>_{\alpha} *)
    fun addFF (dim, shape) =let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = shape,
                body = E.Opn(E.Add, [E.Field(0, expindex), E.Field(1, expindex)])
              }
          end

  (********************************* Subtraction **************************************)

    fun subTT alpha = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha],
                index = alpha,
                body = E.Op2(E.Sub, E.Tensor(0, expindex), E.Tensor(1, expindex))
              }
          end

    val subRR = subTT []

    fun subTF (dim, shape) = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [mkTEN shape, E.FLD dim],
                index = shape,
                body = E.Opn(E.Add,
                  [E.Lift(E.Tensor(0, expindex)), E.Op1(E.Neg, E.Field(1, expindex))])
              }
          end

    fun subFT (dim, shape) = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [mkTEN shape, E.FLD dim],
                index = shape,
                body = E.Op2(E.Sub, E.Field(1, expindex), E.Lift(E.Tensor(0, expindex)))
              }
          end

    fun subFF (dim, shape) = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = shape,
                body = E.Op2(E.Sub, E.Field(0, expindex), E.Field(1, expindex))
              }
          end

  (********************************** Multiplication *************************************)

  (* scalar times tensor product: <s * T_{\alpha}>_{\alpha} *)
    fun mulRT alpha = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN{
                params = [mkTEN [], mkTEN alpha],
                index = alpha,
                body = E.Opn(E.Prod, [E.Tensor(0, []),  E.Tensor(1, expindex)])
              }
          end

    val mulRR = mulRT []

    fun mulRF (dim, shape) =let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                 params = [mkTEN [], E.FLD dim],
                 index = shape,
                 body = E.Opn(E.Prod, [E.Lift(E.Tensor(0, [])), E.Field(1, expindex)])
               }
          end

    fun mulST (dim, shape) =let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [mkTEN shape, E.FLD dim],
                index = shape,
                body = E.Opn(E.Prod, [E.Lift(E.Tensor(0, expindex)), E.Field(1, [])])
              }
          end

    fun mulSS dim = E.EIN{
             params = [E.FLD dim, E.FLD dim],
             index = [],
             body = E.Opn(E.Prod, [E.Field(0, []), E.Field(1, [])])
          }

    fun mulSF(dim, shape) =let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = shape,
                body = E.Opn(E.Prod, [E.Field(0, []), E.Field(1, expindex)])
              }
          end

  (************************************ Division ************************************)

    fun divTR alpha = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN []],
                index = alpha,
                body = E.Op2(E.Div, E.Tensor(0, expindex), E.Tensor(1, []))
              }
          end

    val divRR = divTR []

    fun divFR (dim, shape) = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, mkTEN []],
                index = shape,
                body = E.Op2(E.Div, E.Field(0, expindex), E.Lift(E.Tensor(1, [])))
              }
          end

    fun divSS dim = E.EIN{
            params = [E.FLD dim, E.FLD dim],
            index = [],
            body = E.Op2(E.Div, E.Field(0, []), E.Field(1, []))
          }

    fun divFS(dim, shape) = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = shape,
                body = E.Opn(E.Prod, [E.Field(0, expindex), E.Op2(E.Div, E.Const 1, E.Field(1, []))])
              }
          end

    fun divTS(dim, shape) = let
          val expindex = specialize(shape,0)
          in
            E.EIN{
                params = [mkTEN  shape, E.FLD dim],
                index = shape,
                body = E.Op2(E.Div, E.Lift(E.Tensor(0, expindex)), E.Field(1, []))
              }
          end

    (* Divide Scalars*)
    fun halfT(n) = E.EIN{
            params = [mkTEN [n, n]],
            index = [n,n],
            body = E.Op2(E.Div, E.Tensor(0, [E.V 0 , E.V 1]), E.Const 2)
        }

    fun halfF(dim,n) = E.EIN{
            params = [E.FLD(dim)],
            index = [n,n],
            body = E.Op2(E.Div, E.Field(0, [E.V 0 , E.V 1]), E.Const 2)
        }

    (*scale by delta*)
    fun scaleIdT(n) = E.EIN{
            params = [mkTEN []],
            index = [n,n],
            body = E.Opn(E.Prod, [E.Tensor(0, []), E.Delta(E.V 0, E.V 1)])
        }

    (*scale by delta*)
    fun scaleIdF(dim,n) = E.EIN{
            params = [E.FLD(dim)],
            index = [n,n],
            body = E.Opn(E.Prod, [E.Field(0, []), E.Delta(E.V 0, E.V 1)])
        }


  (************************************* Negation **********************************)

    fun negTT alpha = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN {
                params = [mkTEN alpha], index = alpha,
                body = E.Op1(E.Neg, E.Tensor(0, expindex))
              }
          end

    fun negFF (dim, shape) = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim], index = shape,
                body = E.Op1(E.Neg, E.Field(0, expindex))
              }
          end

  (****************************** cross product ***********************************)

  (* 2-d cross product Eps_{ij}U_i V_j *)
    val cross2TT = E.EIN{
             params = [mkTEN [2], mkTEN [2]],
             index = [],
             body = E.Sum([(0, 0, 1), (1, 0, 1)],
             E.Opn(E.Prod, [E.Eps2(E.V 0, E.V 1), E.Tensor(0, [E.V 0]), E.Tensor(1, [E.V 1])]))
          }

  (* crossProduct is on 3D vectors ..vec3 t8=t0 × t1; *)
    val cross3TT = E.EIN{
             params = [mkTEN [3], mkTEN [3]],
             index = [3],
             body = E.Sum([(1, 0, 2), (2, 0, 2)],
                E.Opn(E.Prod, [
                    E.Epsilon(E.V 0, E.V 1, E.V 2),
                    E.Tensor(0, [E.V 1]),
                    E.Tensor(1, [E.V 2])
                  ]))
          }

  (* Field Cross Product *)
    val cross2FF = E.EIN{
            params = [E.FLD(2), E.FLD(2)], index = [],
            body = E.Sum([(0, 0, 1), (1, 0, 1)],
                E.Opn(E.Prod, [E.Eps2(E.V 0, E.V 1), E.Field(0, [E.V 0]), E.Field(1, [E.V 1])]))
          }

  (* Field Cross Product *)
    val cross3FF = E.EIN{
            params = [E.FLD(3), E.FLD(3)], index= [3],
            body = E.Sum([(1, 0, 2), (2, 0, 2)],
                E.Opn(E.Prod, [
                    E.Epsilon(E.V 0, E.V 1, E.V 2),
                    E.Field(0, [E.V 1]),
                    E.Field(1, [E.V 2])
                  ]))
          }

    (*Field and Tensor Cross product *)
    val cross2FT = E.EIN{
            params = [E.FLD(2), mkTEN [2]], index= [],
            body = E.Sum([(0, 0, 1), (1, 0, 1)],
                E.Opn(E.Prod, [
                    E.Eps2(E.V 0, E.V 1),
                    E.Field(0, [E.V 0]),
                    E.Lift(E.Tensor(1, [E.V 1]))
                  ]))
          }

    val cross3FT = E.EIN{
            params = [E.FLD(3), mkTEN[3]], index= [3],
            body = E.Sum([(1, 0, 2), (2, 0, 2)],
                E.Opn(E.Prod, [
                    E.Epsilon(E.V 0, E.V 1, E.V 2),
                    E.Field(0, [E.V 1]),
                    E.Lift(E.Tensor(1, [E.V 2]))
                  ]))
          }

    val cross2TF = E.EIN{
            params = [mkTEN[2], E.FLD(2)], index = [],
            body = E.Sum([(0, 0, 1), (1, 0, 1)],
                E.Opn(E.Prod, [
                    E.Eps2(E.V 0, E.V 1),
                    E.Lift(E.Tensor(0, [E.V 0])),
                    E.Field(1, [E.V 1])
                  ]))
          }

    val cross3TF = E.EIN{
            params = [mkTEN[3], E.FLD(3)], index= [3],
            body = E.Sum([(1, 0, 2),(2,0,2)],
                E.Opn(E.Prod, [
                    E.Epsilon(E.V 0, E.V 1, E.V 2),
                    E.Lift(E.Tensor(0, [E.V 1])),
                    E.Field(1, [E.V 2])
                  ]))
          }

  (******************** outer product ********************************)

    fun outerTT (alpha, beta) = let
          val expIdxA = specialize (alpha, 0)
          val expIdxB = specialize (beta, length alpha)
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN beta],
                index = alpha@beta,
                body = E.Opn(E.Prod, [E.Tensor(0, expIdxA), E.Tensor(1, expIdxB)])
              }
          end

    (*Assumes same dimension vector field *)
    fun outerFF (dim, alpha, beta) =let
          val expIdxA = specialize (alpha, 0)
          val expIdxB = specialize (beta, length alpha)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = alpha@beta,
                body = E.Opn(E.Prod, [E.Field(0, expIdxA), E.Field(1, expIdxB)])
              }
          end

    fun outerTF (dim, alpha, beta) =let
          val expIdxA = specialize (alpha, 0)
          val expIdxB = specialize (beta, length alpha)
          in
            E.EIN{
                params = [mkTEN alpha, E.FLD dim],
                index = alpha@beta,
                body = E.Opn(E.Prod, [E.Lift(E.Tensor(0, expIdxA)), E.Field(1, expIdxB)])
              }
          end

    fun outerFT (dim, alpha, beta) =let
          val expIdxA = specialize(alpha, 0)
          val expIdxB = specialize(beta, length alpha)
          in
            E.EIN{
                params = [E.FLD dim, mkTEN alpha],
                index = alpha@beta,
                body = E.Opn(E.Prod, [E.Field(0, expIdxA), E.Lift(E.Tensor(1, expIdxB))])
              }
          end

  (*************************** inner product **********************************)
    (* generic inner product: <T_{\alpha i} * T_{i \beta}>_{\alpha \beta} *)
    fun innerTT (shape1, i::beta) = let
          val alpha = List.take(shape1, length shape1 - 1)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, length alpha)
          val sid = length alpha + length beta
          val sx = E.V sid
          val s'' = [(sid, 0, i-1)]
          in
            E.EIN{
                params = [mkTEN shape1, mkTEN(i :: beta)],
                index = alpha@beta,
                body = E.Sum(s'', E.Opn(E.Prod, [
                    E.Tensor(0, expindexA@[sx]),   (* T_{\alpha i} *)
                    E.Tensor(1, [sx]@expindexB )   (* T'_{i \beta} *)
                  ]))
              }
          end
      | innerTT _ = raise Fail "Wrong shape for inner product"

  (* generic inner product: <T_{\alpha i} * T_{i \beta}>_{\alpha \beta} *)
    fun innerFF (shape1, dim, i::beta) = let
          val alpha = List.take(shape1, length shape1 - 1)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, length alpha)
          val sid = length alpha + length beta
          val sx = E.V sid
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = alpha @ beta,
                body = E.Sum([(sid, 0, i-1)],
                  E.Opn(E.Prod, [
                      E.Field(0, expindexA @ [sx]),   (* F_{\alpha i} *)
                      E.Field(1, [sx] @ expindexB)    (* F'_{i \beta} *)
                    ]))
              }
          end
      | innerFF _ = raise Fail "Wrong shape for innerProductField"

    fun innerFT (shape1, dim, i::beta) = let
          val alpha = List.take(shape1, length shape1-1)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, length alpha)
          val sid = length alpha + length beta
          val sx = E.V sid
          in
            E.EIN{
                params = [E.FLD dim, mkTEN(i::beta)],
                index = alpha @ beta,
                body = E.Sum([(sid, 0, i-1)],
                  E.Opn(E.Prod, [
                      E.Field(0, expindexA @ [sx]),           (* F_{\alpha i} *)
                      E.Lift(E.Tensor(1, [sx] @ expindexB ))  (* F'_{i \beta} *)
                    ]))
              }
          end
      | innerFT _ = raise Fail "Wrong shape for innerProductFieldTensor"

    fun innerTF (shape1, dim, i::beta) = let
          val alpha = List.take(shape1, length shape1 - 1)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, length alpha)
          val sid = length alpha + length beta
          val sx = E.V sid
          in
            E.EIN{
                params = [mkTEN shape1, E.FLD dim],
                index = alpha @ beta,
                body = E.Sum([(sid, 0, i-1)],
                  E.Opn(E.Prod, [
                      E.Lift(E.Tensor(0, expindexA @ [sx])),   (* F_{\alpha i} *)
                      E.Field(1, [sx] @ expindexB)             (* F'_{i \beta} *)
                    ]))
              }
          end
      | innerTF _ = raise Fail "Wrong shape for innerProductTensorField"

  (*************************** colon product **********************************)

  (* <T_{\alpha i j} * B{i j \beta }>_\alpha \beta *)
    fun colonTT (shape1, i::j::beta) = let
          val lenAlpha = length shape1 - 2
          val alpha = List.take(shape1, lenAlpha)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, lenAlpha)
          val sumi = lenAlpha + length beta
          val s' = [E.V sumi, E.V(sumi+1)]
          val sx = [(sumi, 0, i-1), ((sumi+1), 0, j-1)]
          in
            E.EIN{
                params = [mkTEN shape1, mkTEN(i::j::beta)],
                index = alpha@beta,
                body = E.Sum(sx,
                  E.Opn(E.Prod, [E.Tensor(0, expindexA@s'), E.Tensor(1, s'@expindexB)]))
              }
          end

  (* <F_{\alpha i j} * G_{i j \beta }>_\alpha \beta *)
    fun colonFF (dim, shape1, i::j::beta) = let
          val lenAlpha = length shape1 - 2
          val alpha = List.take(shape1, lenAlpha)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, lenAlpha)
          val sumi = lenAlpha + length beta
          val s' = [E.V sumi, E.V(sumi+1)]
          val sx = [(sumi, 0, i-1), ((sumi+1), 0, j-1)]
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = alpha@beta,
                body = E.Sum(sx,
                  E.Opn(E.Prod, [E.Field(0, expindexA@s'), E.Field(1, s'@expindexB)]))
              }
          end


  (* <F_{\alpha i j} * T_{i j \beta }>_\alpha \beta *)
    fun colonFT (dim, shape1, i::j::beta) = let
          val lenAlpha = length shape1 - 2
          val alpha = List.take(shape1, lenAlpha)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, lenAlpha)
          val sid = lenAlpha + length beta
          val s' = [E.V sid, E.V(sid+1)]
          val sx = [(sid, 0, i-1), ((sid+1), 0, j-1)]
          in
            E.EIN{
                params = [E.FLD dim, mkTEN shape1],
                index = alpha@beta,
                body = E.Sum(sx,
                  E.Opn(E.Prod, [E.Field(0, expindexA@s'), E.Lift(E.Tensor(1, s'@expindexB))]))
              }
          end

  (* <T_{\alpha i j} * G{i j \beta }>_\alpha \beta *)
    fun colonTF (dim, shape1, i::j::beta) = let
          val lenAlpha = length shape1 - 2
          val alpha = List.take(shape1, lenAlpha)
          val expindexA = specialize(alpha, 0)
          val expindexB = specialize(beta, lenAlpha)
          val sid = lenAlpha + length beta
          val s' = [E.V sid, E.V(sid+1)]
          val sx = [(sid, 0, i-1), ((sid+1), 0, j-1)]
          in
            E.EIN{
                params = [mkTEN(i::j::beta), E.FLD dim],
                index = alpha@beta,
                body = E.Sum(sx,
                  E.Opn(E.Prod, [E.Lift(E.Tensor(0, expindexA@s')), E.Field(1, s'@expindexB)]))
              }
          end

  (******************** Norm  ********************************)

        fun normT [] = E.EIN{
                params = [mkTEN []], index = [], body = E.Op1(E.Abs, E.Tensor(0, []))
              }
          | normT alpha = let
              val expIdx = specialize(alpha, 0)
              val sx = sumIds(length alpha, 0, alpha)
              in
                E.EIN{
                    params = [mkTEN alpha],
                    index = [],
                    body = E.Op1(E.Sqrt,
                      E.Sum(sx, E.Opn(E.Prod, [E.Tensor(0, expIdx), E.Tensor(0, expIdx)])))
                  }
              end

    fun normF (dim, []) =
          E.EIN{params = [E.FLD dim],index = [], body = E.Op1(E.Abs, E.Field(0, []))}
      | normF (dim, alpha) = let
          val expIdx = specialize(alpha, 0)
          val sx = sumIds(length alpha, 0, alpha)
          in
            E.EIN{
                params = [E.FLD dim],
                index = [],
                body = E.Op1(E.Sqrt,
                  E.Sum(sx, E.Opn(E.Prod, [E.Field(0, expIdx), E.Field(0, expIdx)])))
              }
          end

    fun normalizeTT alpha = let
          val expindex = specialize(alpha, 0)
          val len = length alpha
          val expindexDot = specialize(alpha, len)
          val sx = sumIds(len, len, alpha)
          val f = E.Tensor(0, expindex)
          val g = E.Tensor(1, expindexDot)
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha],
                index = alpha,
                body = E.Opn(E.Prod, [
                    f, E.Op2(E.Div, E.Const 1, E.Op1(E.Sqrt, E.Sum(sx, E.Opn(E.Prod, [g, g]))))
                  ])
              }
          end

    fun normalizeFF (dim, alpha as i::_) = let
          val expindex = specialize(alpha, 0)
          val len = length alpha
          val expindexDot = specialize(alpha, len)
          val sx = sumIds(len, len, alpha)
          val f = E.Field(0, expindex)
          val g = E.Field(1, expindexDot)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = alpha,
                body = E.Opn(E.Prod, [
                    f, E.Op2(E.Div, E.Const 1, E.Op1(E.Sqrt, E.Sum(sx, E.Opn(E.Prod, [g, g]))))
                  ])
              }
          end

  (************************* trace *************************)

  (* Trace: <M_{i, i}>  This one Sx represents both i's*)
    fun traceT dim = E.EIN{
            params = [mkTEN [dim, dim]], index = [],
            body = E.Sum([(0, 0, dim-1)], E.Tensor(0, [E.V 0, E.V 0]))
          }

  (* Trace: <Sigma_i F_{\alpha i, i}>  This one Sx represents both i's *)
    fun traceF (dim, d2, alpha) = let
          val expindex = specialize(alpha, 0)
          val sid = length alpha
          val sx = E.V sid
          in
            E.EIN{
                params = [E.FLD dim],
                index = alpha,
                body = E.Sum([(sid, 0, d2-1)], E.Field(0, expindex@[sx, sx]))
              }
          end

  (************************* tranpose *************************)

    fun transposeT alpha = E.EIN{
            params = [mkTEN alpha],
            index = List.rev alpha,
            body = E.Tensor(0, [E.V 1, E.V 0])
          }

  (* Transpose Field F_{ji} *)
    fun transposeF (dim, i, j) = E.EIN{
            params = [E.FLD dim],
            index = [i, j],
            body = E.Field(0, [E.V 1, E.V 0])
          }

  (************************* determinant *************************)

    val det2T = E.EIN{
            params = [mkNoSubstTEN [2, 2]],
            index = [],
            body = E.Op2(E.Sub,
              E.Opn(E.Prod, [E.Tensor(0, [E.C 0, E.C 0]), E.Tensor(0, [E.C 1, E.C 1])]),
              E.Opn(E.Prod, [E.Tensor(0, [E.C 0, E.C 1]), E.Tensor(0, [E.C 1, E.C 0])]))
          }

    val det3T = let
          val a = E.Tensor(0, [E.C 0, E.C 0])
          val b = E.Tensor(0, [E.C 0, E.C 1])
          val c = E.Tensor(0, [E.C 0, E.C 2])
          val d = E.Tensor(0, [E.C 1, E.C 0])
          val e = E.Tensor(0, [E.C 1, E.C 1])
          val f = E.Tensor(0, [E.C 1, E.C 2])
          val g = E.Tensor(0, [E.C 2, E.C 0])
          val h = E.Tensor(0, [E.C 2, E.C 1])
          val i = E.Tensor(0, [E.C 2, E.C 2])
          in
            E.EIN{
                params = [mkNoSubstTEN [3, 3]],
                index = [],
                body = E.Op2(E.Sub,
                  E.Opn(E.Add, [
                      E.Opn(E.Prod, [a, e, i]), E.Opn(E.Prod, [b, f, g]), E.Opn(E.Prod, [c, d, h])
                    ]),
                  E.Opn(E.Add, [
                      E.Opn(E.Prod, [c, e, g]), E.Opn(E.Prod, [b, d, i]), E.Opn(E.Prod, [a, f, h])
                    ]))
              }
          end

    fun det2F dim = E.EIN{
            params = [E.FLD dim],
            index = [],
            body = E.Op2(E.Sub,
              E.Opn(E.Prod, [E.Field(0, [E.C 0, E.C 0]), E.Field(0, [E.C 1, E.C 1])]),
              E.Opn(E.Prod, [E.Field(0, [E.C 0, E.C 1]), E.Field(0, [E.C 1, E.C 0])]))
          }

    fun det3F dim = E.EIN{
            params = [E.FLD dim],
            index = [],
            body = E.Sum([(0, 0, 2)],
              E.Opn(E.Prod, [
                  E.Field(0, [E.C 0, E.V 0]),
                  E.Sum([(1, 0, 2)],
                    E.Opn(E.Prod, [
                        E.Field(0, [E.C 1, E.V 1]),
                        E.Sum([(2, 0, 2)],
                          E.Opn(E.Prod, [E.Epsilon(E.V 0, E.V 1, E.V 2), E.Field(0, [E.C 2, E.V 2])]))
                      ]))
                ]))
          }

  (************************* Inverse  *************************)

    fun mkInvS e = E.Op2(E.Div, E.Const 1, e(0, []))
    fun invS dim = E.EIN{params = [E.FLD dim], index= [], body =  mkInvS E.Field}
    val invR = E.EIN{params = [mkTEN([])], index= [], body =  mkInvS E.Tensor}

    fun mkInv2x2 f = let
          fun mkFCx (ix, jx) = f (0, [E.C ix, E.C jx])
          fun mkFVx (ix, jx) = f (0, [E.V ix, E.V jx])
          val f00 = mkFCx (0, 0)
          val f11 = mkFCx (1, 1)
          val f01 = mkFCx (0, 1)
          val f10 = mkFCx (1, 0)
          val i = 0 and j = 1 and k = 2
          val fij = mkFVx (i, j)
          val fkk = mkFVx (k, k)
        (* numerator*)
          val en = E.Op2(E.Sub,
                E.Opn(E.Prod, [E.Sum([(k, 0, 1)], fkk), E.Delta(E.V i, E.V j)]),
                fij)
        (* denominator *)
          val d1 = E.Opn(E.Prod, [f00, f11])
          val d2 = E.Opn(E.Prod, [f01, f10])
          val dn = E.Op2(E.Sub, d1, d2)
          in
            E.Op2(E.Div, en, dn)
          end

    fun inv2F dim = E.EIN{params = [E.FLD dim], index= [2, 2], body = mkInv2x2 E.Field}

    val inv2T = let
          val shape = [2, 2]
          in
            E.EIN{params = [mkTEN shape], index = shape, body = mkInv2x2 E.Tensor}
          end

  (************************* Exponential **************************)
    fun expF dim = E.EIN{params = [E.FLD dim], index = [], body = E.Op1(E.Exp, E.Field(0, []))}
    val expT = E.EIN{params = [mkNoSubstTEN []], index = [], body = E.Op1(E.Exp, E.Tensor(0, []))}

  (************************* Lifted single-argument math functions *************************)
    local
      fun tensorFn rator = E.EIN{
              params = [mkTEN []],
              index = [],
              body = E.Op1(rator, E.Tensor(0, []))
            }
      fun liftFn rator dim = E.EIN{
              params = [E.FLD dim],
              index = [],
              body = E.Op1(rator, E.Field(0, []))
            }
    in
    fun powFI (dim, n) = E.EIN{
            params = [E.FLD dim],
            index = [], body = E.Op1(E.PowInt n, E.Field(0, []))
          }
    fun powTI n = E.EIN{
            params = [mkTEN []],
            index = [], body = E.Op1(E.PowInt n, E.Tensor(0, []))
          }
    val sqrtR = tensorFn E.Sqrt
    val sqrtF = liftFn E.Sqrt
    val cosR  = tensorFn E.Cosine
    val cosF  = liftFn E.Cosine
    val acosR = tensorFn E.ArcCosine
    val acosF = liftFn E.ArcCosine
    val sinR  = tensorFn E.Sine
    val sinF  = liftFn E.Sine
    val asinR = tensorFn E.ArcSine
    val asinF = liftFn E.ArcSine
    val tanR  = tensorFn E.Tangent
    val tanF  = liftFn E.Tangent
    val atanR = tensorFn E.ArcTangent
    val atanF = liftFn E.ArcTangent
    end (* local *)

  (************************* other tensor ops *************************)

    fun modulateTT shape = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [mkTEN shape, mkTEN shape],
                index = shape,
                body = E.Opn(E.Prod, [E.Tensor(0, expindex), E.Tensor(1, expindex)])
              }
          end

    fun modulateFF(shape, dim)  = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, E.FLD dim],
                index = shape,
                body = E.Opn(E.Prod,[E.Field(0, expindex), E.Field(1, expindex)])
              }
          end

    fun modulateTF(shape, dim)  = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [mkTEN shape, E.FLD dim],
                index = shape,
                body = E.Opn(E.Prod,[E.Lift(E.Tensor(0, expindex)), E.Field(1, expindex)])
              }
          end

    fun modulateFT(shape, dim)  = let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.FLD dim, mkTEN shape],
                index = shape,
                body = E.Opn(E.Prod, [E.Field(0, expindex), E.Lift(E.Tensor(1, expindex))])
              }
          end

    fun identity dim = E.EIN{
            params = [], index = [dim, dim], body = E.Delta(E.V 0, E.V 1)
          }

    fun zeros shape = E.EIN{
            params = [], index = shape, body = E.Zero (specialize(shape, 0))
          }

(* QUESTION: why do we need the const list?  The indices are implicit in the position of
 * of the mask element!  Likewise, the result type can be determined from the argTy and
 * mask.
 *)
    fun sliceT (mask, const, rstTy, argTy) = let
          fun iter ([], _, cnt) = []
            | iter (true::es, c::cs, cnt) = (E.C c)::iter(es, cs, cnt)
            | iter (false::es, cs, cnt) = (E.V cnt)::iter(es, cs, cnt+1)
          val ix = iter(mask, const, 0)
          in
            E.EIN{params = [E.TEN(true, argTy)], index = rstTy, body = E.Tensor(0, ix)}
          end

    fun sliceF (mask, const, rstTy, dim) = let
          fun iter ([], _, cnt) = []
            | iter (true::es, c::cs, cnt) = (E.C c)::iter(es, cs, cnt)
            | iter (false::es, cs, cnt) = (E.V cnt)::iter(es, cs, cnt+1)
          val ix = iter(mask, const, 0)
          in
            E.EIN{params = [E.FLD dim], index = rstTy, body = E.Field(0, ix)}
          end

    fun concatBody (expression, shape, nflds, idshift) = let
          val expindex = specialize(shape, 1)
          val exps = List.tabulate (nflds, fn n =>  E.Opn(E.Prod, [expression(n+idshift, expindex), E.Delta(E.C n, E.V 0)]))
          in
            E.Opn(E.Add, exps)
          end

    fun concatTensor (shape, nflds) =
          E.EIN{
              params = List.tabulate (nflds, fn _=> mkTEN shape),
              index = nflds::shape,
              body = concatBody (E.Tensor, shape, nflds, 0)
            }

    fun concatField (dim, shape, nflds) =
          E.EIN{
              params = List.tabulate (nflds, fn _=> E.FLD dim),
              index = nflds::shape,
              body = concatBody (E.Field, shape, nflds, 0)
            }
            
    fun composition (dim0, shape0, dim1, shape1) = let
          fun err exp = raise Fail (concat["\n Composition incorrect \n\t Expected: Vector field of length ", Int.toString(dim0), "\n\t Observed: ", exp, "\n"])
          val _ = (case shape1
                of [] => if(dim0=1) then () else err("scalar field") 
                | [n] => if(dim0=n) then () else err("vector field of length"^Int.toString(n)) 
                | _ => err("second-order tensor field")
            (* end case *))
          val expindex0 = specialize(shape0, 0)
          val expindex1 = specialize(shape1, 0)
          in
            E.EIN{
                params = [E.FLD (dim0), E.FLD (dim1)], 
                index = shape0, 
                body = E.Comp (E.Field(0, expindex0), [(E.Field(1, expindex1), shape1)])
             }
          end
          
  (* Lerp<ty>(a, b, t) -- computes a + t*(b-a), where a and b have type ty
   * and t has type real
   *)
    fun lerp3 alpha = let
          val expindex = specialize(alpha, 0)
          val a = E.Tensor(0, expindex)
          val b = E.Tensor(1, expindex)
          val c = E.Tensor(2, [])
          val e3 = E.Op2(E.Sub, b, a)
          val e5 = E.Opn(E.Prod, [c, e3])
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha, mkTEN []],
                index = alpha,
                body = E.Opn(E.Add, [a, e5])
              }
          end

    fun lerp5 alpha = let
          val expindex = specialize(alpha, 0)
          val a = E.Tensor(0, expindex)
          val b = E.Tensor(1, expindex)
          val c = E.Tensor(2, [])
          val d = E.Tensor(3, [])
          val e = E.Tensor(4, [])
          val e1 = E.Op2(E.Sub, d, c)
          val e2 = E.Op2(E.Sub, e, c)
          val e3 = E.Op2(E.Sub, b, a)
          val e4 = E.Op2(E.Div, e1, e2)
          val e5 = E.Opn(E.Prod, [e4, e3])
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha, mkTEN [], mkTEN [], mkTEN []],
                index = alpha,
                body = E.Opn(E.Add, [a, e5])
              }
          end

  (* clamps x to the range lo..hi, where lo and hi are scalars and x *)
    fun clampRRT alpha = let
          val expindex = specialize(alpha, 0)
          val a = E.Tensor(0, [])
          val b = E.Tensor(1, [])
          val c = E.Tensor(2, expindex)
          in
            E.EIN{
                params = [mkTEN [], mkTEN [], mkTEN alpha],
                index = alpha,
                body = E.Op3(E.Clamp, a, b, c)
              }
          end

  (* clamps x[alpha] to the range lo[alpha]..hi[alpha] *)
    fun clampTTT alpha = let
          val expindex = specialize(alpha, 0)
          val a = E.Tensor(0, expindex)
          val b = E.Tensor(1, expindex)
          val c = E.Tensor(2, expindex)
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha, mkTEN alpha],
                index = alpha,
                body = E.Op3(E.Clamp, a, b, c)
             }
          end

    fun clerp3 alpha = let
          val expindex = specialize(alpha, 0)
          val a = E.Tensor(0, expindex)
          val b = E.Tensor(1, expindex)
          val c = E.Tensor(2, [])
          val e3 = E.Op2(E.Sub, b, a)
          val e5 = E.Opn(E.Prod, [c, e3])
          val elerp = E.Opn(E.Add, [a, e5])
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha, mkTEN []],
                index = alpha,
                body = E.Op3(E.Clamp, a, b, elerp)
              }
          end

    fun clerp5 alpha = let
          val expindex = specialize(alpha, 0)
          val a = E.Tensor(0, expindex)
          val b = E.Tensor(1, expindex)
          val c = E.Tensor(2, [])
          val d = E.Tensor(3, [])
          val e = E.Tensor(4, [])
          val e1 = E.Op2(E.Sub, d, c)
          val e2 = E.Op2(E.Sub, e, c)
          val e3 = E.Op2(E.Sub, b, a)
          val e4 = E.Op2(E.Div, e1, e2)
          val e5 = E.Opn(E.Prod, [e4, e3])
          val elerp = E.Opn(E.Add, [a, e5])
          in
            E.EIN{
                params = [mkTEN alpha, mkTEN alpha, mkTEN [], mkTEN [], mkTEN []],
                index = alpha,
                body =  E.Op3(E.Clamp, a, b, elerp)
              }
          end

    (******************** other field ops  ********************************)

  (* FLD here is bounded to image field, and dimension of h *)
    fun conv (dim, shape) =let
          val expindex = specialize(shape, 0)
          in
            E.EIN{
                params = [E.IMG(dim, shape), E.KRN],
                index = shape,
                body = E.Conv(0, expindex, 1, [])
              }
          end

  (* Probe: <F(x)>_{\alpha}   *)
    fun probe (alpha, dim) = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN{
                params = [E.FLD dim, mkNoSubstTEN [dim]], index = alpha,
                body = E.Probe(E.Field(0, expindex), E.Tensor(1, []))
              }
          end

  (***************************** derivative ****************************)

  (* \EinExp{\sum_{ij}\mathcal{E}_{ij} \frac{F_j}{\partial x_i} *)
    val curl2d = E.EIN{
            params = [E.FLD 2],
            index = [],
            body = E.Sum([(0, 0, 1), (1, 0, 1)],
              E.Opn(E.Prod, [
                  E.Eps2(E.V 0, E.V 1),
                  E.Apply(E.Partial[E.V 0], E.Field(0, [E.V 1]))
                ]))
          }

    val curl3d = E.EIN{
            params = [mkTEN [3]],
            index = [3],
            body = E.Sum([(1, 0, 2), (2, 0, 2)],
              E.Opn(E.Prod, [
                  E.Epsilon(E.V 0, E.V 1, E.V 2),
                  E.Apply(E.Partial[E.V 1], E.Field(0, [E.V 2]))
                ]))
          }

    fun gradConstant (alpha as a::_) = E.EIN{
            params = [E.FLD a],
            index = [],
            body = E.Apply(E.Partial [(E.C 0)], E.Field(0, []))
          }

  (*< d F /  d_i>_i  *)
      fun grad (alpha as a::_) = let
          val expindex = specialize(alpha, 0)
          in
            E.EIN{
                params = [E.FLD a],
                index = alpha,
                body = E.Apply(E.Partial expindex, E.Field(0, []))
              }
          end

  (*< Sigma d F_alpha /  d x_i>ALpha  i CHANGE HERE *)
    fun dotimes (dim, alpha) = let
          val n = length alpha
          val i' = List.tabulate (n, fn x => E.V x)
          in
            E.EIN{
                params = [E.FLD dim], index = alpha@[dim],
                body = E.Apply(E.Partial[E.V n], E.Field(0, i'))
              }
          end

  (*  <d F_i /d_i> *)
    fun divergence (dim, alpha) = let
          val expindex = specialize(alpha, 0)
          val sid = length alpha
          val sumIndex = E.V sid
          val sumIndexL = [sumIndex]
          val S = expindex@sumIndexL
          in
            E.EIN{
                params = [E.FLD dim],
                index = alpha,
                body = E.Sum([(sid, 0, dim-1)], E.Apply(E.Partial sumIndexL, E.Field(0, S)))
              }
          end

    fun cfexpMix (alpha_f, alphas_tf, alphas_tt) = let
          val n_tf = length(alphas_tf)
          val tterm_tf = List.tabulate(n_tf, fn id => (id+1, E.F))
          val n_tt = length(alphas_tt)
          val shift_tf = n_tf+1
          val tterm_tt = List.tabulate(n_tt, fn id => (id+shift_tf,E.T))
          val fldtem = E.Tensor(0, specialize(alpha_f, 0))
          val bodyterm  = E.OField(E.CFExp (tterm_tf@tterm_tt), fldtem ,  E.Partial [])
          val param_f = [mkTEN alpha_f]
          val param_tt = List.map (fn talpha => mkNoSubstTEN  talpha)  alphas_tt
          val param_tf = List.map (fn talpha => mkNoSubstTEN  talpha)  alphas_tf
          in
            E.EIN {
                params = param_f@param_tf@param_tt,
                index  = alpha_f,
                body   = bodyterm
              }
          end

  end (* mkOperators *)
