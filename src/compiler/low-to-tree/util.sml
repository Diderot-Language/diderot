(* util.sml
 *
 * Various common code for the Low IR to Tree Ir translation.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure Util : sig

  (* translate a Low IR type to a Tree IR type *)
    val trType : LowTypes.ty -> TreeTypes.t

  (* translate a Low IR type to a Tree IR type, where the type is for a local or temporary
   * variable.  The difference between this translation and trType is that tensors are
   * mapped to references to avoid unnecessary copying.
   *)
    val trTempType : LowTypes.ty -> TreeTypes.t

  (* translate a Low IR function parameter type to a Tree IR type *)
    val trParamType : LowTypes.ty -> TreeTypes.t

  (* translate a Low IR type to an APIType *)
    val toAPIType : LowTypes.ty -> APITypes.t

  (* create new tree IL variables *)
    val newParamVar : LowIR.var -> TreeVar.t
    val newLocalVar : LowIR.var -> TreeVar.t
    val newMemVar   : LowIR.var -> TreeVar.t  (* non-reference local variable *)
    val newIterVar  : LowIR.var -> TreeVar.t
    val newTempVar  : string * TreeTypes.t -> TreeVar.t

  (* create a list of variables for the pieces of a vector layout *)
    val newVectorVars : TreeTypes.vec_layout -> TreeVar.t list

  (* check to see if a block needs either a pointer to the world or a pointer to the
   * globals.
   *)
    val analyzeBlock : TreeIR.block -> {usesGlobals : bool, needsWorld : bool}

  end = struct

    structure IR = TreeIR
    structure Op = TreeOps

    fun trType ty = (case ty
           of LowTypes.BoolTy => TreeTypes.BoolTy
            | LowTypes.StringTy => TreeTypes.StringTy
            | LowTypes.IntTy => TreeTypes.IntTy
            | LowTypes.TensorTy[] => TreeTypes.realTy
            | LowTypes.TensorTy dd => TreeTypes.TensorTy dd
            | LowTypes.TupleTy tys => TreeTypes.TupleTy(List.map trType tys)
            | LowTypes.SeqTy(ty, dim) => TreeTypes.SeqTy(trType ty, dim)
            | LowTypes.ImageTy info => TreeTypes.ImageTy info
            | LowTypes.StrandTy n => TreeTypes.StrandIdTy n
          (* end case *))

  (* for local temporaries, we use tensor refs to avoid copying *)
    fun trTempType (ty as LowTypes.TensorTy(shp as _::_)) = TreeTypes.TensorRefTy shp
      | trTempType ty = trType ty

  (* for parameters, we use tensor refs to avoid copying and we use strand pointers.
   * Note that user-defined functions cannot have strand parameters, so this translation
   * only affects map functions.
   *)
    fun trParamType (ty as LowTypes.TensorTy(shp as _::_)) = TreeTypes.TensorRefTy shp
      | trParamType (LowTypes.StrandTy n) = TreeTypes.StrandIdTy n
      | trParamType ty = trType ty

    fun toAPIType ty = (case ty
           of LowTypes.BoolTy => APITypes.BoolTy
            | LowTypes.StringTy => APITypes.StringTy
            | LowTypes.IntTy => APITypes.IntTy
            | LowTypes.TensorTy dd => APITypes.TensorTy dd
            | LowTypes.SeqTy(ty, dim) => APITypes.SeqTy(toAPIType ty, dim)
            | LowTypes.ImageTy info =>
                APITypes.ImageTy(ImageInfo.dim info, ImageInfo.voxelShape info)
            | _ => raise Fail("toAPIType: unexpected " ^ LowTypes.toString ty)
          (* end case *))

    local
      val cnt = ref 0
      fun newVar trTy prefix x = let
            val n = !cnt
            in
              cnt := n+1;
              TreeVar.new (
                String.concat[prefix, LowIR.Var.name x, "_", Int.toString n],
                trTy (LowIR.Var.ty x))
            end
    in

    val newParamVar = newVar trParamType "p_"
    val newLocalVar = newVar trTempType "l_"
    val newIterVar = newVar trTempType "i_"
    val newMemVar = newVar trType "l_"

    fun newTempVar (prefix, ty) = let
          val n = !cnt
          in
            cnt := n+1;
            TreeVar.new (String.concat[prefix, "_", Int.toString n], ty)
          end

    fun newVectorVars layout = let
          fun newV ty = let
                val n = !cnt
                in
                  cnt := n+1;
                  TreeVar.new ("v_" ^ Int.toString n, ty)
                end
          in
            List.map newV (TreeTypes.piecesOf layout)
          end

    end (* local *)

    fun analyzeBlock blk = let
          fun chkBlock (IR.Block{body, ...}, uG, nW) =
                chkStms (body, uG, nW)
          and chkStms ([], uG, nW) = (uG, nW)
            | chkStms (stm::stms, uG, nW) = let
                fun next (true, true) = (true, true)
                  | next (uG, nW) = chkStms (stms, uG, nW)
                in
                  case stm
                   of IR.S_Assign(_, _, e) => next (chkExp (e, uG, nW))
                    | IR.S_MAssign(_, e) => next (chkExp (e, uG, nW))
                    | IR.S_GAssign(_, e) => next (chkExp (e, true, nW))
                    | IR.S_IfThen(e, blk) => let
                        val (uG, nW) = chkExp (e, uG, nW)
                        in
                          next (chkBlock (blk, uG, nW))
                        end
                    | IR.S_IfThenElse(e, blk1, blk2) => let
                        val (uG, nW) = chkExp (e, uG, nW)
                        val (uG, nW) = chkBlock (blk1, uG, nW)
                        in
                          next (chkBlock (blk2, uG, nW))
                        end
                    | IR.S_For(_, e1, e2, blk) => let
                        val (uG, nW) = chkExp (e1, uG, nW)
                        val (uG, nW) = chkExp (e2, uG, nW)
                        in
                          next (chkBlock (blk, uG, nW))
                        end
                    | IR.S_Foreach(_, e, blk) => let
                        val (uG, nW) = chkExp (e, uG, nW)
                        in
                          next (chkBlock (blk, uG, nW))
                        end
                    | IR.S_LoadNrrd _ => raise Fail "unexpected LoadNrrd"
                    | IR.S_Input _ => raise Fail "unexpected Input"
                    | IR.S_InputNrrd _ => raise Fail "unexpected InputNrrd"
                    | IR.S_New(_, es) => next (chkExps (es, uG, true))
                    | IR.S_Save(_, e) => next (chkExp (e, uG, nW))
                    | IR.S_Print(_, es) => next (chkExps (es, uG, true))
                    | IR.S_KillAll => next (uG, true)
                    | IR.S_StabilizeAll => next (uG, true)
                    | IR.S_Return(SOME e) => next (chkExp (e, uG, nW))
                    | _ => next (uG, nW)
                  (* end case *)
                end
          and chkExps ([], uG, nW) = (uG, nW)
            | chkExps (_, true, true) = (true, true)
            | chkExps (e::es, uG, nW) = let
                val (uG, nW) = chkExp (e, uG, nW)
                in
                  chkExps (es, uG, nW)
                end
          and chkExp (IR.E_Global gv, _, nW) = (true, nW)
            | chkExp (IR.E_State(SOME e, _), uG, nW) = chkExp (e, uG, nW)
            | chkExp (IR.E_Op(Op.SphereQuery _, es), uG, nW) = chkExps (es, uG, true)
            | chkExp (IR.E_Op(Op.NumStrands _, es), uG, nW) = chkExps (es, uG, true)
            | chkExp (IR.E_Op(_, es), uG, nW) = chkExps (es, uG, nW)
            | chkExp (IR.E_Apply(f, es), uG, nW) =
                chkExps (es, TreeFunc.hasGlobals f orelse uG, TreeFunc.needsWorld f orelse nW)
            | chkExp (IR.E_Vec(_, _, es), uG, nW) = chkExps (es, uG, nW)
            | chkExp (IR.E_Cons(es, _), uG, nW) = chkExps (es, uG, nW)
            | chkExp (IR.E_Seq(es, _), uG, nW) = chkExps (es, uG, nW)
            | chkExp (IR.E_Pack(_, es), uG, nW) = chkExps (es, uG, nW)
            | chkExp (IR.E_VLoad(_, e, _), uG, nW) = chkExp (e, uG, nW)
            | chkExp (_, uG, nW) = (uG, nW)
          val (usesGlobals, needsWorld) = chkBlock (blk, false, false)
          in
            {usesGlobals = usesGlobals, needsWorld = needsWorld}
          end

  end
