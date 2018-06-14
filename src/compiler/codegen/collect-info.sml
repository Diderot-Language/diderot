(* collect-info.sml
 *
 * Collect information about the types and operations used in a program.  We need this
 * information to figure out what utility code to generate.
 *
 * The types are ordered so that base types are first, followed by TensorRefTy, followed
 * by TensorTy, followed by sequences and tuples.  Furthermore, the argument type of
 * a sequence appears before the sequence type.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure CollectInfo : sig

    type t

    datatype operation
      = Print of TreeTypes.t
      | RClamp | RLerp
      | VScale of int * int             (* `VScale(w, pw)`: scalar times vector; `w` is width of
                                         * vector and `pw` is the padded-width supported by
                                         * the hardware.
                                         *)
      | VSum of int * int               (* `VSum(w, pw)`: vector addition *)
      | VDot of int * int
      | VCeiling of int * int
      | VFloor of int * int
      | VRound of int * int
      | VTrunc of int * int
      | VToInt of TreeTypes.vec_layout
      | VLoad of int * int
      | VCons of int * int
      | VPack of TreeTypes.vec_layout
      | TensorCopy of int list
      | Transform of int
      | Translate of int
      | Inside of VectorLayout.t * int
      | EigenVals2x2
      | EigenVals3x3
      | EigenVecs2x2
      | EigenVecs3x3
      | SphereQuery of int * string
      | RIfWrap
      
    val collect : TreeIR.program -> t

    val listTypes : t -> TreeTypes.t list
    val listOps   : t -> operation list

  end = struct

    structure IR = TreeIR
    structure Ty = TreeTypes
    structure Op = TreeOps

    datatype operation
      = Print of TreeTypes.t
      | RClamp | RLerp
      | VScale of int * int
      | VSum of int * int
      | VDot of int * int
      | VCeiling of int * int
      | VFloor of int * int
      | VRound of int * int
      | VTrunc of int * int
      | VToInt of TreeTypes.vec_layout
      | VLoad of int * int
      | VCons of int * int
      | VPack of TreeTypes.vec_layout
      | TensorCopy of int list
      | Transform of int
      | Translate of int
      | Inside of VectorLayout.t * int
      | EigenVals2x2
      | EigenVals3x3
      | EigenVecs2x2
      | EigenVecs3x3
      | SphereQuery of int * string
      | RIfWrap
            

   (* operator to string (for debugging) *)
    local
      fun vop2s (rator, w, pw) = if (w = pw)
            then rator ^ Int.toString w
            else concat[rator, Int.toString w, "{", Int.toString pw, "}"]
    in
    fun toString rator = (case rator
           of Print ty => concat["Print<", TreeTypes.toString ty, ">"]
            | RClamp => "RClamp"
            | RLerp => "RLerp"
            | VScale(w, pw) => vop2s ("VScale", w, pw)
            | VSum(w, pw) => vop2s ("VSum", w, pw)
            | VDot(w, pw) => vop2s ("VDot", w, pw)
            | VCeiling(w, pw) => vop2s ("VCeiling", w, pw)
            | VFloor(w, pw) => vop2s ("VFloor", w, pw)
            | VRound(w, pw) => vop2s ("VRound", w, pw)
            | VTrunc(w, pw) => vop2s ("VTrunc", w, pw)
            | VToInt layout => "VToInt" ^ VectorLayout.toString layout
            | VLoad(w, pw) => vop2s ("VLoad", w, pw)
            | VCons(w, pw) => vop2s ("VCons", w, pw)
            | VPack layout => "VPack" ^ VectorLayout.toString layout
            | TensorCopy shape =>
                concat["TensorCopy[", String.concatWithMap "," Int.toString shape, "]"]
            | Transform d => concat["Transform", Int.toString d, "D"]
            | Translate d => concat["Translate", Int.toString d, "D"]
            | Inside({wid, ...}, s) =>
                concat["Inside", Int.toString wid, "D<", Int.toString s, ">"]
            | EigenVals2x2 => "EigenVals2x2"
            | EigenVals3x3 => "EigenVals3x3"
            | EigenVecs2x2 => "EigenVecs2x2"
            | EigenVecs3x3 => "EigenVecs3x3"
            | SphereQuery(d, s) => concat["SphereQuery", Int.toString d, "<", s, ">"]
            | RIfWrap  => "RIfWrap "
          (* end case *))
    end (* local *)

    structure OpTbl = HashTableFn (
      struct
        type hash_key = operation
        fun hashVal rator = (case rator
               of Print ty =>0w5 * TreeTypes.hash ty
                | RClamp => 0w13
                | RLerp => 0w17
                | VScale(w, _) => 0w19 + 0w7 * Word.fromInt w
                | VSum(w, _) => 0w23 + 0w7 * Word.fromInt w
                | VDot(w, _) => 0w29 + 0w7 * Word.fromInt w
                | VCeiling(w, _) => 0w43 + 0w7 * Word.fromInt w
                | VFloor(w, _) => 0w47 + 0w7 * Word.fromInt w
                | VRound(w, _) => 0w53 + 0w7 * Word.fromInt w
                | VTrunc(w, _) => 0w59 + 0w7 * Word.fromInt w
                | VToInt{wid, ...} => 0w61 + 0w7 * Word.fromInt wid
                | VLoad(w, _) => 0w67 + 0w7 * Word.fromInt w
                | VCons(w, _) => 0w71 + 0w7 * Word.fromInt w
                | VPack{wid, ...} => 0w79 + 0w7 * Word.fromInt wid
                | TensorCopy dd => 0w83 + List.foldl (fn (i, s) => (Word.fromInt i + 0w3*s)) 0w0 dd
                | Transform d => 0w89 + 0w7 * Word.fromInt d
                | Translate d => 0w97 + 0w7 * Word.fromInt d
                | Inside(layout, s) =>
                    0w101 + 0w7 *VectorLayout.hash layout + 0w13 * Word.fromInt s
                | EigenVals2x2 => 0w103
                | EigenVals3x3 => 0w107
                | EigenVecs2x2 => 0w109
                | EigenVecs3x3 => 0w113
                | SphereQuery(d, s) => 0w117 + 0w7 * Word.fromInt d + HashString.hashString s
                | RIfWrap  => 0w123
              (* end case *))
        fun sameKey (op1, op2) = (case (op1, op2)
               of (Print ty1, Print ty2) => TreeTypes.same(ty1, ty2)
                | (RClamp, RClamp) => true
                | (RLerp, RLerp) => true
                | (VScale(w1, _), VScale(w2, _)) => (w1 = w2)
                | (VSum(w1, _), VSum(w2, _)) => (w1 = w2)
                | (VDot(w1, _), VDot(w2, _)) => (w1 = w2)
                | (VCeiling(w1, _), VCeiling(w2, _)) => (w1 = w2)
                | (VFloor(w1, _), VFloor(w2, _)) => (w1 = w2)
                | (VRound(w1, _), VRound(w2, _)) => (w1 = w2)
                | (VTrunc(w1, _), VTrunc(w2, _)) => (w1 = w2)
                | (VToInt{wid=w1, ...}, VToInt{wid=w2, ...}) => (w1 = w2)
                | (VLoad(w1, _), VLoad(w2, _)) => (w1 = w2)
                | (VCons(w1, _), VCons(w2, _)) => (w1 = w2)
                | (VPack{wid=w1, ...}, VPack{wid=w2, ...}) => (w1 = w2)
                | (TensorCopy dd1, TensorCopy dd2) => ListPair.allEq (op =) (dd1, dd2)
                | (Transform d1, Transform d2) => (d1 = d2)
                | (Translate d1, Translate d2) => (d1 = d2)
                | (Inside(l1, s1), Inside(l2, s2)) =>
                    VectorLayout.same(l1, l2) andalso (s1 = s2)
                | (EigenVals2x2, EigenVals2x2) => true
                | (EigenVals3x3, EigenVals3x3) => true
                | (EigenVecs2x2, EigenVecs2x2) => true
                | (EigenVecs3x3, EigenVecs3x3) => true
                | (SphereQuery(d1, s1), SphereQuery(d2, s2)) => (d1 = d2) andalso (s1 = s2)
                | (RIfWrap,RIfWrap) => true
                | _ => false
              (* end case *))
      end)

    datatype t = Info of {
        tys : unit Ty.Tbl.hash_table,           (* mapping for types in program *)
        ops : unit OpTbl.hash_table             (* mapping for selected operations in the program *)
      }

    fun addType (Info{tys, ...}) = let
          val find = Ty.Tbl.find tys
          val ins = Ty.Tbl.insert tys
          fun addTy ty =  let
                fun insert ty = (case find ty
                       of NONE => ins (ty, ())
                        | SOME () => ()
                      (* end case *))
              (* insert a TensorTy or TensorRefTy, which means inserting both types (plus
               * the last-dimension vector type for 2nd-order and higher tensors)
               *)
                fun insertTensorTy (shp as [_]) = (
                      insert (Ty.TensorTy shp);
                      insert (Ty.TensorRefTy shp))
                  | insertTensorTy (shp as _::dd) = let
                      val d = List.last dd
                      in
                        insert (Ty.TensorTy shp);
                        insert (Ty.TensorRefTy shp);
                      (* we also need the vector types for the "last" member function *)
                        insert (Ty.TensorTy[d]);
                        insert (Ty.TensorRefTy[d])
                      end
                fun add ty = (case ty
                       of Ty.BoolTy => ()
                        | Ty.IntTy => ()
                        | Ty.StringTy => ()
                        | Ty.VecTy(1, 1) => ()
                        | Ty.TensorTy shp => insertTensorTy shp
                        | Ty.TensorRefTy shp => insertTensorTy shp
                        | Ty.StrandIdTy _ => ()
                        | Ty.TupleTy tys => (insert ty; List.app add tys)
                        | Ty.SeqTy(ty', _) => (insert ty; add ty')
                        | _ => insert ty
                      (* end case *))
                in
                  add ty
                end
          in
            addTy
          end

    fun insertOp (Info{ops, ...}) = let
          val find = OpTbl.find ops
          val ins = OpTbl.insert ops
          in
            fn rator => (case find rator
                 of NONE => ins (rator, ())
                  | SOME() => ()
                (* end case *))
          end

    fun addOp info = let
          val insert = insertOp info
          val addTy = addType info
          fun add' rator = (case rator
                 of Op.RClamp => insert RClamp
                  | Op.RLerp => insert RLerp
                  | Op.VScale(w, pw) => insert (VScale(w, pw))
                  | Op.VSum(w, pw) => insert (VSum(w, pw))
                  | Op.VDot(w, pw) => insert (VDot(w, pw))
                  | Op.VCeiling(w, pw) => insert (VCeiling(w, pw))
                  | Op.VFloor(w, pw) => insert (VFloor(w, pw))
                  | Op.VRound(w, pw) => insert (VRound(w, pw))
                  | Op.VTrunc(w, pw) => insert (VTrunc(w, pw))
                  | Op.VToInt layout => insert (VToInt layout)
                  | Op.ProjectLast(Ty.TensorTy(_::(dd as _::_)), _) =>
                      addTy (Ty.TensorRefTy[List.last dd])
                  | Op.ProjectLast(Ty.TensorRefTy(_::(dd as _::_)), _) =>
                      addTy (Ty.TensorRefTy[List.last dd])
                  | Op.TensorCopy shp => insert (TensorCopy shp)
                  | Op.EigenVecs2x2 => insert EigenVecs2x2
                  | Op.EigenVecs3x3 => insert EigenVecs3x3
                  | Op.EigenVals2x2 => insert EigenVals2x2
                  | Op.EigenVals3x3 => insert EigenVals3x3
                  | Op.SphereQuery(d, Ty.StrandIdTy s) => insert (SphereQuery(d, Atom.toString s))
                  | Op.Transform info => insert (Transform(ImageInfo.dim info))
                  | Op.Translate info => insert (Translate(ImageInfo.dim info))
                  | Op.Inside(layout, _, s) => insert (Inside(layout, s))
                  | Op.IfWrap => insert RIfWrap
                  | _ => ()
                (* end case *))
          in
            add'
          end

    fun collect prog = let
          val IR.Program{
                consts, inputs, constInit, globals, funcs, globInit,
                strand, create, start, update, ...
              } = prog
          val IR.Strand{params, state, stateInit, startM, updateM, stabilizeM, ...} = strand
          val info = Info{
                  tys = TreeTypes.Tbl.mkTable (64, Fail "tys"),
                  ops = OpTbl.mkTable (64, Fail "ops")
                }
          val addType = addType info
          val addOp = addOp info
          val insertOp = insertOp info
          fun insertPrint (Ty.TensorTy shp) = insertPrint (Ty.TensorRefTy shp)
            | insertPrint ty = (
                addType ty; insertOp (Print ty);
              (* add a printer for elements (when necessary) *)
                case ty
                 of Ty.TupleTy tys => List.app insertPrint tys
                  | Ty.SeqTy(ty', _) => insertPrint ty'
                  | _ => ()
                (* end case *))
          fun doGlobalV x = addType(TreeGlobalVar.ty x)
          fun doStateV x = addType(TreeStateVar.ty x)
          fun doV x = addType(TreeVar.ty x)
          fun doExp e = (case e
                 of IR.E_State(SOME e, sv) => doExp e
                  | IR.E_Op(rator, args) => (
                      case rator
                       of Op.Transform info => (case ImageInfo.dim info
                             of 1 => ()
                              | d => addType (Ty.TensorRefTy[d, d])
                            (* end case *))
                        | Op.Translate info => (case ImageInfo.dim info
                             of 1 => ()
                              | d => addType (Ty.TensorRefTy[d])
                            (* end case *))
                        | _ => ()
                      (* end case *);
                      addOp rator;
                      List.app doExp args)
                  | IR.E_Vec(w, pw, es) => (
                      addType(Ty.VecTy(w, pw));
                      insertOp (VCons(w, pw));
                      List.app doExp es)
                  | IR.E_Cons(es, ty) => (addType ty; List.app doExp es)
                  | IR.E_Seq(es, ty) => (addType ty; List.app doExp es)
                  | IR.E_Pack(layout, es) => (
                      List.app (fn ty => addType ty) (Ty.piecesOf layout);
                      insertOp (VPack layout);
                      List.app doExp es)
                  | IR.E_VLoad(layout, e, i) => let
                      val ty as Ty.VecTy(w, pw) = Ty.nthVec(layout, i)
                      in
                        addType ty;
                        insertOp (VLoad(w, pw));
                        doExp e
                      end
                  | _ => ()
                (* end case *))
          fun doStm stm = (case stm
                 of IR.S_Assign(isDecl, x, e) => (
                      if isDecl then doV x else ();
                      doExp e)
                  | IR.S_MAssign(_, e) => doExp e
                  | IR.S_GAssign(_, e) => doExp e
                  | IR.S_IfThen(e, b) => (doExp e; doBlk b)
                  | IR.S_IfThenElse(e, b1, b2) => (doExp e; doBlk b1; doBlk b2)
                  | IR.S_For(x, lo, hi, b) => (doV x; doExp lo; doExp hi; doBlk b)
                  | IR.S_Foreach(x, e, b) => (doV x; doExp e; doBlk b)
                  | IR.S_Input(_, _, _, SOME e) => doExp e
                  | IR.S_New(_, es) => List.app doExp es
                  | IR.S_Save(_, e) => doExp e
                  | IR.S_Print(tys, es) => (
                      List.app insertPrint tys;
                      List.app doExp es)
                  | IR.S_Return(SOME e) => doExp e
                  | _ => ()
                (* end case *))
          and doBlk (IR.Block{locals, body}) = (
                List.app doV (!locals);
                List.app doStm body)
          and doFunc (IR.Func{name, body, ...}) = let
                val (resTy, tys) = TreeFunc.ty name
                in
                  List.app addType (resTy::tys);
                  doBlk body
                end
          and doMethod (IR.Method{body, ...}) = doBlk body
          in
            List.app doGlobalV consts;
            List.app (doGlobalV o Inputs.varOf) inputs;
            List.app doGlobalV globals;
            List.app doStateV state;
            List.app doFunc funcs;
            doBlk constInit;
            doBlk globInit;
            doMethod stateInit;
            Option.app doMethod startM;
            doMethod updateM;
            Option.app doMethod stabilizeM;
            Create.app doBlk create;
            Option.app doBlk start;
            Option.app doBlk update;
            info
          end

  (* sort function for types; we need to sort the types to ensure that types are
   * declared before being used in another type.  Since the generated decls are
   * accumulated from last to first, we reverse the sort order.
   *)
    val tySort = let
        (* partial ordering on types is defined by a "depth" metric *)
          fun depth (Ty.TupleTy tys) = 4 + List.foldl (fn (ty, d) => Int.max(depth ty, d)) 0 tys
            | depth (Ty.SeqTy(ty, _)) = 4 + depth ty
            | depth (Ty.TensorTy[]) = 0
            | depth (Ty.TensorRefTy _) = 1
            | depth (Ty.TensorTy _) = 2
            | depth (Ty.ImageTy _) = 3
            | depth _ = 0
          fun gt (Ty.TensorTy dd1, Ty.TensorTy dd2) = List.length dd1 > List.length dd2
            | gt (Ty.TensorRefTy dd1, Ty.TensorRefTy dd2) = List.length dd1 > List.length dd2
            | gt (ty1, ty2) = (depth ty1 > depth ty2)
          in
            ListMergeSort.sort gt
          end

    fun listTypes (Info{tys, ...}) =
          tySort (TreeTypes.Tbl.foldi (fn (ty, _, acc) => ty::acc) [] tys)

    fun listOps (Info{ops, ...}) = OpTbl.foldi (fn (k, _, acc) => k::acc) [] ops

  end
