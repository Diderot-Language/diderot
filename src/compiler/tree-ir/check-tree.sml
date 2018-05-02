(* check-tree.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * TODO: check global and state variable consistency
 *)

(* FIXME: the checking function should be parameterized over the vector layout of the target *)

structure CheckTree : sig

    val check : string * TreeIR.program -> bool

  end = struct

    structure IR = TreeIR
    structure Op = TreeOps
    structure Ty = TreeTypes
    structure GVar = TreeGlobalVar
    structure SVar = TreeStateVar
    structure Var = TreeVar
    structure VSet = Var.Set

    datatype token
      = NL | S of string | I of int | A of Atom.atom | V of IR.var
      | TY of Ty.t | TYS of Ty.t list

    fun error errBuf toks = let
          fun tok2str NL = "\n  ** "
            | tok2str (S s) = s
            | tok2str (I i) = Int.toString i
            | tok2str (A s) = Atom.toString s
            | tok2str (V x) = Var.toString x
            | tok2str (TY ty) = Ty.toString ty
            | tok2str (TYS []) = "()"
            | tok2str (TYS[ty]) = Ty.toString ty
            | tok2str (TYS tys) = String.concat[
                  "(", String.concatWith " * " (List.map Ty.toString tys), ")"
                ]
          in
            errBuf := concat ("**** Error: " :: List.map tok2str toks)
              :: !errBuf
          end

    exception BadVecType of int
    exception InvalidOp of string * Ty.t

    fun chkIndex (idx, bnd) = ((0 <= idx) andalso (idx < bnd))

  (* turn an expression of type TensorTy to one of TensorTyRef *)
    fun mkRefTy (Ty.TensorTy(shp as _::_)) = Ty.TensorRefTy shp
      | mkRefTy ty = ty

  (* Return the signature of a TreeIR operator. *)
    fun sigOfOp (vecTy, rator) = (case rator
           of Op.IAdd => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.ISub => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.IMul => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.IDiv => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.IMod => (Ty.IntTy, [Ty.IntTy, Ty.IntTy])
            | Op.INeg => (Ty.IntTy, [Ty.IntTy])
            | Op.RAdd => (Ty.realTy, [Ty.realTy, Ty.realTy])
            | Op.RSub => (Ty.realTy, [Ty.realTy, Ty.realTy])
            | Op.RMul => (Ty.realTy, [Ty.realTy, Ty.realTy])
            | Op.RDiv => (Ty.realTy, [Ty.realTy, Ty.realTy])
            | Op.RNeg => (Ty.realTy, [Ty.realTy])
            | Op.RClamp => (Ty.realTy, [Ty.realTy, Ty.realTy, Ty.realTy])
            | Op.RLerp => (Ty.realTy, [Ty.realTy, Ty.realTy, Ty.realTy])
            | Op.RCeiling => (Ty.realTy, [Ty.realTy])
            | Op.RFloor => (Ty.realTy, [Ty.realTy])
            | Op.RRound => (Ty.realTy, [Ty.realTy])
            | Op.RTrunc => (Ty.realTy, [Ty.realTy])
            | Op.RealToInt => (Ty.IntTy, [Ty.realTy])
            | Op.LT ty => (Ty.BoolTy, [ty, ty])
            | Op.LTE ty => (Ty.BoolTy, [ty, ty])
            | Op.EQ ty => (Ty.BoolTy, [ty, ty])
            | Op.NEQ ty => (Ty.BoolTy, [ty, ty])
            | Op.GT ty => (Ty.BoolTy, [ty, ty])
            | Op.GTE ty => (Ty.BoolTy, [ty, ty])
            | Op.BAnd => (Ty.BoolTy, [Ty.BoolTy, Ty.BoolTy])
            | Op.BOr => (Ty.BoolTy, [Ty.BoolTy, Ty.BoolTy])
            | Op.BNot => (Ty.BoolTy, [Ty.BoolTy])
            | Op.Abs ty => (ty, [ty])
            | Op.Max ty => (ty, [ty, ty])
            | Op.Min ty => (ty, [ty, ty])
            | Op.VAdd d => (vecTy d, [vecTy d, vecTy d])
            | Op.VSub d => (vecTy d, [vecTy d, vecTy d])
            | Op.VScale d => (vecTy d, [Ty.realTy, vecTy d])
            | Op.VMul d => (vecTy d, [vecTy d, vecTy d])
            | Op.VNeg d => (vecTy d, [vecTy d])
            | Op.VSum d => (Ty.realTy, [vecTy d])
            | Op.VDot d => (Ty.realTy, [vecTy d, vecTy d])
            | Op.VIndex(d, pw, idx) =>
                if chkIndex (idx, d)
                  then (Ty.realTy, [vecTy(d, pw)])
                  else raise InvalidOp("invalid index", Ty.realTy)
            | Op.VCeiling d => (vecTy d, [vecTy d])
            | Op.VFloor d => (vecTy d, [vecTy d])
            | Op.VRound d => (vecTy d, [vecTy d])
            | Op.VTrunc d => (vecTy d, [vecTy d])
            | Op.VToInt layout =>
                (Ty.SeqTy(Ty.IntTy, SOME(#wid layout)), Ty.piecesOf layout)
            | Op.TensorIndex(ty as Ty.TensorTy shp, idxs) =>
                if ListPair.allEq chkIndex (idxs, shp)
                  then (Ty.realTy, [ty])
                  else raise InvalidOp("invalid index", Ty.realTy)
            | Op.TensorIndex(ty as Ty.TensorRefTy shp, idxs) =>
                if ListPair.allEq chkIndex (idxs, shp)
                  then (Ty.realTy, [ty])
                  else raise InvalidOp("invalid index", Ty.realTy)
            | Op.ProjectLast(ty as Ty.TensorTy(shp as _::_::_), idxs) => let
                fun chk ([], [_]) = true
                  | chk (idx::idxs, d::dd) = chkIndex (idx, d) andalso chk (idxs, dd)
                  | chk _ = false
                in
                  if chk (idxs, shp)
                    then (Ty.TensorRefTy[List.last shp], [ty])
                    else raise InvalidOp("invalid index", Ty.TensorRefTy[List.last shp])
                end
            | Op.ProjectLast(ty as Ty.TensorRefTy(shp as _::_::_), idxs) => let
                fun chk ([], [_]) = true
                  | chk (idx::idxs, d::dd) = chkIndex (idx, d) andalso chk (idxs, dd)
                  | chk _ = false
                in
                  if chk (idxs, shp)
                    then (Ty.TensorRefTy[List.last shp], [ty])
                    else raise InvalidOp("invalid index", Ty.TensorRefTy[List.last shp])
                end
            | Op.TensorCopy shp => (Ty.TensorTy shp, [Ty.TensorRefTy shp])
            | Op.TensorRef shp => (Ty.TensorRefTy shp, [Ty.TensorTy shp])
            | Op.EigenVals2x2 => (Ty.SeqTy(Ty.realTy, SOME 2), [Ty.TensorRefTy[2, 2]])
            | Op.EigenVals3x3 => (Ty.SeqTy(Ty.realTy, SOME 3), [Ty.TensorRefTy[3, 3]])
            | Op.Select(ty as Ty.TupleTy tys, i) => (List.nth(tys, i-1), [ty])
            | Op.Subscript(ty as Ty.SeqTy(elemTy, _)) => (mkRefTy elemTy, [ty, Ty.intTy])
            | Op.MkDynamic(ty, n) => (Ty.SeqTy(ty, NONE), [Ty.SeqTy(ty, SOME n)])
            | Op.Prepend(seqTy,elemTy) => (Ty.SeqTy(seqTy, NONE), [elemTy, Ty.SeqTy(seqTy, NONE)])
            | Op.Append(seqTy,elemTy) => (Ty.SeqTy(seqTy, NONE), [Ty.SeqTy(seqTy, NONE), elemTy])
            | Op.Concat ty => (Ty.SeqTy(ty, NONE), [Ty.SeqTy(ty, NONE), Ty.SeqTy(ty, NONE)])
            | Op.Range => (Ty.SeqTy(Ty.intTy, NONE), [Ty.IntTy, Ty.IntTy])
            | Op.Length ty => (Ty.intTy, [Ty.SeqTy(ty, NONE)])
            | Op.SphereQuery(1, strandTy) =>
                (Ty.SeqTy(strandTy, NONE), [Ty.realTy, Ty.realTy])
            | Op.SphereQuery(dim, strandTy) =>
                (Ty.SeqTy(strandTy, NONE), [Ty.TensorRefTy[dim], Ty.realTy])
            | Op.Sqrt => (Ty.realTy, [Ty.realTy])
            | Op.Cos => (Ty.realTy, [Ty.realTy])
            | Op.ArcCos => (Ty.realTy, [Ty.realTy])
            | Op.Sin => (Ty.realTy, [Ty.realTy])
            | Op.ArcSin => (Ty.realTy, [Ty.realTy])
            | Op.Tan => (Ty.realTy, [Ty.realTy])
            | Op.ArcTan => (Ty.realTy, [Ty.realTy])
            | Op.Exp => (Ty.realTy, [Ty.realTy])
            | Op.Sign => (Ty.realTy, [Ty.realTy])
            | Op.IntToReal => (Ty.realTy, [Ty.intTy])
            | Op.NumStrands _ => (Ty.IntTy, [])
            | Op.Transform info => let
                val dim = ImageInfo.dim info
                in
                  if (dim = 1)
                    then (Ty.realTy, [Ty.ImageTy info])
                    else (Ty.TensorRefTy[dim, dim], [Ty.ImageTy info])
                end
            | Op.Translate info => let
                val dim = ImageInfo.dim info
                in
                  if (dim = 1)
                    then (Ty.realTy, [Ty.ImageTy info])
                    else (Ty.TensorRefTy[dim], [Ty.ImageTy info])
                end
            | Op.ControlIndex(info, _, _) => (Ty.IntTy, [Ty.ImageTy info, Ty.IntTy])
            | Op.LoadVoxel info => (Ty.realTy, [Ty.ImageTy info, Ty.IntTy])
            | Op.Inside(layout, info, _) =>
                (Ty.BoolTy, TreeTypes.piecesOf layout @ [Ty.ImageTy info])
            | Op.IndexInside(info, _) => let
                val idxTy = (case ImageInfo.dim info
                       of 1 => Ty.IntTy
                        | d => Ty.SeqTy(Ty.IntTy, SOME d)
                      (* end case *))
                in
                  (Ty.BoolTy, [idxTy, Ty.ImageTy info])
                end
            | Op.ImageDim(info, _) => (Ty.IntTy, [Ty.ImageTy info])
            | Op.MathFn f => MathFns.sigOf (Ty.realTy, f)
            | _ => raise Fail("sigOf: invalid operator " ^ Op.toString rator)
          (* end case *))

  (* utility function for synthesizing eigenvector/eigenvalue signature *)
    fun eigenSig dim = let
          val resTy = [
                  Ty.SeqTy(Ty.realTy, SOME dim),
                  Ty.SeqTy(Ty.TensorTy[dim], SOME dim)
                ]
          in
            (resTy, [Ty.TensorTy[dim, dim]])
          end

    fun msigOf rator = (case rator
           of Op.EigenVecs2x2 => eigenSig 2
            | Op.EigenVecs3x3 => eigenSig 3
            | _ => raise Fail("msigOf: invalid operator " ^ Op.toString rator)
          (* end case *))

    fun check (phase, prog) = let
          val IR.Program{
                  props, target={layout, ...}, consts, inputs, constInit,
                  globals, funcs, globInit, strand, create, start, update
                } = prog
          val errBuf = ref []
          val errFn = error errBuf
       (* handle exceptions *)
          fun onExn exn =
                errFn (S "uncaught exception: " :: S(exnMessage exn) ::
                  List.foldr (fn (s, msg) => NL :: S "    raised at " :: S s :: msg)
                    [] (SMLofNJ.exnHistory exn))
          fun final () = (case !errBuf
                 of [] => false
                  | errs => (
                      Log.msg ["********** IR Errors detected after ", phase, " **********\n"];
                      List.app (fn msg => Log.msg [msg, "\n"]) (List.rev errs);
                      true)
                (* end case *))
          fun sigOf rator = let
                fun vecTy (d, pw) = let
                      fun invalid () = (
                            errFn [
                                S "invalid width ", I d, S " for ", S(Op.toString rator)
                              ];
                            Ty.VecTy(d, pw))
                      in
                        case layout d
                         of {padded, pieces=[w], ...} =>
                              if (w <> pw) then invalid() else Ty.VecTy(d, w)
                          | _ => invalid ()
                        (* end case *)
                      end
                in
                  sigOfOp (vecTy, rator)
                end
        (* check a variable use *)
          fun checkVar (bvs, x) = if VSet.member(bvs, x)
                then ()
                else errFn [S "variable ", V x, S " is not bound"]
          fun chkBlock (bvs : VSet.set, IR.Block{locals, body}) = let
                fun chkExp (cxt, bvs : VSet.set, e) = let
                      fun chk e = (case e
                             of IR.E_Global gv => GVar.ty gv
                              | IR.E_State(NONE, sv) => SVar.ty sv
                              | IR.E_State(SOME e, sv) => (
                                  case chk e
                                   of Ty.StrandIdTy _ => ()
                                    | ty => errFn [S "expected strand type, but found ", TY ty]
                                  (* end case *);
                                  SVar.ty sv)
                              | IR.E_Var x => (checkVar(bvs, x); Var.ty x)
                              | IR.E_Lit(Literal.Int _) => Ty.IntTy
                              | IR.E_Lit(Literal.Real _) => Ty.realTy
                              | IR.E_Lit(Literal.String _) => Ty.StringTy
                              | IR.E_Lit(Literal.Bool _) => Ty.BoolTy
                              | IR.E_Op(rator, args) => (let
                                  val (resTy, paramTys) = sigOf rator
                                  val argTys = List.map chk args
                                  in
                                    if ListPair.allEq Ty.same (paramTys, argTys)
                                      then ()
                                      else errFn [
                                          S "argument type mismatch in application of ",
                                          S(Op.toString rator), S(cxt()),
                                          NL, S "expected: ", TYS paramTys,
                                          NL, S "found:    ", TYS argTys
                                        ];
                                    resTy
                                  end
                                  handle InvalidOp(msg, ty) => (errFn [
                                      S msg, S " in operator ", S(Op.toString rator)
                                    ]; ty))
                              | IR.E_Apply(f, args) => let
                                  val (resTy, paramTys) = TreeFunc.ty f
                                  val argTys = List.map chk args
                                  in
                                    if ListPair.allEq Ty.same (paramTys, argTys)
                                      then ()
                                      else errFn [
                                          S "argument type mismatch in application of ",
                                          S(TreeFunc.toString f), S(cxt()),
                                          NL, S "expected: ", TYS paramTys,
                                          NL, S "found:    ", TYS argTys
                                        ];
                                    resTy
                                  end
                              | IR.E_Vec(w, pw, es) => let
                                  fun chkArg (i, e) = (case chk e
                                         of Ty.VecTy(1, 1) => () (* ok *)
                                          | ty => errFn [
                                                S "component ", I i,
                                                S " of vector does has type ", TY ty, S(cxt())
                                              ])
                                  val ty = Ty.VecTy(w, pw)
                                  in
                                    List.appi chkArg es;
                                    if (length es <> w)
                                      then errFn [
                                          S "expected ", I w,
                                          S " arguments to E_Vec, but found ", I(length es)
                                        ]
                                      else ();
                                    ty
                                  end
                              | IR.E_Cons([], ty) => (
                                  errFn [S "empty cons", S(cxt())];
                                  ty)
                              | IR.E_Cons(es, consTy as Ty.TensorTy dd) => let
                                  val nelems = List.foldl Int.* 1 dd
                                  in
                                    if (length es <> nelems)
                                      then errFn [
                                          S "cons has incorrect number of elements", S(cxt()),
                                          NL, S "  expected: ", I nelems,
                                          NL, S "  found:    ", I(length es)
                                        ]
                                      else ();
                                    chkElems ("cons", Ty.realTy, es);
                                    consTy
                                  end
                              | IR.E_Cons(es, ty) => (
                                  errFn [S "unexpected type for cons", S(cxt()), S ": ", TY ty];
                                  ty)
                              | IR.E_Seq([], ty as Ty.SeqTy(_, SOME 0)) => ty
                              | IR.E_Seq([], ty as Ty.SeqTy(_, SOME n)) => (
                                  errFn [S "empty sequence, but expected ", TY ty, S(cxt())];
                                  ty)
                              | IR.E_Seq(es, seqTy as Ty.SeqTy(ty, NONE)) => (
                                  chkElems ("sequence", ty, es);
                                  seqTy)
                              | IR.E_Seq(es, seqTy as Ty.SeqTy(ty, SOME n)) => (
                                  if (length es <> n)
                                    then errFn [
                                        S "sequence has incorrect number of elements", S(cxt()),
                                        NL, S "  expected: ", I n,
                                        NL, S "  found:    ", I(length es)
                                      ]
                                    else ();
                                  chkElems ("sequence", ty, es);
                                  seqTy)
                              | IR.E_Seq(es, ty) => (
                                  errFn [S "unexpected type for sequence", S(cxt()), S ": ", TY ty];
                                  ty)
                              | IR.E_Pack(layout, es) => let
                                  fun chkOne (i, ty, ty') = if Ty.same(ty, ty')
                                        then ()
                                        else errFn[
                                            S "mismatch in component ", I i,
                                            S " of PACK", S(cxt()),
                                            NL, S "  expected: ", TY ty',
                                            NL, S "  found:    ", TY ty
                                          ]
                                  in
                                    ListPair.appi chkOne (List.map chk es, Ty.piecesOf layout);
                                    Ty.TensorTy[#wid layout]
                                  end
                              | IR.E_VLoad(layout, e, i) => let
                                  val ty = chk e
                                  val expectedTy = Ty.TensorRefTy[#wid layout]
                                  in
                                    if Ty.same(ty, expectedTy)
                                      then ()
                                      else errFn [
                                          S "type mismatch in E_VLoad", S(cxt()),
                                          NL, S "  expected: ", TY expectedTy,
                                          NL, S "  found:    ", TY ty
                                        ];
                                    Ty.nthVec(layout, i)
                                  end
                            (* end case *))
                      and chkElems (cxt', ty, []) = ()
                        | chkElems (cxt', ty, e::es) = let
                            val ty' = chk e
                            in
                              if Ty.same(ty, ty')
                                then ()
                                else errFn [
                                    S "element of ", S cxt', S " has incorrect type", S(cxt()),
                                    NL, S "expected: ", TY ty,
                                    NL, S "found:    ", TY ty'
                                  ];
                              chkElems (cxt', ty, es)
                            end
                      in
                        chk e
                      end
                fun chkStm (stm, bvs : VSet.set) = (case stm
                       of IR.S_Comment _ => bvs
                        | IR.S_Assign(isDef, x, e) => let
                            val ty = chkExp (
                                  fn () => concat[" in assignment to local ", Var.name x],
                                  bvs, e)
                            in
                              if Ty.same(Var.ty x, ty)
                                then ()
                                else errFn[
                                    S "type mismatch in assignment to local ", S(Var.name x),
                                    NL, S "lhs: ", TY(Var.ty x),
                                    NL, S "rhs: ", TY ty
                                  ];
                              if isDef
                                then VSet.add(bvs, x)
                                else (checkVar(bvs, x); bvs)
                            end
                        | IR.S_MAssign(xs, IR.E_Op(rator, es)) => let
                            val lhsTys = List.map TreeVar.ty xs
                            val argTys = List.map (fn e => chkExp (fn _ => "", bvs, e)) es
                            val (resTys, paramTys) = msigOf rator
                            in
(* FIXME: complete *)
                              bvs
                            end
                        | IR.S_MAssign _ => (errFn[S "ill-formed MAssign"]; bvs)
                        | IR.S_GAssign(gv, e) => let
                            val ty = chkExp (
                                  fn () => concat[" assignment to global ", GVar.name gv],
                                  bvs, e)
                            in
                              if Ty.same(GVar.ty gv, ty)
                                then ()
                                else errFn[
                                    S "type mismatch in assignment to global ", S(GVar.name gv),
                                    NL, S "lhs: ", TY(GVar.ty gv),
                                    NL, S "rhs: ", TY ty
                                  ];
                              bvs
                            end
                        | IR.S_IfThen(e, b) => let
                            val ty = chkExp (fn () => " in if-then", bvs, e)
                            in
                              if Ty.same(ty, Ty.BoolTy)
                                then ()
                                else errFn[
                                    S "expected bool for if-then, but found ", TY ty
                                  ];
                              chkBlock (bvs, b);
                              bvs
                            end
                        | IR.S_IfThenElse(e, b1, b2) => let
                            val ty = chkExp (fn () => " in if-then-else", bvs, e)
                            in
                              if Ty.same(ty, Ty.BoolTy)
                                then ()
                                else errFn[
                                    S "expected bool for if-then-else, but found ", TY ty
                                  ];
                              chkBlock (bvs, b1);
                              chkBlock (bvs, b2);
                              bvs
                            end
                        | IR.S_For(x, e1, e2, b) => let
                            fun chkE e = (case chkExp (fn () => " in for", bvs, e)
                                   of Ty.IntTy => ()
                                    | ty => errFn [
                                          S "bound of for is not int type; found ", TY ty
                                        ]
                                  (* end case *))
                            in
                              if (Ty.same(Ty.IntTy, Var.ty x))
                                then ()
                                else errFn [
                                    S "iteration variable ", V x,
                                    S " in for loop has type ", TY(Var.ty x)
                                  ];
                              chkE e1; chkE e2;
                              ignore (chkBlock (VSet.add(bvs, x), b));
                              bvs
                            end
                        | IR.S_Foreach(x, e, b) => (
                            case chkExp (fn () => " in foreach", bvs, e)
                             of Ty.SeqTy(ty, _) =>
                                  if Ty.same(ty, Var.ty x)
                                    then ()
                                    else errFn [
                                        S "type mismatch in foreach ", V x,
                                        NL, S "variable type: ", TY(Var.ty x),
                                        NL, S "domain type:   ", TY ty
                                      ]
                              | ty => errFn [
                                    S "domain of foreach is not sequence type; found ", TY ty
                                  ]
                            (* end case *);
                            ignore (chkBlock (VSet.add(bvs, x), b));
                            bvs)
                        | IR.S_MapReduce(mrs, src) =>
(* FIXME: check body of map-reduce *)
                            List.foldl
                              (fn (IR.MapReduce(lhs, _, _, _, _), bvs) => VSet.add(bvs, lhs))
                                bvs mrs
                        | IR.S_LoadNrrd(x, ty, name, proxy) =>
                            VSet.add(bvs, x) (* FIXME: check type of x *)
                        | IR.S_Input(gv, _, _, NONE) => bvs
                        | IR.S_Input(gv, _, _, SOME e) => let
                            val ty = chkExp (fn () => concat[" in input ", GVar.name gv], bvs, e)
                            in
                              if Ty.same(GVar.ty gv, ty)
                                then ()
                                else errFn[
                                    S "type mismatch in default for input ", S(GVar.name gv),
                                    NL, S "expected: ", TY(GVar.ty gv),
                                    NL, S "found:    ", TY ty
                                  ];
                              bvs
                            end
                        | IR.S_InputNrrd(gv, _, _, _) => (
                            case GVar.ty gv
                             of Ty.SeqTy(_, NONE) => ()
                              | Ty.ImageTy _ => ()
                              | ty => errFn [
                                    S "input variable ", S(GVar.name gv), S " has bogus type ",
                                    TY ty, S " for lhs for InputNrrd"
                                  ]
                            (* end case *);
                            bvs)
                        | IR.S_New(_, es) => (
                            List.app (fn e => ignore (chkExp(fn () => concat[" in new"], bvs, e))) es;
                            bvs)
                        | IR.S_Save(sv, e) => let
                            val ty = chkExp (fn () => concat[" in save ", SVar.name sv], bvs, e)
                            in
                              if Ty.same(SVar.ty sv, ty)
                                then ()
                                else errFn[
                                    S "type mismatch in assignment to state variable ",
                                    S(SVar.name sv),
                                    NL, S "lhs: ", TY(SVar.ty sv),
                                    NL, S "rhs: ", TY ty
                                  ];
                              bvs
                            end
                        | IR.S_KillAll => bvs
                        | IR.S_StabilizeAll => bvs
                        | IR.S_Print(tys, es) => (
                            if (length tys <> length es)
                              then errFn [S "arity mismatch in print statement"]
                              else ();
                            ListPair.appi
                              (fn (i, ty, e) => let
                                val ty' = chkExp(fn () => concat[" in print"], bvs, e)
                                in
                                  if Ty.same(ty', ty)
                                    then ()
                                    else errFn[
                                        S "type mismatch in argument ", I i,
                                        S " of print",
                                        NL, S "expected:  ", TY ty,
                                        NL, S "but found: ", TY ty'
                                      ]
                                end)
                                (tys, es);
                            bvs)
                        | IR.S_Return NONE => bvs
                        | IR.S_Return(SOME e) => (
                            ignore (chkExp (fn () => concat[" in return"], bvs, e));
                            bvs)
                        | IR.S_Active => bvs
                        | IR.S_Stabilize => bvs
                        | IR.S_Die => bvs
                      (* end case *))
                val bvs = VSet.addList(bvs, !locals)
                in
                  ignore (List.foldl chkStm bvs body)
                end
          fun checkBlock arg = (ignore (chkBlock arg) handle ex => onExn ex)
          fun chkOptBlock (_, NONE) = ()
            | chkOptBlock (bvs, SOME blk) = checkBlock (bvs, blk)
          fun chkMethod (bvs, IR.Method{body, ...}) = checkBlock (bvs, body)
          fun chkOptMethod (_, NONE) = ()
            | chkOptMethod (bvs, SOME meth) = chkMethod (bvs, meth)
          fun chkStrand (IR.Strand{params, state, stateInit, startM, updateM, stabilizeM, ...}) = (
                ignore (chkMethod (VSet.fromList params, stateInit));
                chkOptMethod (VSet.empty, startM);
                ignore (chkMethod (VSet.empty, updateM));
                chkOptMethod (VSet.empty, stabilizeM))
          fun chkInput (Inputs.INP{var, ...}) = if GVar.isInput var
                then ()
                else errFn[S "non-input variable ", S(GVar.name var), S " in inputs list"]
          fun chkGlobal gv = if GVar.isInput gv
                then errFn[S "input variable ", S(GVar.name gv), S " in globals list"]
                else ()
          fun chkFunc (IR.Func{name, params, body}) = let
                val (retTy, paramTys) = TreeFunc.ty name
                fun chkParam (x, ty) = Ty.same(Var.ty x, ty)
                in
                  if ListPair.allEq chkParam (params, paramTys)
                    then ()
                    else errFn [
                        S "mismatch between parameter-type of ",
                        S(TreeFunc.toString name), S " and types of parameters",
                        NL, S "parameter type: ", TYS paramTys,
                        NL, S "parameters:     ", TYS(List.map Var.ty params)
                      ];
(* FIXME: check return type of body against function type *)
                  checkBlock (VSet.fromList params, body)
                end
          in
            List.app chkInput inputs;
            List.app chkGlobal globals;
            List.app chkFunc funcs;
            checkBlock (VSet.empty, constInit);
            checkBlock (VSet.empty, globInit);
            chkStrand strand;
            Create.app (fn code => checkBlock (VSet.empty, code)) create;
            chkOptBlock (VSet.empty, start);
            chkOptBlock (VSet.empty, update);
            final ()
          end

  end
