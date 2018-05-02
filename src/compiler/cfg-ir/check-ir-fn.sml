(* check-ir-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Correctness checker for SSA-based IRs.
 *
 * TODO:
 *      check that the state variables and method stateOut variables are all defined.
 *      check that functions are defined
 *)

signature OPERATOR_TY =
  sig
    type rator
    type ty

  (* returns the signature of a single-result operator as (rng, dom). *)
    val sigOf : rator -> ty * ty list

  (* returns the signature of a multi-result operator as (rng, dom). *)
    val msigOf : rator -> ty list * ty list

  (* return the type of a CONS, where the first argument is the annotated type
   * and the second argument is the list of argument types.  Returns false if
   * there is a type error.
   *)
    val typeOfCons : ty * ty list -> bool

  (* return true if the type is of the specified form *)
    val isBoolTy : ty -> bool
    val isStrandTy : ty -> bool

  end

functor CheckIRFn (

    structure IR : SSA
    structure OpTy : OPERATOR_TY
        where type rator = IR.Op.rator
        where type ty = IR.Ty.ty

  ) : sig

  (* check the program for type errors, etc.  The first argument will be used to
   * identify the phase that the check follows and the return result will be true
   * if any errors were detected.
   *)
    val check : string * IR.program -> bool

  end = struct

    structure IR = IR
    structure Ty = IR.Ty
    structure V = IR.Var
    structure VSet = V.Set

  (* forward analysis to determine the variables that are available in blocks *)
    structure Avail = ForwardDFAFn (
      struct

        structure IR = IR
        type t = VSet.set

        val bottom = VSet.empty

        fun join inputs = List.foldl VSet.union bottom inputs

        fun transfer (input, nd as IR.ND{kind, ...}) = (case kind
               of IR.JOIN{phis, ...} => let
                  (* add the lhs of the phi node.  We do not remove the rhs variables, since
                   * after value numbering, they may have further uses.
                   *)
                    fun doPhi ((y, _), vs) = VSet.add(vs, y)
                    val output = List.foldl doPhi input (!phis)
                    in
                      output
                    end
                | IR.FOREACH{var, phis, ...} => let
                  (* add the lhs of the phi node.  We do not remove the rhs variables, since
                   * after value numbering, they may have further uses.
                   *)
                    fun doPhi ((y, _), vs) = VSet.add(vs, y)
                    val output = List.foldl doPhi input (!phis)
                    in
                      VSet.add (output, var)
                    end
                | IR.ASSIGN{stm=(y, _), ...} => VSet.add(input, y)
                | IR.MASSIGN{stm=(ys, _), ...} => VSet.addList(input, ys)
                | _ => input
               (* end case *))

        val same = VSet.equal

        fun toString vs = let
              fun f (v, []) = [IR.Var.toString v, "}"]
                | f (v, l) = IR.Var.toString v :: "," :: l
              in
                if VSet.isEmpty vs then "{}" else String.concat("{" :: VSet.foldl f [] vs)
              end

      end)

    datatype token
      = NL | S of string | A of Atom.atom | V of IR.var | VTYS of IR.var list
      | TY of Ty.ty | TYS of Ty.ty list
      | ND of IR.node

    fun error errBuf toks = let
          fun tok2str NL = "\n  ** "
            | tok2str (S s) = s
            | tok2str (A s) = Atom.toString s
            | tok2str (V x) = concat[V.toString x, "#", Int.toString(V.useCount x)]
            | tok2str (VTYS xs) = tok2str(TYS(List.map V.ty xs))
            | tok2str (TY ty) = Ty.toString ty
            | tok2str (TYS []) = "()"
            | tok2str (TYS[ty]) = Ty.toString ty
            | tok2str (TYS tys) = String.concat[
                  "(", String.concatWith " * " (List.map Ty.toString tys), ")"
                ]
            | tok2str (ND nd) = IR.Node.toString nd
          in
            errBuf := concat ("**** Error: " :: List.map tok2str toks)
              :: !errBuf
          end

  (* property to track true use count of variables *)
    local
      val {getFn, setFn, clrFn, ...} = IR.Var.newProp (fn _ => 0)
    in
    fun incCnt x = let val n = getFn x in setFn(x, n+1) end
    fun use x = (incCnt x; x)
    val getCnt = getFn
    val clrCnt = clrFn
    end (* local *)

  (* property to mark if a variable has already been bound *)
    local
      val {getFn, setFn} = IR.Var.newFlag ()
    in
    val isBound = getFn
    fun markAsBound x = setFn (x, true)
    fun clrBound x = setFn (x, false)
    end

  (* clear properties on a variable *)
    fun clearVar x = (clrCnt x; clrBound x)

    fun checkAssign (errFn, bind) ((y, rhs), bvs) = let
        (* check a variable use *)
          fun checkVar x = if VSet.member(bvs, use x)
                then ()
                else errFn [
                    S "variable ", V x, S " is not bound in", NL,
                    S(IR.assignToString(y, rhs))
                  ]
          fun tyError (ty1, ty2) = errFn [
                  S "type mismatch in \"", S(IR.assignToString (y, rhs)), S "\"",
                  NL, S "lhs: ", TY ty1, NL, S "rhs: ", TY ty2
                ]
          fun checkTys (ty1, ty2) = if Ty.same(ty1, ty2)
                then ()
                else tyError (ty1, ty2)
          in
            (* check that y is not bound twice *)
              if isBound y
                then errFn [
                    S "variable ", V y, S " is bound twice in", NL,
                    S(IR.assignToString (y, rhs))
                  ]
                else bind y;
            (* check RHS *)
              case rhs
               of IR.GLOBAL x => checkTys(V.ty y, IR.GlobalVar.ty x)
                | IR.STATE(NONE, sv) => checkTys(V.ty y, IR.StateVar.ty sv)
                | IR.STATE(SOME x, sv) => (
                    if not(OpTy.isStrandTy(V.ty x))
                      then errFn [
                          S "expected strand type for ", V x, S ", but found ", TY(V.ty x)
                        ]
                      else ();
                    checkVar x; checkTys(V.ty y, IR.StateVar.ty sv))
                | IR.VAR x => (
                    checkVar x;
                    checkTys (V.ty y, V.ty x))
                | IR.LIT lit => let
                    val ty = (case lit
                           of Literal.Int _ => Ty.intTy
                            | Literal.Real _ => Ty.realTy
                            | Literal.String _ => Ty.StringTy
                            | Literal.Bool _ => Ty.BoolTy
                          (* end case *))
                    in
                      checkTys (V.ty y, ty)
                    end
                | IR.OP(rator, xs) => let
                    val (resTy, argTys) = OpTy.sigOf rator
                    in
                      List.app checkVar xs;
                      checkTys (V.ty y, resTy);
                      if ListPair.allEq (fn (x, ty) => Ty.same(V.ty x, ty)) (xs, argTys)
                        then ()
                        else errFn [
                            S "argument type mismatch in \"", S(IR.assignToString (y, rhs)), S "\"",
                            NL, S "expected: ", TYS argTys,
                            NL, S "found:    ", VTYS xs
                          ]
                    end
                | IR.CONS(xs, ty) => (
                    List.app checkVar xs;
                    if OpTy.typeOfCons (ty, List.map V.ty xs)
                      then checkTys (V.ty y, ty)
                      else errFn [S "invalid ", S(IR.assignToString(y, rhs))])
                | IR.SEQ(xs, ty) => (
                    List.app checkVar xs;
(* FIXME: check types of sequence elements *)())
                | IR.EINAPP(ein, xs) => (
                    List.app checkVar xs;
(* FIXME: check ein *)())
                | IR.APPLY(f, xs) => (
                    List.app checkVar xs;
(* FIXME: check against type of f; check that f is defined *)
                    ())
                | _ => errFn [S "bogus statement \"", S(IR.assignToString (y, rhs)), S "\""]
              (* end case *)
            end

    fun checkMAssign (errFn, bind) (stm as (ys, rhs), bvs) = let
        (* check that a lhs variable is not bound twice *)
          fun checkBind y = if isBound y
                then errFn [
                    S "variable ", V y, S " is bound twice in", NL,
                    S(IR.massignToString stm)
                  ]
                else bind y
        (* check a variable use *)
          fun checkVar x = if VSet.member(bvs, use x)
                then ()
                else errFn [
                    S "variable ", V x, S " is not bound in", NL,
                    S(IR.massignToString stm)
                  ]
          in
          (* check that the lhs variables are not bound twice *)
            List.app checkBind ys;
          (* check the rhs *)
            case rhs
             of IR.OP(rator, xs) => let
                (* get the types *)
                  val (resTys, argTys) = OpTy.msigOf rator
                  in
                    List.app checkVar xs;
                    if ListPair.allEq (fn (y, ty) => Ty.same(V.ty y, ty)) (ys, resTys)
                      then ()
                      else errFn [
                          S "type mismatch in \"", S(IR.massignToString stm), S "\"", NL,
                          S "lhs: ", VTYS ys, NL,
                          S "rhs: ", TYS resTys
                        ];
                    if ListPair.allEq (fn (x, ty) => Ty.same(V.ty x, ty)) (xs, argTys)
                      then ()
                      else errFn [
                          S "argument type mismatch in \"", S(IR.massignToString stm), S "\"",
                          NL, S "expected: ", TYS argTys,
                          NL, S "found:    ", VTYS xs
                        ]
                  end
              | IR.MAPREDUCE mrs => let
                  fun chkMR (red, mapf, xs) = List.app checkVar xs
                  in
                    List.app chkMR mrs
                  end
              | _ => errFn [S "bogus statement \"", S(IR.massignToString stm), S "\""]
            (* end case *)
            end

    fun checkPhi (errFn, bind) (nd, bvs, mask) (y, xs) = let
          val ty = V.ty y
          fun chkTy x = if Ty.same(V.ty x, ty)
                  then ()
                  else errFn [
                      S "type mismatch in \"", S(IR.phiToString (y, xs)), S "\"", NL,
                      S "typeof(", V y, S "): ", TY ty, NL,
                      S "typeof(", V x, S "): ", TY(V.ty x)
                    ]
          fun chkRHS ([], []) = ()
            | chkRHS (true::ms, NONE::xs) = chkRHS (ms, xs)
            | chkRHS (false::ms, (SOME x)::xs) = (
                if VSet.member(bvs, use x)
                  then ()
                  else errFn [
                      S "variable ", V x, S " is not bound in", NL,
                      S(IR.phiToString (y, xs))
                    ];
                chkTy x;
                chkRHS (ms, xs))
            | chkRHS _ = errFn [S "phi/mask mismatch in ", ND nd]
          in
          (* check that y is not bound twice *)
            if isBound y
              then errFn [
                  S "variable ", V y, S " is bound twice in", NL,
                  S(IR.phiToString (y, xs))
                ]
              else bind y;
          (* check that rhs vars have the correct type *)
            chkRHS (mask, xs)
          end

    fun checkUseCount errFn x = (
          if (IR.Var.useCount x <> getCnt x)
            then errFn [
                S "incorrect use count ", S(Int.toString(IR.Var.useCount x)),
                S " for ", V x, S " (actual count is ",
                S(Int.toString(getCnt x)), S ")"
              ]
            else ();
          clearVar x)

    fun checkCFG errFn (vs, cfg) = let
        (* compute the variables available on entry to each block *)
          val nodes = Avail.analyse (VSet.fromList vs, cfg)
        (* a list of the bound variables in the CFG *)
          val bvs = ref []
          val _ = List.app markAsBound vs
        (* mark a variable as bound and add it to the bvs list *)
          fun bind x = (markAsBound x; bvs := x :: !bvs)
        (* specialize the assignment checking functions *)
          val checkPhi = checkPhi (errFn, bind)
          val checkAssign = checkAssign (errFn, bind)
          val checkMAssign = checkMAssign (errFn, bind)
        (* check the edges of a node.  For each predecessor, the node should appear in the
         * successor list and for each successor, the node should appear in the predecessor
         * list.
         *)
          fun checkEdges nd = let
                fun eqNd nd' = IR.Node.same(nd, nd')
                fun chkPred src = if List.exists eqNd (IR.Node.succs src)
                      then ()
                      else errFn [
                          S "predecessor edge from ", ND nd, S " -> ", ND src,
                          S " not matched by successor edge"
                        ]
                fun chkSucc dst = if List.exists eqNd (IR.Node.preds dst)
                      then ()
                      else errFn [
                          S "successor edge from ", ND nd, S " -> ", ND dst,
                          S " not matched by predecessor edge"
                        ]
                in
                  List.app chkPred (IR.Node.preds nd);
                  List.app chkSucc (IR.Node.succs nd)
                end
          fun checkNd (nd as IR.ND{kind, ...}) = (case kind
                 of IR.NULL => errFn [S "unexpected ", ND nd]
                  | IR.JOIN{mask, phis, ...} =>
                      List.app
                        (checkPhi (nd, Avail.inValue nd, !mask))
                          (!phis)
                  | IR.COND{cond, ...} => (
                      if not(OpTy.isBoolTy(V.ty(!cond)))
                        then errFn [
                            S "expected bool type for ", V(!cond), S ", but found ",
                            TY(V.ty(!cond))
                          ]
                        else ();
                      if VSet.member(Avail.inValue nd, use (!cond))
                        then ()
                        else errFn [S "unbound variable ", V(!cond), S " in conditional"])
                  | IR.FOREACH{phis, mask, var, src, bodyExit, ...} => (
                      if isBound var
                        then errFn[]
                        else bind var;
                      if VSet.member(Avail.inValue nd, use(!src))
                        then ()
                        else errFn [S "unbound variable ", V(!src), S " in foreach"];
                      List.app (checkPhi (nd, Avail.inValue nd, !mask)) (!phis);
                      case IR.Node.kind(!bodyExit)
                       of IR.NEXT{succ, ...} =>
                            if IR.Node.same(nd, !succ)
                              then ()
                              else errFn [
                                  S "loop's body exit node ", ND(!bodyExit),
                                  S " does not point back to loop header"
                                ]
                        | _ => errFn [
                              S "bad bodyExit ", ND(!bodyExit), S " for loop ", ND nd
                            ]
                      (* end case *))
                  | IR.NEXT{succ, ...} => (case IR.Node.kind(!succ)
                       of IR.FOREACH{bodyExit, ...} =>
                            if IR.Node.same(nd, !bodyExit)
                              then ()
                              else errFn [
                                  S "next node ", ND nd, S " and loop ", ND(!succ),
                                  S " do not agree"
                                ]
                        | _ => errFn [
                              S "bad successor ", ND(!succ), S " for loop exit ", ND nd
                            ]
                      (* end case *))
                  | IR.ASSIGN{stm, ...} =>
                      checkAssign (stm, Avail.inValue nd)
                  | IR.MASSIGN{stm, ...} =>
                      checkMAssign (stm, Avail.inValue nd)
                  | IR.GASSIGN{lhs, rhs, ...} => let
                      val avail = Avail.inValue nd
                      in
                        if VSet.member(avail, use rhs)
                          then ()
                          else errFn [
                              S "variable ", V rhs, S " is not bound in global assignment ",
                              S(IR.GlobalVar.toString lhs)
                            ];
                        if Ty.same(IR.GlobalVar.ty lhs, V.ty rhs)
                          then ()
                          else errFn [
                              S "type mismatch in \"", S(IR.GlobalVar.toString lhs),
                              S " = ", S(V.toString rhs), S "\"",
                              NL, S "lhs: ", TY(IR.GlobalVar.ty lhs),
                              NL, S "rhs: ", TY(V.ty rhs)
                            ]
                      end
                  | IR.NEW{strand, args, ...} => let
                      val avail = Avail.inValue nd
                    (* check a variable use *)
                      fun checkVar x = if VSet.member(avail, use x)
                            then ()
                            else errFn [
                                S "variable ", V x, S " is not bound in new ",
                                S(Atom.toString strand)
                              ]
                      in
                        List.app checkVar args
                      end
                  | IR.SAVE{lhs, rhs, ...} => let
                      val avail = Avail.inValue nd
                      in
                        if VSet.member(avail, use rhs)
                          then ()
                          else errFn [
                              S "variable ", V rhs, S " is not bound in save ",
                              S(IR.StateVar.toString lhs)
                            ];
                        if Ty.same(IR.StateVar.ty lhs, V.ty rhs)
                          then ()
                          else errFn [
                              S "type mismatch in \"", S(IR.StateVar.toString lhs),
                              S " = ", S(V.toString rhs), S "\"",
                              NL, S "lhs: ", TY(IR.StateVar.ty lhs),
                              NL, S "rhs: ", TY(V.ty rhs)
                            ]
                      end
                  | IR.EXIT{kind=ExitKind.RETURN(SOME x), ...} =>
                      if VSet.member(Avail.inValue nd, use x)
                        then ()
                        else errFn [
                            S "variable ", V x, S " is not bound in return"
                          ]
                  | _ => ()
                (* end case *))
          in
            List.app checkEdges nodes;
            List.app checkNd nodes;
          (* clean up variables and check use counts *)
            List.app (checkUseCount errFn) (!bvs);
          (* cleanup *)
            Avail.scrub nodes
          end (* checkCFG *)

    fun check (phase, prog) = let
          val IR.Program{
                  props, consts, inputs, globals, funcs,
                  constInit, globInit, strand, create, start, update
                } = prog
          val errBuf = ref []
          val errFn = error errBuf
          fun final () = (case !errBuf
                 of [] => false
                  | errs => (
                      Log.msg ["********** IR Errors detected after ", phase, " **********\n"];
                      List.app (fn msg => Log.msg [msg, "\n"]) (List.rev errs);
                      true)
                (* end case *))
          val checkCFG = checkCFG errFn
        (* check a function definition *)
          fun checkFunc (IR.Func{name, params, body}) = checkCFG (params, body)
        (* check a strand definition *)
          fun checkStrand (IR.Strand{params, state, stateInit, startM, updateM, stabilizeM, ...}) =
                let
                val nStateVars = List.length state
                fun checkMethod body = checkCFG (params, body)
(*DEBUG*)handle ex => raise ex
                in
                  checkCFG (params, stateInit)
(*DEBUG*)handle ex => raise ex;
                  Option.app checkMethod startM;
                  checkMethod updateM;
                  Option.app checkMethod stabilizeM;
                  List.app (checkUseCount errFn) params
                end
        (* handle exceptions *)
          fun onExn exn =
                errFn (S "uncaught exception: " :: S(exnMessage exn) ::
                  List.foldr (fn (s, msg) => NL :: S "    raised at " :: S s :: msg)
                    [] (SMLofNJ.exnHistory exn))
          fun checkCFG' cfg = (checkCFG ([], cfg) handle ex => onExn ex)
          in
          (* check the input part *)
            checkCFG' constInit;
          (* check functions *)
            List.app checkFunc funcs;
          (* check the global part *)
            checkCFG' globInit;
          (* check initial strand creation *)
            Create.app checkCFG' create;
          (* check the strands *)
            (checkStrand strand) handle ex => onExn ex;
          (* check the optional global start *)
            Option.app checkCFG' start;
          (* check the optional global update *)
            Option.app checkCFG' update;
          (* check for errors *)
            final()
          end

  end
