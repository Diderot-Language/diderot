(* translate.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Translate Simple-AST code into the HighIR representation.  This translation is based on the
 * algorithm described in
 *
 *      Single-pass generation of static single assignment form for structured languages
 *      ACM TOPLAS, Nov. 1994
 *      by Brandis and MossenBock.
 *)

structure Translate : sig

    val translate : Simple.program -> HighIR.program

  end = struct

    structure S = Simple
    structure Ty = SimpleTypes
    structure SV = SimpleVar
    structure VMap = SV.Map
    structure VSet = SV.Set
    structure IR = HighIR
    structure Op = HighOps
    structure DstTy = HighTypes
    structure Census = HighCensus
    structure Inp = Inputs

    val cvtTy = TranslateTy.tr

  (* code contexts *)
    datatype context = Method | GlobalUpdate | Other

  (* maps from SimpleAST variables to the current corresponding SSA variable *)
    datatype env = E of context * IR.var VMap.map

  (* mapping from differentiable field functions to their translated definitions *)
    local
      val {getFn : SimpleFunc.t -> IR.func_def, setFn, ...} =
            SimpleFunc.newProp (fn f => raise Fail(concat[
                "no binding for field function '", SimpleFunc.uniqueNameOf f, "'"
              ]))
    in
    val getFieldFnDef = getFn
    val setFieldFnDef = setFn
    end (* local *)

(* +DEBUG *)
    fun prEnv (prefix, E(_, env)) = let
          val wid = ref 0
          fun pr s = (print s; wid := !wid + size s)
          fun nl () = if (!wid > 0) then (print "\n"; wid := 0) else ()
          fun prElem (src, dst) = let
                val s = String.concat [
                        " ", SV.uniqueNameOf src, "->", IR.Var.toString dst
                      ]
                in
                  pr s;
                  if (!wid >= 100) then (nl(); pr " ") else ()
                end
          in
            pr prefix; pr " ENV: {"; nl(); pr " ";
            VMap.appi prElem env;
            nl(); pr "}"; nl()
          end
(* -DEBUG *)

  (* a property to map Simple variables to High IR state variables.  We need this to support
   * reading the state of other strands.
   *)
    val {getFn=getStateVar, ...} = let
          fun newSVar x = IR.StateVar.new (
                SV.kindOf x = SV.StrandOutputVar,
                SV.nameOf x, cvtTy(SV.typeOf x),
                AnalyzeSimple.varyingStateVar x,
                AnalyzeSimple.sharedStateVar x)
          in
            SV.newProp newSVar
          end

    fun emptyEnv cxt = E(cxt, VMap.empty)

    fun lookup (E(_, vMap)) x = (case VMap.find (vMap, x)
           of SOME x' => x'
            | NONE => raise Fail(concat[
                  "no binding for ", SV.kindToString(SV.kindOf x), " ",
                  SV.uniqueNameOf x, " in environment"
                ])
          (* end case *))

    fun find (E(_, vMap), x) = VMap.find(vMap, x)

    fun insert (E(cxt, vMap), x, x') = E(cxt, VMap.insert(vMap, x, x'))

    fun context (E(_, vMap), cxt) = E(cxt, vMap)

    fun inMethod (E(Method, _)) = true
      | inMethod _ = false

    fun inGlobalUpdate (E(GlobalUpdate, _)) = true
      | inGlobalUpdate _ = false

  (* create a new instance of a variable *)
    fun newVar x = IR.Var.new (SV.nameOf x, cvtTy(SV.typeOf x))

  (* is a Simple AST variable mapped to an IR.global_var? *)
    fun isGlobalVar x = (case SV.kindOf x
           of SV.ConstVar => true
            | SV.InputVar => true
            | SV.GlobalVar => true
            | _ => false
          (* end case *))

  (* convert a global and cache the result in a property *)
    local
      fun new x = let
            val kind = (case SV.kindOf x
                   of SV.ConstVar => IR.ConstVar
                    | SV.InputVar => IR.InputVar
                    | SV.GlobalVar => IR.GlobalVar
                    | k => raise Fail(concat[
                          "global variable ", SV.uniqueNameOf x,
                          " has kind ", SV.kindToString k
                        ])
                  (* end case *))
            in
              IR.GlobalVar.new(
                kind, AnalyzeSimple.updatedGlobal x, SV.nameOf x, cvtTy(SV.typeOf x))
            end
    in
    val {getFn = cvtGlobalVar, ...} = SV.newProp new
    end (* local *)

  (* convert a function variable and cache the result in a property *)
    local
      fun new f = let
            val (resTy, paramTys) = SimpleFunc.typeOf f
            in
              IR.Func.new(SimpleFunc.nameOf f, cvtTy resTy, List.map cvtTy paramTys)
            end
    in
    val {getFn = cvtFuncVar, ...} = SimpleFunc.newProp new
    end (* local *)

  (* generate fresh SSA variables and add them to the environment *)
    fun freshVars (env, xs) = let
          fun cvtVar (x, (env, xs)) = let
                val x' = newVar x
                in
                  (insert(env, x, x'), x'::xs)
                end
          val (env, xs) = List.foldl cvtVar (env, []) xs
          in
            (env, List.rev xs)
          end

  (* a pending-join node tracks the phi nodes needed to join the assignments
   * that flow into the join node.
   *)
    datatype join = JOIN of {
        env : env,                      (* the environment that was current at the conditional *)
                                        (* associated with this node. *)
        arity : int ref,                (* actual number of predecessors *)
        nd : IR.node,                   (* the CFG node for this pending join *)
        phiMap : (IR.var * IR.var list) VMap.map ref,
                                        (* a mapping from Simple AST variables that are assigned *)
                                        (* to their phi nodes. *)
        predKill : bool array           (* killed predecessor edges (because of DIE or STABILIZE *)
      }

  (* a stack of pending joins.  The first component specifies the path index of the current
   * path to the join.
   *)
    type pending_joins = (int * join) list

  (* create a new pending-join node for a conditional *)
    fun newJoin (env, arity) = JOIN{
            env = env, arity = ref arity, nd = IR.Node.mkJOIN [], phiMap = ref VMap.empty,
            predKill = Array.array(arity, false)
          }

  (* create a new pending-join node for a loop *)
    fun newForeach (env, x, xs, phiVars) = let
        (* for each assigned variable y in the body of the loop, we will need a phi node
         *      y'' = PHI(y', y''')
         * where y' is the binding of y coming into the loop and y''' is the binding of y
         * at the end of the loop body.  Since we don't know what y''' is at this point, we
         * just use y''.
         *)
          fun doVar (y, (env', phiMap)) = let
                val y' = lookup env y
                val y'' = newVar y
                in
                  (insert(env', y, y''), VMap.insert(phiMap, y, (y'', [y', y''])))
                end
          val (env', phiMap) = List.foldl doVar (env, VMap.empty) phiVars
          in
            JOIN{
                env = env',
                arity = ref 2,
                nd = IR.Node.mkFOREACH(x, xs),
                phiMap = ref phiMap,
                predKill = Array.array(2, false)
              }
          end

  (* record that a path to the top join in the stack has been killed because of RETURN,
   * DIE or STABILIZE
   *)
    fun killPath ((i, JOIN{arity, predKill, ...}) :: _) = (
          arity := !arity - 1;
          Array.update (predKill, i, true))
      | killPath _ = ()

  (* record an assignment to the IR variable dstVar (corresponding to the Simple AST variable
   * srcVar) in the current pending-join node.  The predIndex specifies which path into the
   * JOIN node this assignment occurs on.
   *)
    fun recordAssign ((predIndex, JOIN{env, phiMap, predKill, nd, ...})::_, srcVar, dstVar) = let
          val arity = Array.length predKill (* the original arity before any killPath calls *)
          val m = !phiMap
          in
            case find (env, srcVar)
             of NONE => () (* local temporary *)
              | SOME dstVar' => (case VMap.find (m, srcVar)
                   of NONE => let
                        val lhs = newVar srcVar
                        val rhs = List.tabulate (arity, fn i => if (i = predIndex) then dstVar else dstVar')
                        in
(**
print(concat["recordAssign: ", SV.uniqueNameOf srcVar, " --> ", IR.Var.toString lhs,
" @ ", IR.Node.toString nd, "\n"]);
**)
                          phiMap := VMap.insert (m, srcVar, (lhs, rhs))
                        end
                    | SOME(lhs, rhs) => let
                        fun update (i, l as x::r) = if (i = predIndex)
                              then dstVar::r
                              else x::update(i+1, r)
                          | update _ = raise Fail "invalid predecessor index"
                        in
                          phiMap := VMap.insert (m, srcVar, (lhs, update(0, rhs)))
                        end
                  (* end case *))
            (* end case *)
          end
      | recordAssign ([], _, _) = ()

  (* complete a pending join operation by filling in the phi nodes from the phi map and
   * updating the environment.
   *)
    fun commitJoin (joinStk, JOIN{env, arity, nd, phiMap, predKill}) = let
          val (preds, phis, mask) = (case IR.Node.kind nd
                 of IR.JOIN{preds, phis, mask, ...} => (!preds, phis, mask)
                  | IR.FOREACH{pred, bodyExit, phis, mask, ...} => ([!pred, !bodyExit], phis, mask)
                  | _ => raise Fail "invalid JOIN node"
                (* end case *))
        (* update the predKill array based on reachability *)
          val _ = let
                fun update (_, []) = ()
                  | update (i, nd::nds) = (
                      if IR.Node.isReachable nd then ()
                      else if Array.sub(predKill, i) then ()
                      else (arity := !arity-1; Array.update(predKill, i, true));
                      update (i+1, nds))
                in
                  update (0, preds)
                end
        (* compute the predecessor mask *)
          val mask' = Array.foldr (op ::) [] predKill
          in
            mask := mask';
            if (!arity = 0)
              then env (* all incoming edges are fake *)
            else if (!arity < Array.length predKill)
              then let
              (* filter out variables that correspond to fake preds from the RHS of a phi *)
                fun filterPhiRHS xs = let
                      fun f ([], _, xs') = List.rev xs'
                        | f (x::xs, i, xs') = if Array.sub(predKill, i)
                              then f (xs, i+1, NONE :: xs')
                              else f (xs, i+1, (SOME x) :: xs')
                      in
                        f (xs, 0, [])
                      end
                fun doVar (srcVar, phi as (dstVar, srcVars), (env, phis)) = (
(*
print(concat["doVar (", SV.uniqueNameOf srcVar, ", ", IR.phiToString phi, ", _) @ ", IR.Node.toString nd, "\n"]);
*)
                      recordAssign (joinStk, srcVar, dstVar);
                      (insert (env, srcVar, dstVar), (dstVar, filterPhiRHS srcVars)::phis))
                val (env, phis') = VMap.foldli doVar (env, []) (!phiMap)
                in
                  phis := phis';
                  env
                end
            else let
              fun doVar (srcVar, phi as (dstVar, xs), (env, phis)) = let
                    val xs = List.map SOME xs
                    in
(*
print(concat["doVar (", SV.uniqueNameOf srcVar, ", ", IR.phiToString phi, ", _) @ ", IR.Node.toString nd, "\n"]);
*)
                      recordAssign (joinStk, srcVar, dstVar);
                      IR.Var.setBinding (dstVar, IR.VB_PHI xs);
                      (insert (env, srcVar, dstVar), (dstVar, xs)::phis)
                    end
              val (env, phis') = VMap.foldli doVar (env, []) (!phiMap)
              in
                phis := phis';
                env
              end
          end
    fun gather(IR.ND{kind,...}) = 
        (case kind 
            of IR.ASSIGN{stm,pred,...} => (IR.ASSGN stm)::gather(!pred)
            |  IR.ENTRY _    => []
        (* end case*))
    fun tensorSize v = (case IR.Var.ty(v)
        of DstTy.TensorTy alpha => alpha
        | _ => raise Fail "Type is a not a tensor"
        (*end case*))           
    fun ft ((IR.ASSGN stm)::es) = concat["\n\t", IR.assignToString stm, ft(es)]
       | ft []  = ""
    fun ff (name, es) = print(concat["\n",name,ft(es)]) 
    fun iTo []  = " real "
     | iTo [b] = concat["vec", Int.toString b]
    fun iTos es = String.concatWithMap "," iTo es 
  (* expression translation *)
    fun cvtExp (env : env, lhs, exp) = (case exp
           of S.E_Var x => [IR.ASSGN(lhs, IR.VAR(lookup env x))]
            | S.E_Lit lit => [IR.ASSGN(lhs, IR.LIT lit)]
            | S.E_Kernel h => [IR.ASSGN(lhs, IR.OP(Op.Kernel(h, 0), []))]
            | S.E_Select(x, fld) => [IR.ASSGN(lhs, IR.STATE(SOME(lookup env  x), getStateVar fld))]
            | S.E_Apply(f, args) =>
                [IR.ASSGN(lhs, IR.APPLY(cvtFuncVar f, List.map (lookup env) args))]
            | S.E_Prim(f, tyArgs, args, ty) => let
                val args' = List.map (lookup env) args
                in
                  TranslateBasis.translate (lhs, f, tyArgs, args')
                end
            | S.E_Tensor(args, _) =>
                [IR.ASSGN(lhs, IR.CONS(List.map (lookup env) args, IR.Var.ty lhs))]
            | S.E_Seq(args, _) => [IR.ASSGN(lhs, IR.SEQ(List.map (lookup env) args, IR.Var.ty lhs))]
            | S.E_Tuple xs => raise Fail "FIXME: E_Tuple"
            | S.E_Project(x, i) => raise Fail "FIXME: E_Project"
            | S.E_Slice(x, indices, ty as Ty.T_Field{diff, dim, shape}) => let
                val x = lookup env x
                (* extract the integer indices from the mask *)
                val args' = List.mapPartial Fn.id indices
                val mask' = List.map Option.isSome indices
                val rator =  MkOperators.sliceF(mask', args', shape, dim)
                val ein = IR.EINAPP(rator, [x])
                in
                    [IR.ASSGN(lhs, ein)]
                end
            | S.E_Slice(x, indices, ty) => let
                val x = lookup env x
              (* check the indices to the slice.  There are two special cases: if all of the indices
               * are NONE, then the result is just the original tensor; and if all of the indices
               * are SOME ix, then the result is scalar so we use TensorIndex.
               *)
                fun chkIndices ([], _, true, idxs) = IR.OP(Op.TensorIndex(IR.Var.ty x, rev idxs), [x])
                  | chkIndices ([], true, _, _) = IR.VAR x (* all axes *)
                  | chkIndices (NONE :: r, true, _, _) = chkIndices (r, true, false, [])
                  | chkIndices (SOME ix :: r, _, true, idxs) = chkIndices (r, false, true, ix::idxs)
                  | chkIndices _ = let
                    (* extract the integer indices from the mask *)
                      val args' = List.mapPartial Fn.id indices
                      val mask' = List.map Option.isSome indices
                      val rator = (case (IR.Var.ty lhs, IR.Var.ty x, ty)
                             of (DstTy.TensorTy rstTy, DstTy.TensorTy argTy, _) =>
                                  MkOperators.sliceT (mask', args', rstTy, argTy)
                              | (_, _, Ty.T_Field{diff, dim, shape}) =>
                                  MkOperators.sliceF(mask', args', shape, dim)
                              | (_, _, _ ) => raise Fail "unsupported type"
                            (* end case *))
                      in
                        IR.EINAPP(rator, [x])
                      end
                in
                  [IR.ASSGN(lhs, chkIndices (indices, true, true, []))]
                end
            | S.E_Coerce{srcTy, dstTy, x} => (case (srcTy, dstTy)
                 of (Ty.T_Int, Ty.T_Tensor _) =>
                      [IR.ASSGN(lhs, IR.OP(Op.IntToReal, [lookup env x]))]
                  | (Ty.T_Sequence(ty, SOME n), Ty.T_Sequence(_, NONE)) =>
                      [IR.ASSGN(lhs, IR.OP(Op.MkDynamic(cvtTy ty, n), [lookup env x]))]
                  | (Ty.T_Field _, Ty.T_Field _) =>
                    (* change in continuity is a no-op *)
                      [IR.ASSGN(lhs, IR.VAR(lookup env x))]
                  | (Ty.T_Kernel, Ty.T_Kernel) =>
                    (* change in continuity is a no-op *)
                      [IR.ASSGN(lhs, IR.VAR(lookup env x))]
                  | _ => raise Fail(concat[
                        "unsupported type coercion: ", Ty.toString srcTy,
                        " ==> ", Ty.toString dstTy
                      ])
                (* end case *))
            | S.E_BorderCtl(ctl, img) => let
                val img = lookup env img
                val DstTy.ImageTy info = IR.Var.ty img
                val (rator, args) = (case ctl
                       of BorderCtl.Default x => (Op.BorderCtlDefault info, [lookup env x, img])
                        | BorderCtl.Clamp => (Op.BorderCtlClamp info, [img])
                        | BorderCtl.Mirror => (Op.BorderCtlMirror info, [img])
                        | BorderCtl.Wrap => (Op.BorderCtlWrap info, [img])
                      (* end case *))
                in
                  [IR.ASSGN(lhs, IR.OP(rator, args))]
                end
            | S.E_LoadSeq(ty, nrrd) => [IR.ASSGN(lhs, IR.OP(Op.LoadSeq(cvtTy ty, nrrd), []))]
            | S.E_LoadImage(_, nrrd, info) =>
                [IR.ASSGN(lhs, IR.OP(Op.LoadImage(DstTy.ImageTy info, nrrd), []))]
            | S.E_InsideImage(pos, img, s) => let
                val Ty.T_Image info = SV.typeOf img
                in
                  [IR.ASSGN(lhs, IR.OP(Op.Inside(info, s), [lookup env pos, lookup env img]))]
                end
            | S.E_FieldFn f => let
                (*  Variable convention used
                 - "alphas" tensor size
                 - "stmt"  statements 
                 - "lhs" high-ir variable
                 - "_comp"  function body 
                 -  "_PF" parameters treated like a fields  
                 - "_PT " parameters treated like a tensor 
                 *)
                 (*function body _comp*)
                val IR.Func{params, body,...} = getFieldFnDef f         
                (*Decompose function body *)
                val IR.CFG{entry, exit} = body                  
                (*get computation inside field definition  _comp*)
                val IR.ND{kind as IR.EXIT {pred, ...}, ...}  = exit                 
                val IR.ND{kind, ...} = !pred
                (*get last variable name used*) 
                val lhs_comp =
                    (case (kind, params)
                        of (IR.ASSIGN{stm as (lhs_comp, _),...},_) =>  lhs_comp
                        | (IR.ENTRY _, [lhs_comp])    => lhs_comp
                    (* end case*)) 
                (*get all the statements used in the function body *)               
                val stmt_comp = List.rev(gather (!pred))            
                (*analyze parameters*)                  
                val lhs_PF = params
                val lhs_PT = [] (*Fixed for now *)
                val lhs_allP  = lhs_PF@lhs_PT 
                (*for each argument set equal to a dummy var *)
                val stmt_allP =  List.map (fn v as IR.V{name,...} => IR.ASSGN(v, IR.LIT(Literal.String (name)))) (lhs_allP)     
                (*get tensor size of arguments*)
                val alphas_PF = List.map tensorSize lhs_PF
                val alphas_PT = List.map tensorSize lhs_PT
                val alphas_allP = alphas_PF@alphas_PT 
                val alpha_comp = tensorSize lhs_comp                                                            
                (*create ein operator*)
                val rator  = MkOperators.cfexpMix (alpha_comp, alphas_PF, alphas_PT)   
                val args =  lhs_comp::lhs_allP 
                val ein = IR.EINAPP(rator,args) 
   
                in
                     stmt_allP@ stmt_comp@[IR.ASSGN(lhs, ein)]
                end
          (* end case *))

  (* add nodes to save the varying strand state, followed by an exit node *)
    fun saveStrandState (env, (srcState, dstState), exit) = let
          val lookup = lookup env
          fun save (x, x', cfg) = if AnalyzeSimple.varyingStateVar x'
                then IR.CFG.appendNode (cfg, IR.Node.mkSAVE(x, lookup x'))
                else cfg  (* no need to save invariant variables! *)
          in
            IR.CFG.appendNode (
              ListPair.foldlEq save IR.CFG.empty (dstState, srcState),
              exit)
          end
(*DEBUG*)handle ex => raise ex

  (* convert a block to a CFG.  The parameters are:
   *    state   -- a pair of the src/dst state variables for saving the state of a strand.
   *               These are empty if the block is not in a strand.
   *    env     -- environment for mapping SimpleIR variables to HighIR locals
   *    joinStk -- a stack of pending joins
   *    blk     -- the block to translate
   *)
    fun cvtBlock (state, env : env, joinStk, blk as S.Block{code, ...}) = let
          fun cvt (env : env, cfg, []) = (cfg, env)
            | cvt (env, cfg, stm::stms) = (case stm
                 of S.S_Var(x, NONE) => let
                      val x' = newVar x
                      in
                        cvt (insert (env, x, x'), cfg, stms)
                      end
                  | S.S_Var(x, SOME e) => let
                      val x' = newVar x
                      val assigns = cvtExp (env, x', e)
                      in
                        recordAssign (joinStk, x, x');
                        cvt (
                          insert(env, x, x'),
                          IR.CFG.concat(cfg, IR.CFG.mkBlock assigns),
                          stms)
                      end
                  | S.S_Assign(lhs, rhs) => let
                      val lhs' = newVar lhs
                      val assigns = cvtExp (env, lhs', rhs)
                      in
                      (* check for assignment to global (i.e., constant, input, or other global) *)
(* FIXME: for the global initialization block, we should batch up the saving of globals until
 * the end so that we can properly set the bindings (i.e., so that we avoid conflicts between
 * branches of an if statement).
 *)
                        if isGlobalVar lhs
                          then cvt (
                            insert(env, lhs, lhs'),
                            IR.CFG.concat(
                              cfg,
                              IR.CFG.mkBlock(assigns @ [IR.GASSGN(cvtGlobalVar lhs, lhs')])),
                            stms)
                          else (
                            recordAssign (joinStk, lhs, lhs');
                            cvt (
                              insert(env, lhs, lhs'),
                              IR.CFG.concat(cfg, IR.CFG.mkBlock assigns),
                              stms))
                      end
                  | S.S_IfThenElse(x, b0, b1) => let
                      val x' = lookup env x
                      val join as JOIN{nd=joinNd, ...} = newJoin (env, 2)
                      val (cfg0, _) = cvtBlock (state, env, (0, join)::joinStk, b0)
                      val (cfg1, _) = cvtBlock (state, env, (1, join)::joinStk, b1)
                      val cond = IR.Node.mkCOND x'
                      fun addEdgeToJoin nd = (case IR.Node.kind nd
                             of IR.EXIT{succ, ...} => (
                                  succ := SOME joinNd;
                                  IR.Node.setPred (joinNd, nd))  (* will be converted to fake later *)
                              | _ => IR.Node.addEdge(nd, joinNd)
                            (* end case *))
                    (* package the CFG the represents the conditional (cond, two blocks, and join) *)
                      val condCFG = (
                            if IR.CFG.isEmpty cfg0
                              then (
                                IR.Node.setTrueBranch (cond, joinNd);
                                IR.Node.setPred (joinNd, cond))
                              else (
                                IR.Node.setTrueBranch (cond, IR.CFG.entry cfg0);
                                IR.Node.setPred (IR.CFG.entry cfg0, cond);
                                addEdgeToJoin (IR.CFG.exit cfg0));
                            if IR.CFG.isEmpty cfg1
                              then (
                                IR.Node.setFalseBranch (cond, joinNd);
                                IR.Node.setPred (joinNd, cond))
                              else (
                                IR.Node.setFalseBranch (cond, IR.CFG.entry cfg1);
                                IR.Node.setPred (IR.CFG.entry cfg1, cond);
                                addEdgeToJoin (IR.CFG.exit cfg1));
                            IR.CFG{entry = cond, exit = joinNd})
                      val env = commitJoin (joinStk, join)
                      val cfg = IR.CFG.concat (cfg, condCFG)
                      in
                      (* add an UNREACHABLE exit node when the join is the final node in the
                       * graph and it is unreachable.
                       *)
                        if List.null joinStk andalso not(IR.Node.isReachable joinNd)
                          then (* NOTE: this case implies that stms is empty! *)
                            (IR.CFG.appendNode(cfg, IR.Node.mkUNREACHABLE()), env)
                          else cvt (env, cfg, stms)
                      end
                  | S.S_Foreach(x, xs, b) => let
                      val x' = newVar x
                      val xs' = lookup env xs
                    (* For any local variable y that is both live on exit of the block b and
                     * assigned to in b, we will need a phi node for y.
                     *)
                      val phiVars = VSet.listItems(
                            VSet.intersection(AnalyzeSimple.assignedVars b, AnalyzeSimple.liveOut b))
                      val join as JOIN{env, nd=foreachNd, ...} = newForeach (env, x', xs', phiVars)
                      val (body, _) = cvtBlock (state, insert(env, x, x'), (1, join)::joinStk, b)
                      val body = IR.CFG.appendNode (body, IR.Node.mkNEXT())
                      val env = commitJoin (joinStk, join)
                      in
                      (* link in CFG edges *)
                        IR.Node.setBodyEntry (foreachNd, IR.CFG.entry body); (* loop header to body *)
                        IR.Node.setPred (IR.CFG.entry body, foreachNd);      (* back edge *)
                        IR.Node.setSucc (IR.CFG.exit body, foreachNd);
                        IR.Node.setBodyExit (foreachNd, IR.CFG.exit body);
                      (* process the rest of the block *)
                        cvt (env, IR.CFG.concat (cfg, IR.CFG{entry=foreachNd, exit=foreachNd}), stms)
                      end
                  | S.S_New(strandId, args) => let
                      val nd = IR.Node.mkNEW(strandId, List.map (lookup env) args)
                      in
                        cvt (env, IR.CFG.appendNode (cfg, nd), stms)
                      end
                  | S.S_KillAll => let
                      val nd = IR.Node.mkMASSIGN([], IR.OP(Op.KillAll, []))
                      in
                        cvt (env, IR.CFG.appendNode (cfg, nd), stms)
                      end
                  | S.S_StabilizeAll => let
                      val nd = IR.Node.mkMASSIGN([], IR.OP(Op.StabilizeAll, []))
                      in
                        cvt (env, IR.CFG.appendNode (cfg, nd), stms)
                      end
                  | S.S_Continue => (
                      killPath joinStk;
                      if inMethod env
                        then (
                            IR.CFG.concat (cfg, saveStrandState (env, state, IR.Node.mkACTIVE())),
                            env
                          )
                      else if inGlobalUpdate env
                        then (IR.CFG.appendNode (cfg, IR.Node.mkNEXTSTEP()), env)
                        else (IR.CFG.appendNode (cfg, IR.Node.mkRETURN NONE), env))
                  | S.S_Die => (
                      killPath joinStk;
                      (IR.CFG.appendNode (cfg, IR.Node.mkDIE ()), env))
                  | S.S_Stabilize => (
                      killPath joinStk;
                      (IR.CFG.concat (cfg, saveStrandState (env, state, IR.Node.mkSTABILIZE())), env))
                  | S.S_Return x => (
                      killPath joinStk;
                      (IR.CFG.appendNode (cfg, IR.Node.mkRETURN(SOME(lookup env x))), env))
                  | S.S_Print args => let
                      val args = List.map (lookup env) args
                      val nd = IR.Node.mkMASSIGN([], IR.OP(Op.Print(List.map IR.Var.ty args), args))
                      in
                        cvt (env, IR.CFG.appendNode (cfg, nd), stms)
                      end
                  | S.S_MapReduce mrs => let
                      fun cvtMR (mr, (env, assigns, lhs, mrs')) = let
                            val (S.MapReduce{
                                    result, reduction, mapf=S.Func{f, ...}, args, source, domain
                                  }) = mr
                          (* note that we are making the source of strands explicit and changing the
                           * type of the first argument to the map function.
                           *)
                            val strandTy = cvtTy(SV.typeOf source)
                            val src = IR.Var.new (SV.nameOf source, DstTy.SeqTy(strandTy, NONE))
                            val env' = insert (env, source, src)
                            val srcAssign = IR.ASSGN(src, IR.OP(Op.Strands(strandTy, domain), []))
                            val result' = newVar result
                            val env = insert(env, result, result')
                            val mr' = (reduction, cvtFuncVar f, List.map (lookup env') args)
                            in
                              (env, srcAssign :: assigns, result'::lhs, mr'::mrs')
                            end
                      val (env, assigns, lhs, mrs) = List.foldl cvtMR (env, [], [], []) mrs
                      val assigns = IR.MASSGN(List.rev lhs, IR.MAPREDUCE(List.rev mrs)) :: assigns
                      in
                        cvt (env, IR.CFG.appendBlock (cfg, List.rev assigns), stms)
                      end
                (* end case *))
          in
            cvt (env, IR.CFG.empty, code)
          end
(*DEBUG*)handle ex => raise ex

  (* a function for generating a block of assignments to load the globals that
   * are referenced in a SimpleIR block.
   *)
    fun loadGlobals (env, blk) = let
          fun load (x, (env, stms)) = let
                val x' = newVar x
                val stm = IR.ASSGN(x', IR.GLOBAL(cvtGlobalVar x))
                val env = insert (env, x, x')
                in
                  (env, stm::stms)
                end
          val globs = AnalyzeSimple.globalsOfBlock blk
          val (env, stms) = VSet.foldr load (env, []) globs
          in
            (IR.CFG.mkBlock stms, env)
          end

    fun cvtMethod (env, isStabilize, state, svars, blk) = let
        (* load the globals into fresh variables *)
          val (loadGlobsCFG, env) = loadGlobals (env, blk)
        (* load the state into fresh variables *)
          val (env, loadCFG) = let
              (* allocate shadow variables for the state variables *)
                val (env, stateIn) = freshVars (env, state)
                fun load (x, x') = IR.ASSGN(x, IR.STATE(NONE, x'))
                val cfg = IR.CFG.mkBlock (ListPair.map load (stateIn, svars))
                in
                  (env, IR.CFG.concat(loadGlobsCFG, cfg))
                end
        (* convert the body of the method *)
          val (cfg, env) = cvtBlock ((state, svars), env, [], blk)
        (* add the entry/exit nodes *)
          val entry = IR.Node.mkENTRY ()
          val loadCFG = IR.CFG.prependNode (entry, loadCFG)
          val exit = if isStabilize
                then IR.Node.mkRETURN NONE
                else IR.Node.mkACTIVE()
          val body = IR.CFG.concat (loadCFG, cfg)
          val body = if IR.Node.hasSucc(IR.CFG.exit body)
                then IR.CFG.concat (body, saveStrandState (env, (state, svars), exit))
                else IR.CFG{entry = IR.CFG.entry body, exit = exit}
          in
            body
          end
(*DEBUG*)handle ex => (print "error in cvtMethod\n"; raise ex)

  (* convert global code *)
    fun cvtGlobalBlock cxt block = let
        (* load the globals into fresh variables *)
          val (loadCFG, env) = loadGlobals (emptyEnv cxt, block)
        (* convert the code *)
          val (cfg, _) = cvtBlock (([], []), env, [], block)
          val cfg = IR.CFG.concat (loadCFG, cfg)
          val cfg = IR.CFG.prependNode (IR.Node.mkENTRY(), cfg)
          val cfg = if inGlobalUpdate env
                then IR.CFG.appendNode (cfg, IR.Node.mkNEXTSTEP())
                else IR.CFG.appendNode (cfg, IR.Node.mkRETURN NONE)
          in
            cfg
          end
(*DEBUG*)handle ex => raise ex

  (* extend the global environment with the strand's parameters *)
    fun initEnvFromParams params = let
          fun cvtParam (x, (env, xs)) = let
                val x' = newVar x
                in
                  (insert(env, x, x'), x'::xs)
                end
          val (env, params) = List.foldl cvtParam (emptyEnv Other, []) params
          in
            (env, List.rev params)
          end

  (* convert a function definition to a HighIR function *)
    fun cvtFunc (S.Func{f, params, body}) = let
        (* initialize the environment with the function's parameters *)
          val (env, params) = initEnvFromParams params
          val (loadBlk, env) = loadGlobals (env, body)
          val (bodyCFG, _) = cvtBlock (([], []), env, [], body)
          val cfg = IR.CFG.prependNode (IR.Node.mkENTRY(), loadBlk)
          val cfg = IR.CFG.concat(cfg, bodyCFG)
          val fdef = IR.Func{name = cvtFuncVar f, params = params, body = cfg}
          in
            if (SimpleFunc.isDifferentiable f) then setFieldFnDef(f, fdef) else ();
            fdef
          end

  (* lift functions used in map-reduce expressions *)
    fun liftFuncs NONE = []
      | liftFuncs (SOME blk) = let
          fun liftBlk (S.Block{code, ...}, fns) = List.foldl liftStm fns code
          and liftStm (S.S_IfThenElse(_, b1, b2), fns) = liftBlk(b2, liftBlk(b1, fns))
            | liftStm (S.S_Foreach(_, _, b), fns) = liftBlk(b, fns)
            | liftStm (S.S_MapReduce mrs, fns) =
                List.foldl (fn (S.MapReduce{mapf, ...}, fns) => cvtFunc mapf :: fns) fns mrs
            | liftStm (_, fns) = fns
          in
            liftBlk (blk, [])
          end

    fun translate prog = let
          val S.Program{
                  props, consts, inputs, constInit, globals, funcs,
                  globInit, strand, create, start, update
                } = prog
          val _ = AnalyzeSimple.analyze prog
          val consts' = List.map cvtGlobalVar consts
          val inputs' = List.map (Inputs.map cvtGlobalVar) inputs
          val inputs = List.map Inputs.varOf inputs
          val constInit = let
                val (cfg, _) = cvtBlock (([], []), emptyEnv Other, [], constInit)
                val cfg = IR.CFG.prependNode (IR.Node.mkENTRY(), cfg)
                val cfg = IR.CFG.appendNode (cfg, IR.Node.mkRETURN NONE)
                in
                  cfg
                end
          val globals' = List.map cvtGlobalVar globals
          val funcs' = List.map cvtFunc funcs
        (* if the program has global reductions, then lift those functions *)
          val funcs' = if Properties.hasProp Properties.GlobalReduce props
                then liftFuncs start @ liftFuncs update @ funcs'
                else funcs'
        (* create the global initialization code *)
          val globInit = let
              (* we start by loading the input globals, since they may be needed to compute the
               * other globals
               *)
                val (loadBlk, env) = loadGlobals (emptyEnv Other, globInit)
                val (globBlk, env) = cvtBlock (([], []), env, [], globInit)
                val cfg = IR.CFG.prependNode (IR.Node.mkENTRY(), loadBlk)
                val cfg = IR.CFG.concat(cfg, globBlk)
                val cfg = IR.CFG.appendNode (cfg, IR.Node.mkRETURN NONE)
                in
                  cfg
                end
(*DEBUG*)handle ex => raise ex
          fun cvtStrand strand = let
                val S.Strand{
                        name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                      } = strand
              (* initialize the environment with the strand's parameters *)
                val (env, params) = initEnvFromParams params
              (* create the state variables *)
                val svars = List.map getStateVar state
              (* convert the state initialization code *)
                val (stateInit, env) = let
                    (* load globals into local variables *)
                      val (loadGlobsCFG, env) = loadGlobals (env, stateInit)
                      val env = List.foldl (fn (x, env) => insert(env, x, newVar x)) env state
                      val (cfg, env) = cvtBlock (([], []), env, [], stateInit)
                      val cfg = IR.CFG.concat(loadGlobsCFG, cfg)
                      val cfg = IR.CFG.prependNode (IR.Node.mkENTRY(), cfg)
                    (* add nodes to initialize the strand state *)
                      val cfg = let
                            val lookup = lookup env
                            fun save (x, x', cfg) =
                                  IR.CFG.appendNode (cfg, IR.Node.mkSAVE(x, lookup x'))
                            in
                              IR.CFG.appendNode (
                                ListPair.foldlEq save cfg (svars, state),
                                IR.Node.mkRETURN NONE)
                            end
                      in
                        (cfg, env)
                      end
                fun cvtMeth isStabilize blk =
                      cvtMethod (context(env, Method), isStabilize, state, svars, blk)
                in
                  IR.Strand{
                      name = name,
                      params = params,
                      spatialDim = spatialDim,
                      state = svars,
                      stateInit = stateInit,
                      startM = Option.map (cvtMeth false) startM,
                      updateM = cvtMeth false updateM,
                      stabilizeM = Option.map (cvtMeth true) stabilizeM
                    }
                end
(*DEBUG*)handle ex => raise ex
          val create = Create.map (cvtGlobalBlock Other) create
          val prog = IR.Program{
                  props = props,
                  consts = consts',
                  inputs = inputs',
                  globals = globals',
                  funcs = funcs',
                  constInit = constInit,
                  globInit = globInit,
                  strand = cvtStrand strand,
                  create = create,
                  start = Option.map (cvtGlobalBlock Other) start,
                  update = Option.map (cvtGlobalBlock GlobalUpdate) update
                }
          in
            Census.init prog;
            prog
          end

  end
