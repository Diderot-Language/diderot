(* check-stmt.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure CheckStmt : sig

  (* type check a statement *)
    val check : Env.t * Env.context * ParseTree.stmt -> AST.stmt

  (* type check a variable declaration *)
    val checkVarDecl : Env.t * Env.context * Var.kind * ParseTree.var_dcl
          -> (Atom.atom * Var.t * AST.expr option)

  (* check the creation of a new strand; either in a "new" statement or in the
   * initial-strands creation code.
   *)
    val checkStrandCreate : Env.t * Env.context * Atom.atom * ParseTree.expr list -> AST.stmt

  end = struct

    structure PT = ParseTree
    structure L = Literal
    structure E = Env
    structure Ty = Types
    structure TU = TypeUtil
    structure BV = BasisVars

    val chkE = CheckExpr.check

  (* a statement to return when there is a type error *)
    fun bogusStm env = (AST.S_Block[], env)
  (* a bogus expression for when there is a type error *)
    val bogusExp = AST.E_Lit(L.Int 0)

    val err = TypeError.error
    val warn = TypeError.warning

    datatype token = datatype TypeError.token

  (* mark a variable use with its location *)
    fun useVar (cxt : Env.context, x) = (x, #2 cxt)

  (* typecheck a variable declaration *)
    fun checkVarDecl (env, cxt, kind, d) = (case d
           of PT.VD_Mark m => checkVarDecl (env, (#1 cxt, #span m), kind, #tree m)
            | PT.VD_Decl(ty, {span, tree=x}, optExp) => let
                val ty = CheckType.check (env, cxt, ty)
                val x' = Var.new (x, span, kind, ty)
                in
                  case optExp
                   of SOME e => let
                        val eTy = chkE (env, cxt, e)
                        in
                          case Util.coerceType (ty, eTy)
                           of SOME e' => (x, x', SOME e')
                            | NONE => (
                                err(cxt, [
                                    S "type of variable ", A x,
                                    S " does not match type of initializer\n",
                                    S "  expected: ", TY ty, S "\n",
                                    S "  found:    ", TY(#2 eTy)
                                  ]);
                                (x, x', SOME bogusExp))
                          (* end case *)
                        end
                  | NONE => (x, x', NONE)
                end
          (* end case *))

  (* check the creation of a new strand; either in a "new" statement or in an "start"
   * block.
   *)
    fun checkStrandCreate (env, cxt, strand, args) = let
          val argsAndTys' = List.map (fn e => CheckExpr.check(env, cxt, e)) args
          val (args', tys') = ListPair.unzip argsAndTys'
          in
          (* check that strand is defined and that the argument types match *)
            case Env.findStrand (env, strand)
             of SOME sEnv => let
                  val paramTys = StrandEnv.paramTys sEnv
                  in
                    case Unify.matchArgs (paramTys, args', tys')
                     of SOME args' => AST.S_New(StrandEnv.strandName sEnv, args')
                      | NONE => (
                          err (cxt, [
                              S "type error in new ", A strand, S "\n",
                              S "  expected: ", TYS paramTys, S "\n",
                              S "  found:    ", TYS tys'
                            ]);
                          AST.S_Block[])
                    (* end case *)
                  end
              | NONE => (err (cxt, [S "unknown strand ", A strand]); AST.S_Block[])
            (* end case *)
          end

  (* check for unreachable code and non-return statements in the tail position of a function.
   * Note that unreachable code is typechecked and included in the AST.  It is pruned away
   * by simplify.
   *)
    fun chkCtlFlow (cxt, scope, stm) = let
          fun inFun () = (case scope of E.FunctionScope _ => true | _ => false)
          fun funName () = let val E.FunctionScope(_, f) = scope in S(Atom.toString f) end
          fun inCreateOrMeth () = (case scope
                 of E.MethodScope(_, StrandUtil.Start) => true
                  | E.MethodScope(_, StrandUtil.Update) => true
                  | E.CreateScope => true
                  | _ => false
                (* end case *))
          fun inUpdate () = (case scope
                 of E.MethodScope(_, StrandUtil.Update) => true
                  | E.UpdateScope => true
                  | _ => false
                (* end case *))
          fun inStartOrUpdateMethod () = (case scope
                 of E.MethodScope _ => true | _ => false)
          val hasDieOrStabilize = ref false
        (* checks a statement for correct control flow; it returns false if control may
         * flow from the statement to the next in a sequence and true if control cannot
         * flow to the next statement.  The parameter flags have the following meaning:
         *
         *      hasSucc         -- true if the statement has a successor
         *      isJoin          -- true if the following statement joins multiple control
         *                         paths
         *      unreachable     -- true if the previous statement escapes; i.e., control
         *                         cannot reach this statment.
         *)
          fun chk ((errStrm, _), hasSucc, isJoin, unreachable, PT.S_Mark{span, tree}) =
                chk((errStrm, span), hasSucc, isJoin, unreachable, tree)
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_Block(stms as _::_)) = let
                fun chk' ([], escapes) = escapes
                  | chk' ([stm], escapes) =
                      chk(cxt, hasSucc, isJoin, escapes orelse unreachable, stm) orelse escapes
                  | chk' (stm::stms, escapes) = let
                      val escapes = chk(cxt, true, false, escapes orelse unreachable, stm) orelse escapes
                      in
                        chk'(stms, escapes)
                      end
                in
                  chk' (stms, false)
                end
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_IfThen(_, stm)) = (
                if inFun() andalso not hasSucc andalso not unreachable
                  then err(cxt, [
                        S "Missing return statement in tail position of function ", funName()
                    ])
                  else ();
                ignore (chk (cxt, hasSucc, true, unreachable, stm));
                false)
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_IfThenElse(_, stm1, stm2)) = let
                val escapes = chk (cxt, hasSucc, true, unreachable, stm1)
                val escapes = chk (cxt, hasSucc, true, unreachable, stm2) andalso escapes
                in
                  if escapes andalso hasSucc andalso not unreachable
                    then (
                      warn(cxt, [S "unreachable statements after \"if-then-else\" statement"]);
                      true)
                    else escapes
                end
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_Foreach(_, _, stm)) = (
                ignore (chk (cxt, hasSucc, true, unreachable, stm));
                false)
            | chk (cxt, _, _, _, PT.S_New _) = (
                case scope
                 of E.MethodScope(_, StrandUtil.Start) => ()
                  | E.MethodScope(_, StrandUtil.Update) => ()
                  | _ => err(cxt, [S "\"new\" statement outside of start/update method"])
                (* end case *);
                false)
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_Continue) = (
(* QUESTION: should we allow "continue" in loops? *)
                case scope
                 of E.MethodScope(_, StrandUtil.Update) => ()
                  | E.UpdateScope => ()
                  | _ => if hasSucc andalso not isJoin andalso not unreachable
                    then warn(cxt, [S "statements following \"continue\" statment are unreachable"])
                    else ()
                (* end case *);
                true)
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_Stabilize) = let
              (* check strand stabilize *)
                fun chkStabilize () = (
                      if (not unreachable) then hasDieOrStabilize := true else ();
                      if hasSucc andalso not isJoin andalso not unreachable
                        then warn(cxt, [
                            S "statements following \"stabilize\" statment are unreachable"
                          ])
                        else ();
                      true)
                in
                  case scope
                   of E.MethodScope(_, StrandUtil.Start) => chkStabilize ()
                    | E.MethodScope(_, StrandUtil.Update) => chkStabilize ()
                    | E.StartScope => false (* global stabilize_all; _not_ an exit *)
                    | E.UpdateScope => false (* global stabilize_all; _not_ an exit *)
                    | _ => (err(cxt, [
                          S "\"stabilize\" statment outside of start/update method/block"
                        ]);
                        false)
                  (* end case *)
                end
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_Die) =  let
              (* check strand die *)
                fun chkDie () = (
                      if (not unreachable) then hasDieOrStabilize := true else ();
                      if hasSucc andalso not isJoin andalso not unreachable
                        then warn(cxt, [
                            S "statements following \"die\" statment are unreachable"
                          ])
                        else ();
                      true)
                in
                  case scope
                   of E.MethodScope(_, StrandUtil.Start) => chkDie ()
                    | E.MethodScope(_, StrandUtil.Update) => chkDie ()
                    | E.StartScope => false (* global kill_all; _not_ an exit *)
                    | E.UpdateScope => false (* global kill_all; _not_ an exit *)
                    | _ => (err(cxt, [
                          S "\"die\" statment outside of start/update method/block"
                        ]);
                        false)
                  (* end case *)
                end
            | chk (cxt, hasSucc, isJoin, unreachable, PT.S_Return _) = (
                if not(inFun())
                  then err(cxt, [S "\"return\" statment outside of function body"])
                else if hasSucc andalso not isJoin andalso not unreachable
                  then warn(cxt, [S "statements following \"return\" statment are unreachable"])
                  else ();
                true)
            | chk (cxt, hasSucc, isJoin, unreachable, _) = (
                if inFun() andalso not hasSucc andalso not unreachable
                  then err(cxt, [
                        S "Missing return statement in tail position of function ", funName()
                    ])
                  else ();
                false)
          in
            ignore (chk (cxt, false, false, false, stm));
            case (!hasDieOrStabilize, scope)
             of (false, E.MethodScope(_, StrandUtil.Update)) =>
                  warn (cxt, [S "update method does not have reachable die or stabilize statement"])
              | _ => ()
            (* end case *)
          end

  (* check the type of a statement *)
    fun chk (env, cxt, e) = (case e
           of PT.S_Mark m => chk (E.withEnvAndContext (env, cxt, m))
            | PT.S_Block stms => let
                fun chk' (_, [], stms) = AST.S_Block(List.rev stms)
                  | chk' (env, s::ss, stms) = let
                      val (s', env') = chk (env, cxt, s)
                      in
                        chk' (env', ss, s'::stms)
                      end
                in
                  (chk' (Env.blockScope env, stms, []), env)
                end
            | PT.S_IfThen(e, s) => let
                val (e', ty) = chkE (env, cxt, e)
                val (s', _) = chk (env, cxt, s)
                in
                (* check that condition has bool type *)
                  case TU.prune ty
                   of Ty.T_Bool => ()
                    | Ty.T_Error => ()
                    | _ => err(cxt, [S "condition not boolean type"])
                  (* end case *);
                  (AST.S_IfThenElse(e', s', AST.S_Block[]), env)
                end
            | PT.S_IfThenElse(e, s1, s2) => let
                val (e', ty) = chkE (env, cxt, e)
                val (s1', _) = chk (env, cxt, s1)
                val (s2', _) = chk (env, cxt, s2)
                in
                (* check that condition has bool type *)
                  case TU.prune ty
                   of Ty.T_Bool => ()
                    | Ty.T_Error => ()
                    | _ => err (cxt, [S "expected type 'bool' for condition, but found ", TY ty])
                  (* end case *);
                  (AST.S_IfThenElse(e', s1', s2'), env)
                end
            | PT.S_Foreach(ty, iter, body) => let
                val ty = CheckType.check (env, cxt, ty)
                val ((x', e'), env') = CheckExpr.checkIter (E.loopScope env, cxt, iter)
                in
                  if Unify.equalType(ty, Var.monoTypeOf x')
                    then ()
                    else err (cxt, [
                        S "type mismatch in iterator\n",
                        S "  declared element type: ", TY ty, S "\n",
                        S "  actual element type:   ", TY(Var.monoTypeOf x')
                      ]);
                  (AST.S_Foreach((x', e'), #1 (chk (env', cxt, body))), env)
                end
            | PT.S_Print args => let
                fun chkArg e = let
                      val (e', ty) = chkE (env, cxt, e)
                      in
                        if TU.isValueType ty
                          then ()
                          else err (cxt, [
                              S "expected value type in print, but found ", TY ty
                           ]);
                        e'
                      end
                val args' = List.map chkArg args
                in
                  (AST.S_Print args', env)
                end
            | PT.S_New(strand, args) => let
              (* note that scope has already been checked in chkCtlFlow *)
                val stm = checkStrandCreate (env, cxt, strand, args)
                in
                  Env.recordProp (env, Properties.NewStrands);
                  (stm, env)
                end
            | PT.S_Stabilize => (* Note: scope validity has already been checked in chkCtlFlow *)
                if Env.inGlobalBlock env
                  then (
                    Env.recordProp (env, Properties.StabilizeAll);
                    (AST.S_StabilizeAll, env))
                  else (AST.S_Stabilize, env)
            | PT.S_Die => ( (* note that scope has already been checked in chkCtlFlow *)
                Env.recordProp (env, Properties.StrandsMayDie);
                if Env.inGlobalBlock env
                  then (
                    Env.recordProp (env, Properties.KillAll);
                    (AST.S_KillAll, env))
                  else (AST.S_Die, env))
            | PT.S_Continue => (* note that scope has already been checked in chkCtlFlow *)
                (AST.S_Continue, env)
            | PT.S_Return e => let
                val eTy = chkE (env, cxt, e)
                in
                  case E.currentScope env
                   of E.FunctionScope(ty', f) => (case Util.coerceType(ty', eTy)
                         of SOME e' => (AST.S_Return e', env)
                          | NONE => (
                              err (cxt, [
                                  S "type of return expression does not match return type of function ",
                                  A f, S "\n",
                                  S "  expected: ", TY ty', S "\n",
                                  S "  found:    ", TY(#2 eTy)
                                ]);
                              bogusStm env)
                        (* end case *))
                    | _ => (AST.S_Return(#1 eTy), env) (* this error condition has already been reported *)
                  (* end case *)
                end
            | PT.S_Decl vd => let
                val (x, x', e) = checkVarDecl (env, cxt, Var.LocalVar, vd)
                in
                  E.checkForRedef (env, cxt, x);
                  (AST.S_Decl(x', e), E.insertLocal(env, cxt, x, x'))
                end
            | PT.S_Assign({span, tree=x}, rator, e) => (case Env.findVar (env, x)
                 of NONE => (
                      err (cxt, [S "undefined variable ", A x, S " on lhs of assignment"]);
                      bogusStm env)
                  | SOME x' => let
                      val ([], ty) = Var.typeOf x'
                      val eTy = chkE (env, cxt, e)
                      fun illegalAssign kind = (
                            err (cxt, [
                                S "illegal assignment to ", S kind, S " ", A x,
                                S " in ", S(E.scopeToString(E.currentScope env))
                              ]);
                            bogusStm env)
                    (* check for assignment to variables that are immutable because of their type *)
                      fun chkAssign () = (case Var.monoTypeOf x'
                             of (Ty.T_Field _) => illegalAssign "field-valued variable"
                              | (Ty.T_Image _) => illegalAssign "image-valued variable"
                              | (Ty.T_Kernel _) => illegalAssign "kernel-valued variable"
                              | ty => (case rator
                                   of NONE => let
                                      (* check for promotion *)
                                        val (e', ty') = (case Util.coerceType(ty, eTy)
                                               of SOME e' => (e', ty)
                                                | NONE => (
                                                    err(cxt, [
                                                        S "type of assigned variable ", A x,
                                                        S " does not match type of rhs\n",
                                                        S "  expected: ", TY ty, S "\n",
                                                        S "  found:    ", TY(#2 eTy)
                                                      ]);
                                                    eTy)
                                              (* end case *))
                                        in
                                          (AST.S_Assign(useVar((#1 cxt, span), x'), e'), env)
                                        end
                                    | SOME rator => let
                                        val x' = useVar((#1 cxt, span), x')
                                        val e1' = AST.E_Var x'
                                        val (e2', ty2) = eTy
                                        val Env.PrimFun ovldList = Env.findFunc (env, rator)
(* NOTE: is there a potential problem with something like: i += r (where i is int and r is real)?
 * It is okay to promote the rhs type, but not the lhs!
 *)
                                        val (rhs, _) = CheckExpr.resolveOverload (
                                              cxt, rator, [ty, ty2], [e1', e2'], ovldList)
                                        in
                                          (AST.S_Assign(x', rhs), env)
                                        end
                                  (* end case *))
                            (* end case *))
                    (* check that assignment to global variables is allowed in the current scope *)
                      fun chkGlobalAssign () = (case E.currentScope env
                             of E.FunctionScope _ => illegalAssign "global variable"
                              | E.MethodScope _ => illegalAssign "global variable"
                              | E.InitScope => chkAssign()
                              | E.StartScope => chkAssign()
                              | E.UpdateScope => chkAssign()
                              | _ => raise Fail "impossible scope"
                            (* end case *))
                      in
                      (* check that assigning to x' is okay *)
                        case Var.kindOf x'
                         of Var.BasisVar => illegalAssign "builtin function"
                          | Var.ConstVar => illegalAssign "constant variable"
                          | Var.InputVar => chkGlobalAssign ()
                          | Var.GlobalVar => chkGlobalAssign ()
                          | Var.FunVar => illegalAssign "function"
                          | Var.FunParam => illegalAssign "function parameter"
                          | Var.StrandParam => illegalAssign "strand parameter"
                          | Var.IterVar => illegalAssign "iteration variable"
                          | _ => chkAssign ()
                        (* end case *)
                      end
                (* end case *))
          (* end case *))

    fun check (env, cxt, stm) = (
          chkCtlFlow (cxt, E.currentScope env, stm);
          #1 (chk (env, cxt, stm)))

  end
