(* check-var-uses.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * This pass checks two properties of variables:
 *
 *    1) variables that might be used before being assigned a value (ERROR)
 *    2) variables that defined, but not used (WARNING)
 *
 * FIXME: check for globals and state variables that are not initialized in
 * their respective initialization sections
 *)

structure CheckVarUses : sig

  (* check for uses of undefined variables and unused variables in the program *)
    val check : Env.context * AST.program -> unit

  end = struct

    structure VSet = Var.Set

    datatype token = datatype TypeError.token

    fun error ((errStrm, _), span, msg) = Error.errorAt (errStrm, span, TypeError.format msg)

    fun warning ((errStrm, _), span, msg) = Error.warningAt (errStrm, span, TypeError.format msg)

    fun uninitError (cxt, x) = error (cxt, Var.locationOf x, [
            S(Var.kindToString x), S " ", V x, S " is not initialized"
          ])

(* FIXME: if a variable is assigned to, but never referenced, we should say that in
 * the warning message.
 *)
    fun unusedWarning (cxt, x) = warning (cxt, Var.locationOf x, [
            S(Var.kindToString x), S " ", V x, S " is unused"
          ])

  (* Check the use of a variable and report an error if it has not been initialized.
   * We also remove it from the unused set and return the new unused set.
   *)
    fun chkUse (cxt : Env.context, (x, span), undef, unused) = (case Var.kindOf x
           of Var.BasisVar => (
                if Basis.isStrandSet x
                  then TypeError.error ((#1 cxt, span), [
                      S "use of strand set ", V x, S " outside of comprehension"
                    ])
                  else ();
                unused)
            | Var.ConstVar => VSet.subtract(unused, x)
            | Var.FunVar => VSet.subtract(unused, x)
            | _ => (
              (* check for use of an undefined variable; the test against unused is
               * to reduce repeated error messages.
               *)
                if VSet.member(undef, x) andalso VSet.member(unused, x)
                  then TypeError.error ((#1 cxt, span), [
                      S "possible use of variable ", V x, S " before it has been initialized"
                    ])
                  else ();
                VSet.subtract(unused, x))
          (* end case *))

  (* check for unused variable and, if unused, remove it from the unused set *)
    fun chkForUnused cxt (x, unused) = if VSet.member(unused, x)
          then (
            unusedWarning(cxt, x);
            VSet.subtract(unused, x))
          else unused

  (* check an expression for uses of undefined variables and remove used variables
   * from the unused list
   *)
    fun chkExpr (cxt, e, undef, unused) = let
          fun chk (e, unused) = (case e
                 of AST.E_Var x => chkUse(cxt, x, undef, unused)
                  | AST.E_Lit _ => unused
                  | AST.E_Kernel _ => unused
                  | AST.E_Select(e, x) => chkUse(cxt, x, chk (e, undef), unused)
                  | AST.E_Prim(f, _, [], _) => (
                      if Basis.isStrandSet f
                        then TypeError.error (cxt, [
                            S "use of strand set ", V f, S " outside of comprehension"
                          ])
                        else ();
                      unused)
                  | AST.E_Prim(_, _, args, _) => chk' (args, unused)
                  | AST.E_Apply(f, args, _) => chk' (args, chkUse (cxt, f, undef, unused))
                  | AST.E_Comprehension(e, (x, e'), _) => let
                      val unused = chk (e, chk (e', unused))
                      in
                        chkForUnused cxt (x, unused)
                      end
                  | AST.E_ParallelMap(e, x, xs, _) => chkForUnused cxt (x, chk (e, unused))
                  | AST.E_Tensor(args, _) => chk' (args, unused)
                  | AST.E_Field(args, _) => chk' (args, unused)
                  | AST.E_Seq(args, _) => chk' (args, unused)
                  | AST.E_Slice(e, indices, _) =>
                      List.foldl (fn (SOME e, unu) => chk(e, unu) | (NONE, unu) => unu)
                        (chk (e, unused)) indices
                  | AST.E_Cond(e1, e2, e3, _) => chk (e3, chk (e2, chk (e1, unused)))
                  | AST.E_Orelse(e1, e2) => chk (e2, chk (e1, unused))
                  | AST.E_Andalso(e1, e2) => chk (e2, chk (e1, unused))
                  | AST.E_LoadNrrd _ => unused
                  | AST.E_Coerce{e, ...} => chk (e, unused)
                (* end case *))
          and chk' (es, unused) = List.foldl chk unused es
          in
            chk (e, unused)
          end

    fun chkStmt (cxt, s, undef, unused) = let
          fun chkScope ([], undef, unused, bound) = let
              (* check for unused variables bound in this scope *)
                val unused = VSet.foldl (chkForUnused cxt) unused bound
                in
                  (VSet.difference(undef, bound), unused)
                end
            | chkScope (stm::stms, undef, unused, bound) = let
                val (undef, unused, bound) = chk (stm, undef, unused, bound)
                in
                  chkScope (stms, undef, unused, bound)
                end
          and chkBlockOrStm (AST.S_Block stms, undef, unused, bound) =
                chkScope (stms, undef, unused, bound)
            | chkBlockOrStm (stm, undef, unused, bound) = chkScope ([stm], undef, unused, bound)
          and chk (stm, undef, unused, bound) = (case stm
                 of AST.S_Block stms => let
                      val (undef, unused) = chkScope (stms, undef, unused, VSet.empty)
                      in
                        (undef, unused, bound)
                      end
                  | AST.S_Decl(x, NONE) =>
                      (VSet.add(undef, x), VSet.add(unused, x), VSet.add(bound, x))
                  | AST.S_Decl(x, SOME e) => let
                      val unused = chkExpr (cxt, e, undef, unused)
                      in
                        (undef, VSet.add(unused, x), VSet.add(bound, x))
                      end
                  | AST.S_IfThenElse(e, stm1, stm2) => let
                      val unused = chkExpr (cxt, e, undef, unused)
                      val (undef1, unused1) = chkBlockOrStm (stm1, undef, unused, VSet.empty)
                      val (undef2, unused2) = chkBlockOrStm (stm2, undef, unused, VSet.empty)
                      in
                      (* variables have to be defined on both paths to be removed from undef,
                       * but can be used on either path to be removed from unused.
                       *)
                        (VSet.union(undef1, undef2), VSet.intersection(unused1, unused2), bound)
                      end
                  | AST.S_Foreach((x, e), stm) => let
                      val unused = chkExpr (cxt, e, undef, unused)
                    (* note that we do not assume that the loop is guaranteed to execute,
                     * so we ignore any changes to the undef set.
                     *)
                      val (_, unused) = chkBlockOrStm (stm, undef, unused, VSet.singleton x)
                      in
                        (undef, unused, bound)
                      end
                  | AST.S_Assign(x, e) =>
                      (VSet.subtract(undef, #1 x), chkExpr (cxt, e, undef, unused), bound)
                  | AST.S_New(_, args) => (undef, chkExps (args, undef, unused), bound)
                  | AST.S_KillAll => (undef, unused, bound)
                  | AST.S_StabilizeAll => (undef, unused, bound)
                  | AST.S_Continue => (undef, unused, bound)
                  | AST.S_Die => (undef, unused, bound)
                  | AST.S_Stabilize => (undef, unused, bound)
                  | AST.S_Return e => (undef, chkExpr (cxt, e, undef, unused), bound)
                  | AST.S_Print args => (undef, chkExps (args, undef, unused), bound)
                (* end case *))
          and chkExps (exps, undef, unused) = let
                fun chk (exp, unused) = chkExpr (cxt, exp, undef, unused)
                in
                  List.foldl chk unused exps
                end
          in
            chkBlockOrStm (s, undef, unused, VSet.empty)
          end

    fun chkOptBlock (_, NONE, undef, unused) = (undef, unused)
      | chkOptBlock (cxt, SOME stm, undef, unused) = chkStmt (cxt,stm, undef, unused)

    fun chkStrand (cxt, strand, undef, unused) = let
          val AST.Strand{
                  params, spatialDim, state, stateInit, startM, updateM, stabilizeM, ...
                } = strand
        (* check a state-variable declaration *)
          fun chkVarDecl ((x, optE), (undef, unused, bound, outputs)) = let
                val (undef, unused) = (case optE
                       of NONE => (VSet.add(undef, x), unused)
                        | SOME e => (undef, chkExpr (cxt, e, undef, unused))
                      (* end case *))
                val (unused, outputs) = if Var.isOutput x
                      then (unused, x::outputs)
                      else (VSet.add(unused, x), outputs)
                in
                  (undef, unused, x::bound, outputs)
                end
        (* first we add the parameters to the sets of bound and unused variables *)
          val bound = params
          val unused = VSet.addList (unused, params)
        (* process the state variables *)
(* FIXME: if spatialDim is not NONE, then "pos" is implicitly used! *)
          val (undef, unused, bound, outputs) =
                List.foldl chkVarDecl (undef, unused, bound, []) state
        (* check the state initialization block (if present) *)
          val (undef, unused) = chkOptBlock (cxt, stateInit, undef, unused)
        (* at this point we capture the set of uninitialized state variables *)
          val uninit = let
                fun chk ((_, SOME _), s) = s
                  | chk ((x, NONE), s) = if VSet.member(undef, x) then VSet.add(s, x) else s
                in
                  List.foldl chk VSet.empty state
                end
        (* check the start method first (if it exists) *)
          val (undef, unused) = chkOptBlock (cxt, startM, undef, unused)
        (* then the update method *)
          val (undef', unused) = chkStmt (cxt, updateM, undef, unused)
        (* check the stabilize method; we ignore definitions from the update method, since
         * we might get called from the start method.
         *)
(* FIXME: we should check if the start method has a call to stabilize! *)
          val (undef'', unused) = chkOptBlock (cxt, stabilizeM, undef, unused)
        (* merge the undef sets from the update and stabilize methods *)
          val undef = VSet.intersection (undef', undef'')
        (* state variables that are defined, but that are not initialized during strand creation *)
          val uninit = VSet.difference(uninit, undef)
          in
        (* check for undefined output variables *)
            List.app
              (fn x => if VSet.member(undef, x) then uninitError (cxt, x) else ())
                outputs;
(* QUESTION: we could change this to a warning, but we would need to correctly handle uninitialized
 * variables during translation to High IR.
 *)
          (* report uninitialized state variables that are later defined *)
            List.app
              (fn x => if VSet.member(uninit, x)
                  then error (cxt, Var.locationOf x, [
                      S(Var.kindToString x), S " ", V x, S " is not initialized"
                    ])
                  else ()
                ) outputs;
          (* report unused state variables *)
(* FIXME: if a state variable is used in a global reduction, then it shouldn't be reported! *)
            List.foldl (chkForUnused cxt) unused bound
          end

    fun check (cxt, prog) = let
          val AST.Program{
                  const_dcls, input_dcls, globals, globInit, strand, create, start, update, ...
                } = prog
          fun chkVarDecl ((x, NONE), (undef, unused, bound)) =
                (VSet.add(undef, x), VSet.add(unused, x), VSet.add(bound, x))
            | chkVarDecl ((x, SOME e), (undef, unused, bound)) =
                (undef, VSet.add(chkExpr(cxt, e, undef, unused), x), VSet.add(bound, x))
        (* check input-variable declarations; these get their values at runtime *)
          fun chkInputDecl (((x, NONE), _), (undef, unused, bound)) =
                (undef, VSet.add(unused, x), VSet.add(bound, x))
            | chkInputDecl (((x, SOME e), _), (undef, unused, bound)) =
                (undef, VSet.add(chkExpr(cxt, e, undef, unused), x), VSet.add(bound, x))
          fun chkGlobalDecl (AST.D_Var vd, accum) = chkVarDecl (vd, accum)
            | chkGlobalDecl (AST.D_Func(f, params, body), (undef, unused, bound)) = let
                val (undef, unused) = chkStmt (cxt, body, undef, VSet.addList(unused, params))
                val unused = List.foldl (chkForUnused cxt) unused params
                in
                  (undef, VSet.add(unused, f), bound)
                end
            | chkGlobalDecl (AST.D_DiffFunc(f, params, body), (undef, unused, bound)) = let
                val unused = chkExpr(cxt, body, undef, VSet.addList(unused, params))
                in
                  (undef, VSet.add(unused, f), bound)
                end
        (* first process the global declarations *)
(* FIXME: constant variables that are used solely to define other constants are reported
 * as unused.
 *)
          val (undef, unused, bound) =
                List.foldl chkVarDecl (VSet.empty, VSet.empty, VSet.empty) const_dcls
          val (undef, unused, bound) =
                List.foldl chkInputDecl (undef, unused, bound) input_dcls
          val (undef, unused, bound) =
                List.foldl chkGlobalDecl (undef, unused, bound) globals
        (* the optional global initialization block *)
          val (undef, unused) = chkOptBlock (cxt, globInit, undef, unused)
        (* capture the set of uninizialized globals (not including inputs) *)
          val uninit = let
                fun chk (AST.D_Var(x, NONE), s) =
                      if VSet.member(undef, x) then VSet.add(s, x) else s
                  | chk (_, s) = s
                in
                  List.foldl chk VSet.empty globals
                end
        (* the strand (no global initialization allowed) *)
          val unused = chkStrand (cxt, strand, undef, unused)
        (* check the initial-strands creation code *)
          val unused = #2 (chkStmt (cxt, Create.createCode create, undef, unused))
        (* the optional global start block *)
          val (undef, unused) = chkOptBlock (cxt, start, undef, unused)
        (* the optional global update block *)
          val (undef, unused) = chkOptBlock (cxt, update, undef, unused)
          in
          (* report uninitialized state variables *)
            VSet.app (fn x => uninitError (cxt, x)) uninit;
          (* report unused variables *)
            VSet.app
              (fn x => if VSet.member(unused, x) then unusedWarning(cxt, x) else ())
                bound
          end

  end
