(* analyze-simple.sml
 *
 * Analysis for Simple AST blocks.  We compute the set of free local variables that
 * are assigned in the block and the live variables.  These pieces of information
 * are stored as properties in the block and are used to generate the phi nodes in
 * the CFG representation.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure AnalyzeSimple : sig

    val analyze : Simple.program -> unit

    val assignedVars : Simple.block -> SimpleVar.Set.set
    val liveIn  : Simple.block -> SimpleVar.Set.set
    val liveOut : Simple.block -> SimpleVar.Set.set

  (* a property set on global variables that are modified during the global-update code *)
    val updatedGlobal : SimpleVar.t -> bool

  (* a property set on state variables that are modified during super steps *)
    val varyingStateVar : SimpleVar.t -> bool

  (* a property set on state variables that are read by other strands *)
    val sharedStateVar : SimpleVar.t -> bool

  (* a property set on top-level blocks that tracks the globals that are read in the block *)
    val globalsOfBlock : Simple.block -> SimpleVar.Set.set

  end = struct

    structure S = Simple
    structure SV = SimpleVar
    structure VMap = SV.Map
    structure VSet = SV.Set

    datatype context
      = ConstInit | UserFunc | GlobalInit | StateInit
      | StartMeth | UpdateMeth | StabilizeMeth
      | Create | GlobalUpdate

  (* for debugging *)
    fun cxtToString cxt = (case cxt
           of ConstInit => "ConstInit"
            | UserFunc => "UserFunc"
            | GlobalInit => "GlobalInit"
            | StateInit => "StateInit"
            | StartMeth => "StartMeth"
            | UpdateMeth => "UpdateMeth"
            | StabilizeMeth => "StabilizeMeth"
            | Create => "Create"
            | GlobalUpdate => "GlobalUpdate"
          (* end case *))

    local
      val {getFn, setFn, ...} = S.newProp (fn _ => VSet.empty)
    in
    val assignedVars = getFn
    val setAssigned = setFn
    end

    local
      val {getFn, setFn, ...} = S.newProp (fn _ => VSet.empty)
    in
    val liveIn = getFn
    val setLiveIn = setFn
    end

    local
      val {getFn, setFn, ...} = S.newProp (fn _ => VSet.empty)
    in
    val liveOut = getFn
    val setLiveOut = setFn
    end

  (* track if a global variable is updated in the global-update code *)
    local
      val {setFn, getFn} = SV.newFlag()
    in
    val updatedGlobal = getFn
    fun markUpdatedGlobal x = setFn(x, true)
    end (* local *)

  (* track if a state variable is modified during super steps *)
    local
      val {setFn, getFn} = SV.newFlag()
    in
    val varyingStateVar = getFn
    fun markVaryingStateVar x = setFn(x, true)
    end (* local *)

  (* track if a state variable is read by other strands *)
    local
      val {setFn, getFn} = SV.newFlag()
    in
    val sharedStateVar = getFn
    fun markSharedStateVar x = (case SV.kindOf x
           of SV.StrandStateVar => setFn(x, true)
            | SV.StrandOutputVar => setFn(x, true)
            | _ => ()
          (* end case *))
    end (* local *)

    local
      val {getFn, setFn, ...} =
            PropList.newProp (fn (S.Block{props, ...}) => props, fn _ => VSet.empty)
    in
    val globalsOfBlock = getFn
    val setGlobalsOfBlock = setFn
    end (* local *)

    fun markUpdate (cxt, x) = (case (cxt, SV.kindOf x)
           of (ConstInit, SV.ConstVar) => ()
            | (_, SV.ConstVar) => raise Fail "update of ConstVar"
(* QUESTION: is it the case that state variables that are updated during
 * the start method but not later can be treated as invariant?
 *)
            | (StartMeth, SV.StrandStateVar) => markVaryingStateVar x
            | (StartMeth, SV.StrandOutputVar) => markVaryingStateVar x
            | (UpdateMeth, SV.StrandStateVar) => markVaryingStateVar x
            | (UpdateMeth, SV.StrandOutputVar) => markVaryingStateVar x
            | (StabilizeMeth, SV.StrandStateVar) => markVaryingStateVar x
            | (StabilizeMeth, SV.StrandOutputVar) => markVaryingStateVar x
            | (GlobalUpdate, SV.InputVar) => markUpdatedGlobal x
            | (GlobalUpdate, SV.GlobalVar) => markUpdatedGlobal x
            | _ => ()
          (* end case *))

    fun analyzeBlock cxt blk = let
          val globals = ref VSet.empty
          fun addGlobal x = (globals := VSet.add(!globals, x))
          fun addVar (x, vs) = let
                fun addGlobal x = (globals := VSet.add(!globals, x); vs)
                in
                  case (SV.kindOf x, cxt)
                   of (SV.ConstVar, ConstInit) => vs
                    | (SV.ConstVar, _) => addGlobal x
                    | (SV.InputVar, ConstInit) => vs
                    | (SV.InputVar, _) => addGlobal x
                    | (SV.GlobalVar, ConstInit) => vs
                    | (SV.GlobalVar, GlobalInit) => vs
                    | (SV.GlobalVar, _) => addGlobal x
                    | (SV.StrandStateVar, _) => VSet.add(vs, x)
                    | (SV.StrandOutputVar, _) => VSet.add(vs, x)
                    | (SV.LocalVar, _) => VSet.add(vs, x)
                    | (SV.IterVar, _) => VSet.add(vs, x)
                    | _ => vs
                  (* end case *)
                end
          fun addList (xs, vs) = List.foldl addVar vs xs
        (* compute the used local and global variables of a Simple AST expression *)
          fun addUses (e, vs) = let
                val u = (case e
                       of S.E_Var x => [x]
                        | S.E_Lit _ => []
                        | S.E_Kernel _ => []
                        | S.E_Select(y, z) => (
                            markSharedStateVar z;
                            [y])
                        | S.E_Apply(_, xs) => xs
                        | S.E_Prim(_, _, xs, _) => xs
                        | S.E_Tensor(xs, _) => xs
                        | S.E_Field(xs, _) => xs
                        | S.E_Seq(xs, _) => xs
                        | S.E_Tuple xs => xs
                        | S.E_Project(x, i) => [x]
                        | S.E_Slice(x, _, _) => [x]
                        | S.E_Coerce{x, ...} => [x]
                        | S.E_BorderCtl(BorderCtl.Default x, y) => [x, y]
                        | S.E_BorderCtl(_, x) => [x]
                        | S.E_LoadSeq _ => []
                        | S.E_LoadImage _ => []
                        | S.E_InsideImage(pos, img, _) => [pos, img]
                        | S.E_FieldFn f => []
                      (* end case *))
                in
                  addList (u, vs)
                end
          fun doBlock (blk as S.Block{code, ...}, liveIn) = let
                val result as (liveOut, assigns) = List.foldr doStm (liveIn, VSet.empty) code
                in
                  setAssigned (blk, assigns);
                  setLiveIn (blk, liveIn);
                  setLiveOut (blk, liveOut);
                  result
                end
          and doStm (S.S_Var(x, NONE), (live, assigns)) =
                (VSet.subtract(live, x), VSet.subtract(assigns, x))
            | doStm (S.S_Var(x, SOME e), (live, assigns)) =
                (addUses(e, VSet.subtract(live, x)), VSet.subtract(assigns, x))
            | doStm (S.S_Assign(x, e), (live, assigns)) = (
                markUpdate (cxt, x);
                (addUses(e, VSet.subtract(live, x)), addVar(x, assigns)))
            | doStm (S.S_IfThenElse(x, b1, b2), (liveIn, assignsIn)) = let
                val liveIn = addVar(x, liveIn)
                val (live1, assigns1) = doBlock (b1, liveIn)
                val (live2, assigns2) = doBlock (b2, liveIn)
                val assigns = VSet.union(assignsIn, VSet.union(assigns1, assigns2))
                in
                  (VSet.union(live1, live2), assigns)
                end
            | doStm (S.S_Foreach(x, xs, b), (liveIn, assignsIn)) = let
                val liveIn = addVar(xs, liveIn)
                val (liveOut, assigns) = doBlock (b, liveIn)
                val liveOut = VSet.union(VSet.subtract(liveOut, x), liveIn)
                val assigns = VSet.union(VSet.subtract(assigns, x), assignsIn)
                in
(* QUESTION: do we want to modify the properties of b? *)
                  (liveOut, assigns)
                end
            | doStm (S.S_New(_, xs), (live, assigns)) = (addList (xs, live), assigns)
            | doStm (S.S_Return x, (live, assigns)) = (addVar (x, live), assigns)
            | doStm (S.S_Print xs, (live, assigns)) = (addList (xs, live), assigns)
            | doStm (S.S_MapReduce mrs, acc) = let
                fun doMR (S.MapReduce{result, args, source, ...}, (live, assigns)) = (
                      markUpdate (cxt, result);
(* QUESTION: do we need to remove result from live? *)
                      (addList(args, live), assigns))
                in
                  List.foldl doMR acc mrs
                end
            | doStm (_, acc) = acc
          val (bnd, assigns) = doBlock (blk, VSet.empty)
          in
            setGlobalsOfBlock (blk, !globals)
          end

    fun analyze prog = let
          val S.Program{
                  props, constInit, funcs, globInit, strand, create, start, update, ...
                } = prog
          val S.Strand{state, stateInit, startM, updateM, stabilizeM, ...} = strand
          in
          (* if the program has communication then the "pos" variable is shared *)
            if Properties.hasProp Properties.StrandCommunication props
              then (case List.find (fn x => (SV.nameOf x = "pos")) state
                 of SOME x => markSharedStateVar x
                  | NONE => let
                      fun pr s = TextIO.output(TextIO.stdErr, concat s)
                      in
                        pr ["**** impossible: missing 'pos' state variable\n"];
                        pr ["  ** state variables:\n"];
                        List.app
                          (fn x => pr[
                              "     nameOf(", SV.uniqueNameOf x, ") = \"",
                              SV.nameOf x, "\"\n"
                            ])
                          state;
                        raise Fail "impossible: missing 'pos' state variable"
                      end
                (* end case *))
              else ();
            analyzeBlock ConstInit constInit;
            List.app (fn (S.Func{f, body, ...}) => analyzeBlock UserFunc body) funcs;
            analyzeBlock GlobalInit globInit;
            analyzeBlock StateInit stateInit;
            Option.app (analyzeBlock StartMeth) startM;
            analyzeBlock UpdateMeth updateM;
            Option.app (analyzeBlock StabilizeMeth) stabilizeM;
            Option.app (analyzeBlock GlobalUpdate) start;
            Option.app (analyzeBlock GlobalUpdate) update;
            Create.app (analyzeBlock Create) create
          end

  end
