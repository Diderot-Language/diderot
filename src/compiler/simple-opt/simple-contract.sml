(* simple-contract.sml
 *
 * This is a limited contraction phase for the SimpleAST representation.  The purpose is
 * to eliminate unused variables and dead code.  Specifically, the following
 * transformations are performed:
 *
 *   -- unused constant and global variables are elminated
 *
 *   -- unused strand state variables are eliminated (but not outputs)
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure SimpleContract : sig

    val transform : Simple.program -> Simple.program

  end = struct

    structure S = Simple
    structure SV = SimpleVar
    structure ST = Stats
    structure P = Properties

  (********** Counters for statistics **********)
    val cntUnusedConst          = ST.newCounter "simple-contract:unused-constant"
    val cntUnusedGlobal         = ST.newCounter "simple-contract:unused-global-var"
    val cntUnusedState          = ST.newCounter "simple-contract:unused-state-var"
    val cntUnusedLocal          = ST.newCounter "simple-contract:unused-local-var"
    val cntDeadAssign           = ST.newCounter "simple-contract:dead-assign"
    val cntDeadIf               = ST.newCounter "simple-contract:dead-if"
    val cntDeadForeach          = ST.newCounter "simple-contract:dead-foreach"
    val cntStaticIf             = ST.newCounter "simple-contract:static-if"
    val firstCounter            = cntUnusedConst
    val lastCounter             = cntStaticIf

    fun sumChanges () = ST.sum {from=firstCounter, to=lastCounter}

  (* an approximation of the state of boolean variables *)
    datatype abs_bool = UndefBool | KnownBool of bool | UnknownBool
    local
      val {setFn, getFn, ...} = SV.newProp (fn _ => ref UndefBool)
    in
      fun setBoolVar (x, b) = (case getFn x
             of (r as ref UndefBool) => r := KnownBool b
              | (r as ref(KnownBool b')) => if (b = b') then () else r := UnknownBool
              | _ => ()
            (* end case *))
      fun getBoolVar x = (case !(getFn x)
             of KnownBool b => SOME b
              | UnknownBool => NONE
              | _ => raise Fail("read of undefined variable " ^ SimpleVar.uniqueNameOf x)
            (* end case *))
  (* if a variable has boolean type, set its value to UnknownBool *)
    fun setBoolUnknown x = (case SV.typeOf x
           of SimpleTypes.T_Bool => (getFn x := UnknownBool)
            | _ => ()
          (* end case *))
    end

  (* for constant, global, strand-state variables, and local variables we count uses *)
    local
      val {clrFn, getFn, peekFn, ...} = SV.newProp (fn _ => ref 0)
    in
      fun use x = let val r = getFn x in r := !r + 1 end
      fun unuse x = let val r = getFn x in r := !r - 1 end
      fun markUsed x = (case SV.kindOf x
             of SV.ConstVar => use x
              | SV.GlobalVar => use x
              | SV.StrandStateVar => use x
              | SV.StrandOutputVar => use x
              | SV.LocalVar => use x
              | _ => ()
            (* end case *))
      fun useCnt x = (case peekFn x of SOME(ref n) => n | _ => 0)
      fun isUsed x = (case SV.kindOf x
             of SV.InputVar => true (* inputs are always in use *)
              | SV.StrandOutputVar => true (* outputs are always in use *)
              | _ => (case peekFn x of SOME(ref n) => (n > 0) | _ => false)
            (* end case *))
      fun clrUsedMark x = clrFn x
    end (* local *)

(* FIXME: analyze functions for purity *)
  (* does an expression have an effect? *)
    fun hasEffect (S.E_Apply _) = true
      | hasEffect _ = false

  (* analyze a block for booleans and for unused variables *)
    fun analyzeBlock blk = let
          fun analyzeBlk (S.Block{code, ...}) = List.app analyzeStm code
          and analyzeStm stm = (case stm
                 of S.S_Var(x, NONE) => ()
                  | S.S_Var(x, SOME(S.E_Lit(Literal.Bool b))) => setBoolVar(x, b)
                  | S.S_Var(x, SOME e) => (setBoolUnknown x; analyzeExp e)
                  | S.S_Assign(x, S.E_Lit(Literal.Bool b)) => setBoolVar(x, b)
                  | S.S_Assign(x, e) => (setBoolUnknown x; analyzeExp e)
                  | S.S_IfThenElse(x, b1, b2) => (
                      markUsed x;
                      analyzeBlk b1; analyzeBlk b2)
                  | S.S_Foreach(x, xs, blk) => (markUsed xs; analyzeBlk blk)
                  | S.S_New(strnd, xs) => List.app markUsed xs
                  | S.S_KillAll => ()
                  | S.S_StabilizeAll => ()
                  | S.S_Continue => ()
                  | S.S_Die => ()
                  | S.S_Stabilize => ()
                  | S.S_Return x => markUsed x
                  | S.S_Print xs => List.app markUsed xs
                  | S.S_MapReduce mrs => let
                      fun doMR (S.MapReduce{args, mapf, ...}) = (
                            analyzeFunc mapf;
                            List.app markUsed args)
                      in
                        List.app doMR mrs
                      end
                (* end case *))
          and analyzeExp exp = (case exp
                 of S.E_Var x => markUsed x
                  | S.E_Lit _ => ()
                  | S.E_Kernel _ => ()
                  | S.E_Select(x, fld) => (markUsed x; markUsed fld)
                  | S.E_Apply(f, xs) => List.app markUsed xs
                  | S.E_Prim(_, _, xs, _) => List.app markUsed xs
                  | S.E_Tensor(xs, _) => List.app markUsed xs
                  | S.E_Field(xs, _) => List.app markUsed xs
                  | S.E_Seq(xs, _) => List.app markUsed xs
                  | S.E_Tuple xs => List.app markUsed xs
                  | S.E_Project(x, _) => markUsed x
                  | S.E_Slice(x, _, _) => markUsed x
                  | S.E_Coerce{x, ...} => markUsed x
                  | S.E_BorderCtl(BorderCtl.Default x, y) => (markUsed x; markUsed y)
                  | S.E_BorderCtl(_, x) => markUsed x
                  | S.E_LoadSeq _ => ()
                  | S.E_LoadImage _ => ()
                  | S.E_InsideImage(pos, img, _) => (markUsed pos; markUsed img)
                  | S.E_FieldFn _ => ()
                (* end case *))
          in
            analyzeBlk blk
          end

  (* count variable uses in a function *)
    and analyzeFunc (S.Func{params, body, ...}) = (
          List.app setBoolUnknown params;
          analyzeBlock body)

  (* count the variable uses in a strand *)
    fun analyzeStrand (S.Strand{
          params, spatialDim, state, stateInit, startM, updateM, stabilizeM, ...
        }) = (
          List.app setBoolUnknown params;
        (* mark all outputs as being used *)
          List.app
            (fn x => (case SV.kindOf x of SV.StrandOutputVar => use x | _ => ()))
              state;
          analyzeBlock stateInit;
          Option.app analyzeBlock startM;
          analyzeBlock updateM;
          Option.app analyzeBlock stabilizeM)

  (* an initial pass to count the variable uses over the entire program *)
    fun analyze prog = let
          val S.Program{
                  inputs, constInit, globInit, funcs, strand, create, start, update, ...
                } = prog
          fun doInput (Inputs.INP{var, ...}) = setBoolUnknown var
          in
            List.app doInput inputs;
            analyzeBlock constInit;
            analyzeBlock globInit;
            List.app analyzeFunc funcs;
            analyzeStrand strand;
            Create.app analyzeBlock create;
            Option.app analyzeBlock start;
            Option.app analyzeBlock update
          end

  (* delete an expression by decrementing the use counts of the variables in it *)
    fun deleteExp exp = (case exp
           of S.E_Var x => unuse x
            | S.E_Lit _ => ()
            | S.E_Kernel _ => ()
            | S.E_Select(x, fld) => (unuse x; unuse fld)
            | S.E_Apply(f, xs) => (SimpleFunc.decCnt f; List.app unuse xs)
            | S.E_Prim(_, _, xs, _) => List.app unuse xs
            | S.E_Tensor(xs, _) => List.app unuse xs
            | S.E_Field(xs, _) => List.app markUsed xs
            | S.E_Seq(xs, _) => List.app unuse xs
            | S.E_Tuple xs => List.app unuse xs
            | S.E_Project(x, _) => unuse x
            | S.E_Slice(x, _, _) => unuse x
            | S.E_Coerce{x, ...} => unuse x
            | S.E_BorderCtl(BorderCtl.Default x, y) => (unuse x; unuse y)
            | S.E_BorderCtl(_, x) => unuse x
            | S.E_LoadSeq _ => ()
            | S.E_LoadImage _ => ()
            | S.E_InsideImage(pos, img, _) => (unuse pos; unuse img)
            | S.E_FieldFn f => ignore(SimpleFunc.decCnt f)
          (* end case *))

  (* delete a block of code and decrement use counts of the variables in it *)
    fun deleteBlk (S.Block{code, ...}) = let
          fun deleteStm stm = (case stm
                 of S.S_Var(_, SOME e) => deleteExp e
                  | S.S_Assign(_, e) => deleteExp e
                  | S.S_IfThenElse(x, blk1, blk2) => (unuse x; deleteBlk blk1; deleteBlk blk2)
                  | S.S_Foreach(_, xs, blk) => (unuse xs; deleteBlk blk)
                  | S.S_New(_, xs) => List.app unuse xs
                  | S.S_Return x => unuse x
                  | S.S_Print xs => List.app unuse xs
                  | S.S_MapReduce mrs => let
                      fun deleteMR (S.MapReduce{mapf=S.Func{body, ...}, args, source, ...}) = (
                            List.app unuse args; deleteBlk body)
                      in
                        List.app deleteMR mrs
                      end
                  | _ => ()
                (* end case *))
          in
            List.app deleteStm code
          end

  (* rewrite a block and remove references to unused variables *)
    fun contractBlock blk = let
          fun contractBlk (S.Block{props, code}) = let
                fun contractStms [] = []
                  | contractStms (stm::stms) = (case stm
                       of S.S_Var(x, NONE) => if isUsed x
                            then stm :: contractStms stms
                            else (ST.tick cntUnusedLocal; contractStms stms)
                        | S.S_Var(x, SOME e) => if isUsed x orelse hasEffect e
                            then stm :: contractStms stms
                            else (ST.tick cntUnusedLocal; deleteExp e; contractStms stms)
                        | S.S_Assign(x, e) => if isUsed x orelse hasEffect e
                            then stm :: contractStms stms
                            else (ST.tick cntDeadAssign; deleteExp e; contractStms stms)
                        | S.S_IfThenElse(x, b1, b2) => (
                            case getBoolVar x
                             of SOME b => let
                                  val S.Block{code, ...} = if b
                                        then (deleteBlk b2; b1)
                                        else (deleteBlk b1; b2)
                                  in
                                    ST.tick cntStaticIf;
                                    unuse x;
                                    contractStms (code @ stms)
                                  end
                              | NONE => (case (contractBlk b1, contractBlk b2)
                                   of (S.Block{code=[], ...}, S.Block{code=[], ...}) => (
                                        ST.tick cntDeadIf; unuse x; contractStms stms)
                                    | (b1, b2) => S.S_IfThenElse(x, b1, b2) :: contractStms stms
                                  (* end case *))
                            (* end case *))
                        | S.S_Foreach(x, xs, blk) => (
                            case contractBlk blk
                             of S.Block{code=[], ...} => (
                                  ST.tick cntDeadForeach; unuse xs; contractStms stms)
                              | blk => S.S_Foreach(x, xs, blk) :: contractStms stms
                            (* end case *))
                        | _ => stm :: contractStms stms
                      (* end case *))
                in
                  S.Block{props = props, code = contractStms code}
                end
          fun loop (nChanges, blk) = let
                val blk = contractBlk blk
                val n = sumChanges()
                in
                  if (n <> nChanges) then loop (n, blk) else blk
                end
          in
            loop (sumChanges(), blk)
          end

  (* contract a strand *)
    fun contractStrand strand = let
          val S.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          in
            S.Strand{
                name = name, params = params, spatialDim = spatialDim, state = state,
                stateInit = contractBlock stateInit,
                startM = Option.map contractBlock startM,
                updateM = contractBlock updateM,
                stabilizeM = Option.map contractBlock stabilizeM
              }
          end

  (* contract a program *)
    fun contractProg (nChanges, prog) = let
          val S.Program{
                  props, consts, inputs, constInit, globals, funcs,
                  globInit, strand, create, start, update
                } = prog
          val constInit = contractBlock constInit
          val globInit = contractBlock globInit
          val strand = contractStrand strand
          val start = Option.map contractBlock start
          val update = Option.map contractBlock update
          val n = sumChanges()
          in
            if n = nChanges
              then (n, prog)
              else (n, S.Program{
                  props = props, consts = consts, inputs = inputs,
                  constInit = constInit, globals = globals, funcs = funcs,
                  globInit = globInit, strand = strand, create = create,
                  start = start, update = update
                })
          end

  (* remove unused state variables from a strand and clear properties *)
    fun finishStrand strand = let
          val S.Strand{
                name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
              } = strand
          val (used, unused) = List.partition isUsed state
          in
            List.app clrUsedMark used;
            if List.null unused
              then strand
              else (
                List.app (fn _ => ST.tick cntUnusedState) unused;
                S.Strand{
                    name = name, params = params, spatialDim = spatialDim,
                    state = used, stateInit = stateInit,
                    startM = startM, updateM = updateM, stabilizeM = stabilizeM
                  })
          end

  (* remove unused constant, global, and state variables from the program *)
    fun finishProg prog = let
          val S.Program{
                  props, consts, inputs, constInit, globals, funcs,
                  globInit, strand, create, start, update
                } = prog
          fun removeUnused cntr x = if isUsed x
                then true
                else (ST.tick cntr; false)
          val consts = List.filter (removeUnused cntUnusedConst) consts
          val globals = List.filter (removeUnused cntUnusedGlobal) globals
        (* remove program properties that are no-longer true to account for
         * dead-code elimination
         *)
          val props = if List.null consts
                then P.clearProp P.HasConsts props
                else props
          val props = if List.null globals andalso List.null inputs
                then P.clearProp P.HasGlobals props
                else props
          val (strand, props) = (case strand
                 of S.Strand{
                        name, params, spatialDim=SOME _, state,
                        stateInit, startM, updateM, stabilizeM
                      } => if List.all (fn x => (SV.nameOf x <> "pos")) state
                        then let (* strand communication was deleted *)
                          val strand = S.Strand{
                                  name=name, params=params, spatialDim=NONE,
                                  state=state, stateInit=stateInit,
                                  startM=startM, updateM=updateM,
                                  stabilizeM=stabilizeM
                                }
                          val props = P.clearProp P.StrandCommunication props
                          in
                            (strand, props)
                          end
                        else (strand, props)
                  | _ => (strand, props)
                (* end case *))
(* FIXME: DynamicSeq, StrandsMayDie, and NewStrands may also be invalid *)
          val prog = S.Program{
                  props = props,
                  consts = consts,
                  inputs = inputs,
                  constInit = constInit,
                  globals = globals,
                  funcs = funcs,
                  globInit = globInit,
                  strand = finishStrand strand,
                  create = create,
                  start = start,
                  update = update
                }
          in
            List.app clrUsedMark consts;
            List.app clrUsedMark globals;
            prog
          end

    fun transform prog = let
        (* first we count the variable uses over the entire program *)
          val () = analyze prog
        (* then contract until there are no more changes *)
          val n = sumChanges()
          val (nChanges, prog) = let
                fun loop (nChanges, prog) = let
                      val (n, prog) = contractProg (nChanges, prog)
                      in
                        if (n <> nChanges) then loop (n, prog) else (n, prog)
                      end
                in
                  loop (n, prog)
                end
          in
          (* finally we finish the program by removing unused constant, global, and state variables *)
            if (n <> nChanges)
              then finishProg prog
              else prog (* no contraction occurred *)
          end

  end
