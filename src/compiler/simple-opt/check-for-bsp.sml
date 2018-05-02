(* check-for-bsp.sml
 *
 * Check for features that require the program to be executed in BSP mode.  These
 * features are:
 *
 *   - the use of number-of-strands query functions in strand
 *     methods (including indirect uses via user-defined functions).
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure CheckForBSP : sig

    val check : Simple.program -> Simple.program

  end = struct

    structure S = Simple
    structure FSet = SimpleFunc.Set

    fun chkBlock (sqFns, blk) = let
          fun chkBlk (S.Block{code, ...}) = List.exists chkStm code
          and chkStm (S.S_Var(_, SOME e)) = chkExp e
            | chkStm (S.S_Assign(_, e)) = chkExp e
            | chkStm (S.S_IfThenElse(_, b1, b2)) = chkBlk b1 orelse chkBlk b2
            | chkStm (S.S_Foreach(_, _, b)) = chkBlk b
            | chkStm _ = false
          and chkExp (S.E_Apply(f, _)) = FSet.member(sqFns, f)
            | chkExp (S.E_Prim(f, _, _, _)) = Basis.isStrandsQueryFn f
            | chkExp _ = false
          in
            chkBlk blk
          end

    fun chkFn (S.Func{f, body, ...}, sqFns) = if chkBlock (sqFns, body)
          then FSet.add(sqFns, f)
          else sqFns

    fun chkStrand (sqFns, S.Strand{stateInit, startM, updateM, stabilizeM, ...}) = let
          fun chkOptBlk NONE = false
            | chkOptBlk (SOME blk) = chkBlock (sqFns, blk)
          in
            chkBlock (sqFns, stateInit) orelse
            chkOptBlk startM orelse
            chkBlock (sqFns, updateM) orelse
            chkOptBlk stabilizeM
          end

    fun check prog = let
          val S.Program{
                  props, consts, inputs, constInit, globals, globInit, funcs, strand,
                  create, start, update
                } = prog
          val sqFns = List.foldl chkFn FSet.empty funcs
          val props = if chkStrand(sqFns, strand)
                then Properties.NeedsBSP :: props
                else props
          in
            S.Program{
                props=props, consts=consts, inputs=inputs, constInit=constInit,
                globals=globals, globInit=globInit, funcs=funcs, strand=strand,
                create=create, start=start, update=update
              }
          end

  end
