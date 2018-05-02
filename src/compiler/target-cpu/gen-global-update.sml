(* gen-global-update.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GenGlobalUpdate : sig

  (* generate the global start/update function *)
    val gen : CodeGenEnv.t * string * TreeIR.block -> CLang.decl

  end = struct

    structure IR = TreeIR
    structure R = Reductions
    structure Env = CodeGenEnv
    structure CL = CLang
    structure RN = CxxNames

  (* make the C++ expression for a reduction *)
    fun mkReduce (red, e1, e2) = (case red
           of R.ALL => CL.mkBinOp(e1, CL.#&&, e2)
            | R.EXISTS => CL.mkBinOp(e1, CL.#||, e2)
            | R.IMAX => CL.mkApply("std::max", [e1, e2])
            | R.RMAX => CL.mkApply("std::max", [e1, e2])
            | R.IMIN => CL.mkApply("std::min", [e1, e2])
            | R.RMIN => CL.mkApply("std::min", [e1, e2])
            | R.IPRODUCT => CL.mkBinOp(e1, CL.#*, e2)
            | R.RPRODUCT => CL.mkBinOp(e1, CL.#*, e2)
            | R.ISUM => CL.mkBinOp(e1, CL.#+, e2)
            | R.RSUM => CL.mkBinOp(e1, CL.#+, e2)
          (* end case *))

  (* code generation for sequential map-reduce *)
(*
    accum = /* reduction identity */;
    for (auto ix = this->_strands.begin_XXX()
        ix != this->_strands.end_XXX();
        ix = this->_strands.next_XXX(ix))
    {
        S_strand *s = this->_strands.strand(ix);
        diderot::strand_status sts = this->_strands.status(ix);
        if (/* s in strand set */) {
            value = /* map code */
            accum = REDUCE(value, accm);
        }
    }
*)

    fun genMapReduceSeq (env, mrs, src, stms) = let
          val worldV = CL.mkVar(Env.world env)
          val strandTy = let
                val TreeTypes.StrandIdTy s = TreeVar.ty src
                in
                  RN.strandTy(Atom.toString s)
                end
        (* compute the least-upper bound of the sources *)
          val srcSet = let
                val IR.MapReduce(_, _, _, _, srcSet)::rest = mrs
                in
                  List.foldl
                    (fn (IR.MapReduce(_, _, _, _, set), set') => StrandSets.join(set, set'))
                      srcSet rest
                end
        (* define accumulators for lhs results *)
          val (accs, stms, env) = let
                fun mkAcc (IR.MapReduce(x, r, _, _, set), (accs, stms, env)) = let
                      val acc = TreeVar.name x
                      val stm = CL.mkDeclInit(
                            TypeToCxx.trType (env, TreeVar.ty x), acc,
                            TreeToCxx.trLit(env, R.identity r))
                      val env = Env.insert(env, x, acc)
                      in
                        (acc::accs, stm::stms, env)
                      end
                in
                  List.foldr mkAcc ([], stms, env) mrs
                end
        (* _strands array access *)
          val strands = RN.strandArray env
        (* strand index *)
          val ix = CodeGenUtil.freshVar "ix"
        (* strand status (if needed in doMR) *)
          val status = CodeGenUtil.freshVar "sts"
          fun needsStatus (IR.MapReduce(_, _, _, _, set)) = not (StrandSets.same(srcSet, set))
          val statusDcl = if List.exists needsStatus mrs
                then let
                  val stsTy = CL.T_Named "diderot::strand_status"
                  in [
                    CL.mkDeclInit(stsTy, status,
                      CL.mkDispatch(strands, "status", [CL.mkVar ix]))
                  ] end
                else []
        (* map the index to the strand ID *)
          val id = CodeGenUtil.freshVar "id"
          val idDecl = CL.mkDeclInit(CL.T_Named "strand_array::sid_t", id,
                CL.mkDispatch(strands, "id", [CL.mkVar ix]))
          val env = Env.insert(env, src, id)
        (* build the body of the loop *)
          fun doMR (acc, IR.MapReduce(x, r, f, args, set)) = let
                val mapExp = TreeToCxx.trApply (env, f, args)
                val reduceStm = CL.mkAssign(CL.mkVar acc, mkReduce(r, CL.mkVar acc, mapExp))
                in
                  if StrandSets.same(srcSet, set)
                    then reduceStm
                    else let
                      val kSts = (case set
                             of StrandSets.ACTIVE => "diderot::kActive"
                              | StrandSets.STABLE => "diderot::kStable"
                              | StrandSets.ALL => raise Fail "impossible"
                            (* end case *))
                      in
                        CL.mkIfThen(CL.mkBinOp(CL.mkVar kSts, CL.#==, CL.mkVar status), reduceStm)
                      end
                end
          val loopBody = statusDcl @ idDecl :: ListPair.map doMR (accs, mrs)
          val {begin, stop, next} = let
                val suffix = (case srcSet
                       of StrandSets.ACTIVE => "active"
                        | StrandSets.STABLE => "stable"
                        | StrandSets.ALL => "alive"
                      (* end case *))
                in {
                  begin = CL.mkDispatch(strands, "begin_"^suffix, []),
                  stop = CL.mkDispatch(strands, "end_"^suffix, []),
                  next = CL.mkDispatch(strands, "next_"^suffix, [CL.mkVar ix])
                } end
          val loopStm = CL.mkFor(
                CL.autoTy, [(ix, begin)],
                CL.mkBinOp(CL.mkVar ix, CL.#!=, stop),
                [CL.mkAssignOp(CL.mkVar ix, CL.$=, next)],
                CL.mkBlock loopBody)
          in
            (env, loopStm :: stms)
          end

  (* generate the global start/update function *)
    fun gen (env, name, body) = let
          val env = Env.insert(env, PseudoVars.world, "this")
          val _ = Env.setMapReduceCB (env, genMapReduceSeq)
          in
            CL.D_Func([], CL.voidTy, [], "world::global_" ^ name,
              [],
              GenUtil.genBodyWithGlobPtr (env, body))
          end

  end
