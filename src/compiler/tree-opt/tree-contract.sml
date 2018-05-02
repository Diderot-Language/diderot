(* tree-contract.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TreeContract : sig

    val transform : TreeIR.program -> TreeIR.program

  end = struct

    structure IR = TreeIR
    structure V = TreeVar
    structure Ty = TreeTypes
    structure Op = TreeOps
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntIdentityAssign       = ST.newCounter "tree-contract:identity-assign"
    val cntIndexLoad            = ST.newCounter "tree-contract:index-vload"
    val cntIndexCons            = ST.newCounter "tree-contract:index-cons"
    val cntPackVecs             = ST.newCounter "tree-contract:pack-vecs"

(* rewrites:
        VIndex<_, _, i>(VLoad(_, e, j)) ==> TensorIndex
        Pack(Vec _, ..., Vec _) => CONS
        VLoad(_, CONS([..., xs, ...], _), _) => Vec(xs)
*)

  (* are there available rewrites for an expression? *)
    fun canReduce exp = (case exp
           of IR.E_Global _ => false
            | IR.E_State _ => false
            | IR.E_Var _ => false
            | IR.E_Lit _ => false
            | IR.E_Op(Op.VIndex _, [IR.E_VLoad _]) => true
            | IR.E_Op(_, es) => List.exists canReduce es
            | IR.E_Apply(_, es) => List.exists canReduce es
            | IR.E_Vec(_, _, es) => List.exists canReduce es
            | IR.E_Cons(es, _) => List.exists canReduce es
            | IR.E_Seq(es, _) => List.exists canReduce es
            | IR.E_Pack(layout, es) => let
                fun chk ([], allVecs) = allVecs
                  | chk ((e as IR.E_Vec _)::es, true) = canReduce e orelse chk (es, true)
                  | chk (e::es, _) = canReduce e orelse chk (es, false)
                in
                  chk (es, true)
                end
            | IR.E_VLoad(_, e, _) => canReduce e
          (* end case *))

    fun reduce exp = (case exp
           of IR.E_Global _ => exp
            | IR.E_State _ => exp
            | IR.E_Var _ => exp
            | IR.E_Lit _ => exp
            | IR.E_Op(Op.VIndex(_, _, i), [IR.E_VLoad(layout, e, j)]) => let
                fun index (0, _, idx) = idx
                  | index (j, p::ps, idx) = index (j-1, ps, idx+p)
                val idx = index (j, #pieces layout, i)
                in
                  ST.tick cntIndexLoad;
                  case e
                   of IR.E_Cons(es, _) => (ST.tick cntIndexCons; reduce (List.nth(es, idx)))
                    | _ => IR.E_Op(Op.TensorIndex(Ty.TensorRefTy[#wid layout], [idx]), [reduce e])
                  (* end case *)
                end
            | IR.E_Op(rator, es) => IR.E_Op(rator, List.map reduce es)
            | IR.E_Apply(f, es) => IR.E_Apply(f, List.map reduce es)
            | IR.E_Vec(w, pw, es) => IR.E_Vec(w, pw, List.map reduce es)
            | IR.E_Cons(es, ty) => IR.E_Cons(List.map reduce es, ty)
            | IR.E_Seq(es, ty) => IR.E_Seq(List.map reduce es, ty)
            | IR.E_Pack(layout, es) => let
                val es = List.map reduce es
                in
                  if List.all (fn (IR.E_Vec _) => true | _ => false) es
                    then (
                      ST.tick cntPackVecs;
                      IR.E_Cons(
                        List.foldr (fn (IR.E_Vec(_, _, es), es') => es @ es') [] es,
                        Ty.TensorTy[#wid layout]))
                    else IR.E_Pack(layout, es)
                end
(* TODO: check for CONS arg *)
            | IR.E_VLoad(layout, e, i) => IR.E_VLoad(layout, reduce e, i)
          (* end case *))

    fun reduceBlock dualState = let
          fun condReduce e = if canReduce e then reduce e else e
          fun reduceStms ([], stms') = List.rev stms'
            | reduceStms (stm::stms, stms') = let
                fun continue stms' = reduceStms (stms, stms')
                in
                  case stm
                   of IR.S_Assign(isDcl, x, e) =>
                        if canReduce e
                          then continue (IR.S_Assign(isDcl, x, reduce e) :: stms')
                          else continue (stm::stms')
                    | IR.S_MAssign(xs, e) =>
                        if canReduce e
                          then continue (IR.S_MAssign(xs, reduce e) :: stms')
                          else continue (stm::stms')
                    | IR.S_GAssign(gv, IR.E_Global gv') =>
                        if TreeGlobalVar.same(gv, gv')
                          then (ST.tick cntIdentityAssign; continue stms')
                          else continue (stm::stms')
                    | IR.S_GAssign(gv, e) =>
                        if canReduce e
                          then continue (IR.S_GAssign(gv, reduce e) :: stms')
                          else continue (stm::stms')
                    | IR.S_IfThen(e, blk) =>
                        continue (IR.S_IfThen(condReduce e, reduceBlk blk) :: stms')
                    | IR.S_IfThenElse(e, blk1, blk2) =>
                        continue (
                          IR.S_IfThenElse(condReduce e, reduceBlk blk1, reduceBlk blk2)
                          :: stms')
                    | IR.S_For(x, lo, hi, blk) =>
                        continue (IR.S_For(x, condReduce lo, condReduce hi, reduceBlk blk) :: stms')
                    | IR.S_Foreach(x, e, blk) =>
                        continue (IR.S_Foreach(x, condReduce e, reduceBlk blk) :: stms')
                    | IR.S_Input(gv, name, desc, SOME e) =>
                        if canReduce e
                          then continue (IR.S_Input(gv, name, desc, SOME(reduce e)) :: stms')
                          else continue (stm::stms')
                    | IR.S_New(name, args) =>
                        if List.exists canReduce args
                          then continue (IR.S_New(name, List.map reduce args) :: stms')
                          else continue (stm::stms')
                    | IR.S_Save(sv, IR.E_State(NONE, sv')) =>
                        if TreeStateVar.same(sv, sv') andalso (not dualState)
                          then (ST.tick cntIdentityAssign; continue stms')
                          else continue (stm::stms')
                    | IR.S_Save(sv, e) =>
                        if canReduce e
                          then continue (IR.S_Save(sv, reduce e) :: stms')
                          else continue (stm::stms')
                    | _ => continue (stm::stms')
                  (* end case *)
                end
            and reduceBlk (IR.Block{locals, body}) = IR.Block{
                    locals = locals, body = reduceStms (body, [])
                  }
          in
            reduceBlk
          end

(* TODO: unused variable elimination; tensor/vector conversion elimination *)
    fun transform prog = let
          val IR.Program{
                props, target, consts, inputs, constInit, globals,
                funcs, globInit, strand, create, start, update
              } = prog
          val IR.Strand{
                  name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                } = strand
          val reduceBlk = reduceBlock (Properties.dualState props)
          fun reduceMethod (IR.Method{needsW, hasG, body}) = IR.Method{
                  needsW = needsW, hasG = hasG, body = reduceBlk body
                }
          val constInit = reduceBlk constInit
          val globInit = reduceBlk globInit
          val funcs = let
                fun reduceFunc (IR.Func{name, params, body}) =
                      IR.Func{name = name, params = params, body = reduceBlk body}
                in
                  List.map reduceFunc funcs
                end
          val strand = IR.Strand{
                  name = name, params = params,
                  spatialDim = spatialDim, state = state,
                  stateInit = reduceMethod stateInit,
                  startM = Option.map reduceMethod startM,
                  updateM = reduceMethod updateM,
                  stabilizeM = Option.map reduceMethod stabilizeM
                }
          val create = Create.map reduceBlk create
          val start = Option.map reduceBlk start
          val update = Option.map reduceBlk update
          in
            IR.Program{
                props = props, target = target, consts = consts, inputs = inputs,
                constInit = constInit, globals = globals, funcs = funcs,
                globInit = globInit, strand = strand, create = create, start = start,
                update = update
              }
          end

  end
