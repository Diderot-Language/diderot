(* promote-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * Transformations, such as normalization, can create references to variables defined
 * in the global initialization section from nested scopes (e.g., inside strands).
 * This functor implements a pass that promotes such variables to be globals.
 *)

functor PromoteFn (IR : SSA) : sig

    val transform : IR.program -> IR.program

  end = struct

    structure V = IR.Var
    structure VTbl = V.Tbl
    structure VMap = V.Map
    structure GV = IR.GlobalVar

  (* adjust a variable's use count *)
    fun incUse (IR.V{useCnt, ...}) = (useCnt := !useCnt + 1)
    fun decUse (IR.V{useCnt, ...}) = (useCnt := !useCnt - 1)
    fun incGlobal (IR.GV{useCnt, ...}) = (useCnt := !useCnt + 1)

  (* local variables in the global initialization section are either just used locally
   * or used to define a global (via GASSIGN).
   *)
    datatype info = LOCAL | GLOBAL of IR.global_var

    fun initTbl globalInit = let
          val tbl = VTbl.mkTable (64, Fail "promote")
          val insert = VTbl.insert tbl
          fun doNode nd = (case IR.Node.kind nd
                 of IR.GASSIGN{lhs, rhs, ...} => insert (rhs, GLOBAL lhs)
                  | _ => List.app (fn x => insert (x, LOCAL)) (IR.Node.defs nd)
                (* end case *))
          in
            IR.CFG.apply doNode globalInit;
            tbl
          end

  (* attempt to resolve a global-scope local to a global.  This situation can occur when
   * an input variable is bound to a local in the global initialization code and then the
   * local's scope is extruded.
   *)
    fun resolveVar x = (case V.binding x
           of IR.VB_RHS(IR.GLOBAL gv) => SOME gv
            | IR.VB_RHS(IR.VAR x) => resolveVar x
            | _ => NONE
          (* end case *))

    fun transform prog = let
          val IR.Program{
                  props, consts, inputs, constInit, globals,
                  funcs, globInit, strand, create, start, update
                } = prog
        (* table mapping locals in the globalInit to info *)
          val promoteTbl = initTbl globInit
        (* newly defined globals paired with their globalInit local definition *)
          val newGlobs : (IR.global_var * IR.var) list ref = ref []
        (* create a new global for the local variable x *)
          fun promoteVar x = let
                fun uniqueName name =
                      if List.exists (fn (g, _) => (GV.name g = name)) (!newGlobs)
                      orelse List.exists (fn g => (GV.name g = name)) globals
                        then uniqueName (name ^ "X")
                        else name
                in
                  GV.new (IR.GlobalVar, false, uniqueName(V.name x), V.ty x)
                end
        (* update a CFG as necessary to references to globalInit locals with references to new
         * globals.
         *)
          fun doCFG cfg = let
              (* assignment statements to load local copies of the new globals; these get put
               * at the beginning of the block.
               *)
                val initStms = ref []
                fun doNode (nd as IR.ND{id, props, kind}, env) = let
                      fun doUse (false, [], _, env, _) = env
                        | doUse (true, [], ys, env, mkNode) = (
                            IR.CFG.replaceNode (nd, mkNode(List.rev ys));
                            env)
                        | doUse (changed, x::xs, ys, env, mkNode) = (case VTbl.find promoteTbl x
                             of SOME LOCAL => let
                                  val x' = V.copy x
                                  fun rewrite gx = let
                                        val x' = V.copy x
                                        in
                                          decUse x; incUse x'; incGlobal gx;
                                          initStms := IR.ASSGN(x', IR.GLOBAL gx) :: !initStms;
                                          VTbl.insert promoteTbl (x, GLOBAL gx);
                                          doUse (true, xs, x'::ys, VMap.insert(env, x, x'), mkNode)
                                        end
                                  in
                                    case resolveVar x
                                     of SOME gx => rewrite gx
                                      | NONE => let
                                          val gx = promoteVar x
                                          in
Log.msg' (fn () => ["promote ", V.toString x, " --> ", GV.toString gx, "\n"]);
                                            newGlobs := (gx, x) :: !newGlobs;
                                            rewrite gx
                                          end
                                    (* end case *)
                                  end
                              | SOME(GLOBAL gx) => (case VMap.find(env, x)
                                   of SOME x' => (
                                        decUse x; incUse x';
                                        doUse (true, xs, x'::ys, env, mkNode))
                                    | NONE => let (* no previous use of g in this CFG *)
                                        val x' = V.copy x
                                        in
                                          decUse x; incUse x'; incGlobal gx;
                                          initStms := IR.ASSGN(x', IR.GLOBAL gx) :: !initStms;
                                          doUse (true, xs, x'::ys, VMap.insert(env, x, x'), mkNode)
                                        end
                                  (* end case *))
                              | NONE => doUse (changed, xs, x::ys, env, mkNode)
                            (* end case *))
                      fun doArgs (args, mkNode) = doUse (false, args, [], env, mkNode)
                      in
                        case kind
                         of IR.NULL => env
                          | IR.ENTRY _ => env
                          | IR.JOIN _ => env
                          | IR.COND _ => env
                          | IR.FOREACH _ => env
                          | IR.NEXT _ => env
                          | IR.COM _ => env
                          | IR.ASSIGN{stm=(lhs, rhs), ...} => (case rhs
                               of IR.GLOBAL _ => env
                                | IR.STATE _ => env
                                | IR.VAR x => doArgs ([x], fn [x] => IR.Node.mkASSIGN(lhs, IR.VAR x))
                                | IR.LIT _ => env
                                | IR.OP(rator, args) => doArgs (args,
                                    fn args => IR.Node.mkASSIGN(lhs, IR.OP(rator, args)))
                                | IR.CONS(args, ty) => doArgs (args,
                                    fn args => IR.Node.mkASSIGN(lhs, IR.CONS(args, ty)))
                                | IR.SEQ(args, ty) => doArgs (args,
                                    fn args => IR.Node.mkASSIGN(lhs, IR.SEQ(args, ty)))
                                | IR.EINAPP(ein, args) => doArgs (args,
                                    fn args => IR.Node.mkASSIGN(lhs, IR.EINAPP(ein, args)))
                                | IR.APPLY(f, args) => doArgs (args,
                                    fn args => IR.Node.mkASSIGN(lhs, IR.APPLY(f, args)))
                                | _ => raise Fail("bogus rhs for ASSIGN: " ^ IR.RHS.toString rhs)
                              (* end case *))
                          | IR.MASSIGN{stm=(lhs, rhs), ...} => (case rhs
                               of IR.OP(rator, args) => doArgs (args,
                                    fn args => IR.Node.mkMASSIGN(lhs, IR.OP(rator, args)))
                                | IR.MAPREDUCE[(r, f, args)] => doArgs (args,
                                    fn args => IR.Node.mkMASSIGN(lhs, IR.MAPREDUCE[(r, f, args)]))
                                | IR.MAPREDUCE _ => raise Fail "FIXME: fused MAPREDUCE"
                                | _ => raise Fail("bogus rhs for MASSIGN: " ^ IR.RHS.toString rhs)
                              (* end case *))
                          | IR.GASSIGN{lhs, rhs, ...} =>
                              doArgs ([rhs], fn [x] => IR.Node.mkGASSIGN(lhs, rhs))
                          | IR.NEW{strand, args, ...} =>
                              doArgs (args, fn args => IR.Node.mkNEW(strand, args))
                          | IR.SAVE{lhs, rhs, ...} =>
                              doArgs ([rhs], fn [rhs] => IR.Node.mkSAVE(lhs, rhs))
                          | IR.EXIT _ => env
                        (* end case *)
                      end
                in
                  List.foldl doNode VMap.empty (IR.CFG.sort cfg);
                  case !initStms
                   of [] => cfg
                    | stms => IR.CFG.prependBlock(List.rev stms, cfg)
                  (* end case *)
                end
        (* process the functions *)
          val funcs' = let
                fun doFunc (IR.Func{name, params, body}) = IR.Func{
                        name = name, params = params, body = doCFG body
                      }
                in
                  List.map doFunc funcs
                end
        (* process the strand *)
          val strand' = let
                val IR.Strand{
                        name, params, spatialDim, state, stateInit, startM, updateM, stabilizeM
                      } = strand
                in
                  IR.Strand{
                      name = name, params = params,
                      spatialDim = spatialDim, state = state,
                      stateInit = doCFG stateInit,
                      startM = Option.map doCFG startM,
                      updateM = doCFG updateM,
                      stabilizeM = Option.map doCFG stabilizeM
                    }
                end
        (* process the initial strand creation code *)
          val create' = Create.map doCFG create
        (* process the global start *)
          val start' = Option.map doCFG start
        (* process the global update *)
          val update' = Option.map doCFG update
        (* check to see if we have created new globals and thus need to update the globalInit block *)
          val (props', globals', globInit') = (case !newGlobs
                 of [] => (props, globals, globInit)
                  | globs => let
                      fun f ([], globals, stms) = (globals, stms)
                        | f ((gx, x)::globs, globals, stms) = (
                              incUse x;
                              GV.setBinding(gx, x);
                              f (globs, gx::globals, IR.GASSGN(gx, x)::stms))
                      val (globals', stms) = f (globs, [], [])
                    (* check to see if we are creating globals where there were none before *)
                      val props' = if List.null globals
                            then Properties.HasGlobals :: props
                            else props
                      in
                        (props', globals @ globals', IR.CFG.appendBlock (globInit, stms))
                      end
                (* end case *))
          in
            IR.Program{
                props = props',
                consts = consts,
                inputs = inputs,
                constInit = constInit,
                globals = globals',
                funcs = funcs',
                globInit = globInit',
                strand = strand',
                create = create',
                start = start',
                update = update'
              }
          end

  end
