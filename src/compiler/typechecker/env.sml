(* env.sml
 *
 * Environments and contexts to support typechecking.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2018 The University of Chicago
 * All rights reserved.
 *)

structure Env : sig

    type t

  (* create a new environment at global scope that includes the basis environment *)
    val new : ParseTree.version -> t

  (* extract the underlying global environment *)
    val globalEnv : t -> GlobalEnv.t

    type context = Error.err_stream * Error.span

    datatype scope
      = GlobalScope                             (* global declarations *)
      | FunctionScope of Types.ty * Atom.atom   (* inside a function definition *)
      | StrandScope of Atom.atom                (* inside a strand definition *)
      | MethodScope of Atom.atom * StrandUtil.method_name
                                                (* inside a strand method definition *)
      | InitScope                               (* inside global initialization *)
      | CreateScope                             (* inside initial strand creation *)
      | StartScope                              (* inside global start *)
      | UpdateScope                             (* inside global update *)

  (* string representation of scope *)
    val scopeToString : scope -> string

  (* return the current scope *)
    val currentScope : t -> scope

  (* are we currently inside a foreach loop? *)
    val inLoop : t -> bool

  (* are we currently in a strand method scope? *)
    val inMethod : t -> bool

  (* are we currently in a strand scope? *)
    val inStrand : t -> bool

  (* are we currently in the global initialization block? *)
    val inGlobalInit : t -> bool

  (* are we currently in a global start or update block? *)
    val inGlobalBlock : t -> bool

  (* start a new function scope *)
    val functionScope : t * Types.ty * Atom.atom -> t
  (* start a new strand scope *)
    val strandScope : t * Atom.atom -> t
  (* start a new strand-method scope *)
    val methodScope : t * StrandUtil.method_name -> t
  (* start a new global initialization scope *)
    val initScope : t -> t
  (* start a new initial-strand creation scope *)
    val createScope : t -> t
  (* start a new global start scope *)
    val startScope : t -> t
  (* start a new global update scope *)
    val updateScope : t -> t
  (* start a new block scope *)
    val blockScope : t -> t
  (* start a new loop-body scope *)
    val loopScope : t -> t

  (* get the strand type (NONE if not within Strand or Method scope) *)
    val strandTy : t -> (Types.ty * StrandEnv.t) option

  (* functions are either user-defined or pre-defined *)
    datatype fun_def
      = PrimFun of AST.var list                 (* possibly overloaded builtin function *)
      | UserFun of AST.var                      (* user-defined function *)

  (* insert a global variable into the environment (includes constants and inputs) *)
    val insertGlobal : t * context * Atom.atom * AST.var -> t

  (* insert a user function into the environment *)
    val insertFunc : t * context * Atom.atom * AST.var -> t

  (* insert a strand into the environment *)
    val insertStrand : t * context * StrandEnv.t -> t

  (* insert a local variable into the environment *)
    val insertLocal : t * context * Atom.atom * AST.var -> t

    val findStrand : t * Atom.atom -> StrandEnv.t option
    val findFunc : t * Atom.atom -> fun_def
    val findVar : t * Atom.atom -> AST.var option
    val findKernel : t * Atom.atom -> Kernel.t option

  (* find the strand-set function with the given name *)
    val findSetFn : t * Atom.atom -> AST.var option

  (* unwrap a parse-tree location marker and update the context with the span *)
    val withContext : context * 'a Error.mark -> (context * 'a)

  (* same as `withContext`, but includes the environment too *)
    val withEnvAndContext : t * context * 'a Error.mark -> (t * context * 'a)

  (* check for redefinition of an identifier in the same scope *)
    val checkForRedef : t * context * Atom.atom -> unit

  (* tracking program features *)
    val recordProp : t * Properties.t -> unit
    val hasProp : t * Properties.t -> bool
    val properties : t -> Properties.t list

  end = struct

    structure GE = GlobalEnv
    structure Ty = Types
    structure AMap = AtomMap

    datatype fun_def = datatype GE.fun_def

    datatype scope
      = GlobalScope                             (* global declarations *)
      | FunctionScope of Ty.ty * Atom.atom      (* inside a function definition *)
      | StrandScope of Atom.atom                (* inside a strand definition *)
      | MethodScope of Atom.atom * StrandUtil.method_name
                                                (* inside a strand method definition *)
      | InitScope                               (* inside global initialization *)
      | CreateScope                             (* inside initial strand creation *)
      | StartScope                              (* inside global start *)
      | UpdateScope                             (* inside global update *)

    fun scopeToString GlobalScope = "global scope"
      | scopeToString (FunctionScope(_, f)) = "function " ^ Atom.toString f
      | scopeToString (StrandScope s)= "strand " ^ Atom.toString s
      | scopeToString (MethodScope(s, m)) = concat[
            "method ", Atom.toString s, ".", StrandUtil.nameToString m
          ]
      | scopeToString InitScope = "global initialization"
      | scopeToString CreateScope = "strand creation"
      | scopeToString StartScope = "global start"
      | scopeToString UpdateScope = "global update"

    datatype t = E of {
        scope : scope,                          (* current scope *)
        inLoop : bool,                          (* true if we are inside a loop *)
        bindings : Error.location AMap.map,     (* map from atoms to innermost binding location *)
        gEnv : GE.t,                            (* global environment *)
        vEnv : AST.var AMap.map                 (* variables defined in inner scopes *)
      }

    type context = Error.err_stream * Error.span

  (* create a fresh environment with global scope from a global environment *)
    fun new vers = E{
            scope = GlobalScope,
            inLoop = false,
            bindings = AMap.empty,
            gEnv = Basis.env vers,
            vEnv = AMap.empty
          }

    fun globalEnv (E{gEnv, ...}) = gEnv

  (* start a new scope *)
    fun functionScope (E{gEnv, ...}, ty, f) = E{
            scope=FunctionScope(ty, f), inLoop=false, bindings=AtomMap.empty,
            gEnv=gEnv, vEnv=AMap.empty
          }
    fun strandScope (E{gEnv, ...}, strand) = E{
            scope=StrandScope strand, inLoop=false, bindings=AtomMap.empty,
            gEnv=gEnv, vEnv=AMap.empty
          }
    fun methodScope (E{scope, gEnv, vEnv, ...}, name) = (case scope
           of StrandScope strand => E{
                  scope=MethodScope(strand, name), inLoop=false,
                  bindings=AtomMap.empty, gEnv=gEnv, vEnv=vEnv
                }
            | _ => raise Fail("methodScope called in " ^ scopeToString scope)
          (* end case *))
    fun initScope (E{gEnv, ...}) =
          E{scope=InitScope, inLoop=false, bindings=AtomMap.empty, gEnv=gEnv, vEnv=AMap.empty}
    fun createScope (E{gEnv, ...}) =
          E{scope=CreateScope, inLoop=false, bindings=AtomMap.empty, gEnv=gEnv, vEnv=AMap.empty}
    fun updateScope (E{gEnv, ...}) =
          E{scope=UpdateScope, inLoop=false, bindings=AtomMap.empty, gEnv=gEnv, vEnv=AMap.empty}
    fun startScope (E{gEnv, ...}) =
          E{scope=StartScope, inLoop=false, bindings=AtomMap.empty, gEnv=gEnv, vEnv=AMap.empty}
    fun blockScope (E{scope, inLoop, gEnv, vEnv, ...}) =
          E{scope=scope, inLoop=inLoop, bindings=AtomMap.empty, gEnv=gEnv, vEnv=vEnv}
    fun loopScope (E{scope, gEnv, vEnv, ...}) =
          E{scope=scope, inLoop=true, bindings=AtomMap.empty, gEnv=gEnv, vEnv=vEnv}

    fun currentScope (E{scope, ...}) = scope

    fun inLoop (E{inLoop, ...}) = inLoop

    fun inMethod (E{scope=MethodScope _, ...}) = true
      | inMethod _ = false

    fun inStrand (E{scope=StrandScope _, ...}) = true
      | inStrand (E{scope=MethodScope _, ...}) = true
      | inStrand _ = false

    fun strandTy (E{scope, gEnv, ...}) = let
          fun result s = (case GE.findStrand(gEnv, s)
                 of SOME sEnv => SOME(Types.T_Strand s, sEnv)
                  | _ => raise Fail "impossible"
                (* end case *))
          in
            case scope
             of StrandScope s => result s
              | MethodScope(s, _) => result s
              | _ => NONE
            (* end case *)
          end

    fun inGlobalInit (E{scope=InitScope, ...}) = true
      | inGlobalInit _ = false

    fun inGlobalBlock (E{scope=StartScope, ...}) = true
      | inGlobalBlock (E{scope=UpdateScope, ...}) = true
      | inGlobalBlock _ = false

    fun findStrand (E{gEnv, ...}, s) = GE.findStrand(gEnv, s)
    fun findFunc (E{gEnv, ...}, f) = GE.findFunc(gEnv, f)
    fun findVar (E{gEnv, vEnv, ...}, x) = (case AMap.find(vEnv, x)
           of NONE => GE.findVar (gEnv, x)
            | someVar => someVar
          (* end case *))
    fun findKernel (E{gEnv, ...}, k) = GE.findKernel(gEnv, k)

  (* find the strand-set function with the given name; we do this by brute force
   * for now.
   *)
    fun findSetFn (_, name) =
          if Atom.same(name, BasisNames.set_active) then SOME BasisVars.set_active
          else if Atom.same(name, BasisNames.set_all) then SOME BasisVars.set_all
          else if Atom.same(name, BasisNames.set_stable) then SOME BasisVars.set_stable
          else NONE

    fun insertGlobal (E{scope, inLoop, bindings, gEnv, vEnv}, cxt, x, x') = let
          val loc = Error.location cxt
          in
            GE.insertVar (gEnv, x, x');
            E{
              scope = scope, inLoop = inLoop,
              bindings = AtomMap.insert(bindings, x, loc),
              gEnv = gEnv,
              vEnv = vEnv
            }
          end

    fun insertFunc (E{scope, inLoop, bindings, gEnv, vEnv}, cxt, f, f') =  let
          val loc = Error.location cxt
          in
            GE.insertFunc(gEnv, f, GE.UserFun f');
            E{
              scope = scope, inLoop = inLoop,
              bindings = AtomMap.insert(bindings, f, loc),
              gEnv = gEnv,
              vEnv = vEnv
            }
          end

    fun insertStrand (E{scope, inLoop, bindings, gEnv, vEnv}, cxt, sEnv) = let
          val () = GE.insertStrand(gEnv, sEnv)
          val env = E{
                  scope=scope, inLoop = inLoop,
                  bindings = AtomMap.insert(bindings, StrandEnv.strandName sEnv, Error.location cxt),
                  gEnv = gEnv,
                  vEnv = vEnv
                }
          in
            env
          end

    fun insertLocal (E{scope, inLoop, bindings, gEnv, vEnv}, cxt, x, x') = let
          val loc = Error.location cxt
          in
            E{
              scope = scope, inLoop = inLoop,
              bindings = AtomMap.insert(bindings, x, loc),
              gEnv = gEnv,
              vEnv = AMap.insert(vEnv, x, x')
            }
          end

    fun withContext ((errStrm, _), {span, tree}) =
          ((errStrm, span), tree)
    fun withEnvAndContext (env, (errStrm, _), {span, tree}) =
          (env, (errStrm, span), tree)

  (* check for redefinition of an identifier in the same scope *)
(* TODO: check for shadowing too? *)
    fun checkForRedef (E{bindings, ...}, cxt : context, x) = (case AtomMap.find(bindings, x)
           of SOME loc => TypeError.error (cxt, [
                  TypeError.S "redefinition of ", TypeError.A x,
                  TypeError.S(Error.fmt (", previous definition at line %l", "") loc)
                ])
            | NONE => ()
          (* end case *))

  (* tracking program features *)
    fun recordProp (E{gEnv, ...}, p) = GE.recordProp (gEnv, p)
    fun hasProp (E{gEnv, ...}, p) = GE.hasProp (gEnv, p)
    fun properties (E{gEnv, ...}) = GE.properties gEnv

  end
