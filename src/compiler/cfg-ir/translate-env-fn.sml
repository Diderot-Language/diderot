(* translate-env-fn.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

signature TRANSLATE_ENV_PARAMS =
  sig

    structure SrcIR : SSA
    structure DstIR : SSA

    val cvtTy : SrcIR.Ty.ty -> DstIR.Ty.ty

  end

signature TRANSLATE_ENV =
  sig

    structure SrcIR : SSA
    structure DstIR : SSA

    type t

    val mkEnv : unit -> t

    val rename : (t * SrcIR.var) -> DstIR.var
    val renameList : (t * SrcIR.var list) -> DstIR.var list
    val renameGV : (t * SrcIR.global_var) -> DstIR.global_var
    val renameSV : (t * SrcIR.state_var) -> DstIR.state_var

  (* register a SrcIR function for future translation *)
    val registerFunc : t * SrcIR.func * (t * DstIR.func -> DstIR.func_def) -> unit

  (* get the translated function definitions *)
    val getFuncs : t -> DstIR.func_def list

  (* rename a SrcIR function; this operation may require translating the SrcIR
   * func_def to the DstIR func_def.
   *)
    val renameFV : (t * SrcIR.func) -> DstIR.func

    val insertNd : (t * Stamp.t * DstIR.node) -> unit
    val findNd : t -> Stamp.t -> DstIR.node option

  end

functor TranslateEnvFn (Params : TRANSLATE_ENV_PARAMS) : TRANSLATE_ENV =
  struct

    structure SrcIR = Params.SrcIR
    structure SrcGV = SrcIR.GlobalVar
    structure SrcSV = SrcIR.StateVar
    structure SrcFV = SrcIR.Func
    structure DstIR = Params.DstIR

    type var_env = DstIR.var SrcIR.Var.Tbl.hash_table
    type global_var_env = DstIR.global_var SrcGV.Tbl.hash_table
    type state_var_env = DstIR.state_var SrcSV.Tbl.hash_table

    datatype t = E of {
        ndMap : DstIR.node Stamp.Tbl.hash_table,
        vMap : var_env,
        gvMap : global_var_env,
        svMap : state_var_env,
        fvMap : func SrcFV.Tbl.hash_table,      (* map from SrcIR funcs to `func` values *)
        funcs : DstIR.func_def list ref         (* list of translated functions *)
      }

  (* To avoid cyclic dependencies between translating functions and translating
   * global initialization, we translate functions on demand.  The following
   * datatype represents a function that has not been translated yet or
   * one that has been translated.
   *)
    and func
      = SrcFunc of t * DstIR.func -> DstIR.func_def     (* function has not been translated yet *)
      | DstFunc of DstIR.func                           (* name of translated function *)

    val cvtTy = Params.cvtTy

    fun mkEnv () = E{
            ndMap = Stamp.Tbl.mkTable (256, Fail "ndMap"),
            vMap = SrcIR.Var.Tbl.mkTable (256, Fail "vMap"),
            gvMap = SrcIR.GlobalVar.Tbl.mkTable (64, Fail "gvMap"),
            svMap = SrcIR.StateVar.Tbl.mkTable (64, Fail "svMap"),
            fvMap = SrcIR.Func.Tbl.mkTable (64, Fail "fvMap"),
            funcs = ref []
          }

    fun registerFunc (E{fvMap, ...}, f, mkFunc) = SrcFV.Tbl.insert fvMap (f, SrcFunc mkFunc)

    fun getFuncs (E{funcs, ...}) = List.rev (!funcs)

    fun rename (E{vMap, ...}, x) = (case SrcIR.Var.Tbl.find vMap x
          of SOME x' => x'
           | NONE => let
               val x' = DstIR.Var.new (SrcIR.Var.name x, cvtTy(SrcIR.Var.ty x))
               in
                 SrcIR.Var.Tbl.insert vMap (x, x');
                 x'
               end
         (* end case *))

    fun renameList (env, xs) = List.map (fn x => rename (env, x)) xs

    fun renameGV (E{gvMap, ...}, x) = (case SrcGV.Tbl.find gvMap x
           of SOME x' => x'
            | NONE => let
                val x' = DstIR.GlobalVar.new (
                      SrcGV.kind x, SrcGV.isVarying x,
                      SrcGV.name x, cvtTy(SrcGV.ty x))
                in
                  SrcGV.Tbl.insert gvMap (x, x');
                  x'
                end
          (* end case *))

    fun renameSV (E{svMap, ...}, x) = (case SrcSV.Tbl.find svMap x
           of SOME x' => x'
            | NONE => let
                val x' = DstIR.StateVar.new (
                      SrcSV.isOutput x, SrcSV.name x, cvtTy(SrcSV.ty x),
                      SrcSV.isVarying x, SrcSV.isShared x)
                in
                  SrcSV.Tbl.insert svMap (x, x');
                  x'
                end
          (* end case *))

    fun renameFV (env as E{fvMap, funcs, ...}, f) = (case SrcFV.Tbl.find fvMap f
           of SOME(SrcFunc mkFunc) => let
              (* need to map the function variable and then translate the definition *)
                val (resTy, paramTys) = SrcFV.ty f
                val f' = DstIR.Func.new (SrcFV.name f, cvtTy resTy, List.map cvtTy paramTys)
                val fdef = mkFunc(env, f')
                in
                  SrcFV.Tbl.insert fvMap (f, DstFunc f');
                  funcs := fdef :: !funcs;
                  f'
                end
            | SOME(DstFunc f') => f'
            | NONE => raise Fail("renameFV: unregistered function " ^ SrcIR.Func.toString f)
          (* end case *))

    fun insertNd (E{ndMap, ...}, id, nd) = Stamp.Tbl.insert ndMap (id, nd)

    fun findNd (E{ndMap, ...}) = Stamp.Tbl.find ndMap

  end
