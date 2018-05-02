(* codegen-env.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure CodeGenEnv : sig

    type t

    val new : {
            spec : TargetSpec.t,        (* target spec *)
            world : CLang.var,          (* variable for accessing the world *)
            global : CLang.var,         (* variable for accessing globals *)
            selfLocal : CLang.var,      (* variable for accessing local strand state *)
            selfIn : CLang.var,         (* variable for accessing shared input strand state *)
            selfOut : CLang.var         (* variable for accessing shared output strand state *)
          } -> t

    val empty : TargetSpec.t -> t

  (* code generation callbacks *)
    type map_reduce_cb =
          t * TreeIR.map_reduce list * TreeVar.t * CLang.stm list -> t * CLang.stm list
    val setMapReduceCB : t * map_reduce_cb -> unit
    val mapReduceCB : map_reduce_cb

  (* get the target spec record from the environment *)
    val target : t -> TargetSpec.t

    val lookup : t * TreeVar.t -> CLang.var

    val insert : t * TreeVar.t * CLang.var -> t

  (* lookup the bindings for accessing the globals and strand state *)
    val global : t -> CLang.var
    val selfLocal : t -> CLang.var
    val selfIn : t -> CLang.var
    val selfOut : t -> CLang.var
    val world : t -> CLang.var

  (* get CLang versions of standard Diderot types *)
    val realTy    : t -> CLang.ty
    val intTy     : t -> CLang.ty
    val boolTy    : t -> CLang.ty
    val rawRealTy : t -> RawTypes.t
    val rawIntTy  : t -> RawTypes.t

  (* dump to stdout for debugging purposes *)
    val dump : string * t -> unit

  end = struct

    structure V = TreeVar
    structure VMap = TreeVar.Map
    structure CL = CLang

    datatype t = Env of {
        vmap : CL.var VMap.map,
        spec : TargetSpec.t,
        mapReduceCB : map_reduce_cb option ref
      }

    withtype map_reduce_cb =
        t * TreeIR.map_reduce list * TreeVar.t * CLang.stm list -> t * CLang.stm list

  (* dump to stdout for debugging purposes *)
    fun dump (msg, Env{vmap, ...}) = let
          fun prBinding (x, x') = print(concat["  ", TreeVar.toString x, " --> ", x', "\n"])
          in
            print(concat["***** ", msg, " *****\n"]);
            VMap.appi prBinding vmap;
            print "*****\n"
          end

    fun new {spec, world, global, selfLocal, selfIn, selfOut} = Env{
            vmap = List.foldl VMap.insert' VMap.empty [
                (PseudoVars.world, world),
                (PseudoVars.global, global),
                (PseudoVars.selfLocal, selfLocal),
                (PseudoVars.selfIn, selfIn),
                (PseudoVars.selfOut, selfOut)
              ],
            spec = spec,
            mapReduceCB = ref NONE
          }

    fun empty spec = Env{
            vmap = VMap.empty,
            spec = spec,
            mapReduceCB = ref NONE
          }

    fun setMapReduceCB (Env{mapReduceCB, ...}, cbFun) = mapReduceCB := SOME cbFun
    fun mapReduceCB (env as Env{mapReduceCB, ...}, mrs, src, stms) = (case !mapReduceCB
           of SOME cbFun => cbFun (env, mrs, src, stms)
            | NONE => raise Fail "mapReduceCB: no callback set"
          (* end case *))

    fun target (Env{spec, ...}) = spec

    fun lookup (Env{vmap, ...}, x) = (case VMap.find (vmap, x)
           of SOME x' => x'
            | NONE => raise Fail(concat["lookup(_, ", V.name x, ")"])
          (* end case *))
(* +DEBUG *)
val lookup = (fn (env, x) => lookup(env, x) handle ex => (dump("ENV", env); raise ex))
(* -DEBUG *)

    fun insert (Env{vmap, spec, mapReduceCB}, x, x') =
          Env{vmap = VMap.insert(vmap, x, x'), spec = spec, mapReduceCB = mapReduceCB}

    fun global env = lookup(env, PseudoVars.global)
    fun selfLocal env = lookup(env, PseudoVars.selfLocal)
    fun selfIn env = lookup(env, PseudoVars.selfIn)
    fun selfOut env = lookup(env, PseudoVars.selfOut)
    fun world env = lookup(env, PseudoVars.world)

    fun realTy (Env{spec, ...}) = if (#double spec) then CL.double else CL.float
    fun intTy (Env{spec, ...}) = if (#longint spec) then CL.int64 else CL.int32
    fun boolTy _ = CL.T_Named SizeOf.c_bool
    fun rawRealTy (Env{spec, ...}) = if (#double spec)
          then RawTypes.RT_Double
          else RawTypes.RT_Float
    fun rawIntTy (Env{spec, ...}) = if (#longint spec)
          then RawTypes.RT_Int64
          else RawTypes.RT_Int32

(* +DEBUG *)
val mapReduceCB = fn arg => (mapReduceCB arg) handle ex => (dump("ENV", #1 arg); raise ex)
(* -DEBUG *)

  end
