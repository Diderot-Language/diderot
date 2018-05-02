(* tree-func.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TreeFunc : sig

    type t

  (* new function variable *)
    val new : string * TreeTypes.t * TreeTypes.t list * bool * bool * bool -> t

    val name : t -> string
    val qname : t -> string             (* qualified name "fn_" ^ name *)

    val toString : t -> string

  (* return type and parameter types *)
    val ty : t -> TreeTypes.t * TreeTypes.t list

  (* return true if the function is used as the map part of a global map-reduce *)
    val isMapFun : t -> bool

  (* return true if the function needs a pointer to the world *)
    val needsWorld : t -> bool

  (* return true if the function needs a pointer to the globals *)
    val hasGlobals : t -> bool

  end = struct

    datatype t = datatype TreeIR.func

    fun new (name, ty, paramTys, nW, nG, isMap) = FV{
            name = name,
            id = Stamp.new(),
            ty = ty,
            paramTys = paramTys,
            isMapFn = isMap,
            needsW = nW orelse isMap,
            hasG = nG
          }

    fun name (FV{name, ...}) = name

    fun qname (FV{name, id, isMapFn=true, ...}) = concat["mapfn_", name, "_", Stamp.toString id]
      | qname (FV{name, ...}) = "fn_" ^ name

    fun toString (FV{name, id, ...}) = concat[name, "$", Stamp.toString id]

    fun ty (FV{ty, paramTys, ...}) = (ty, paramTys)

    fun isMapFun (FV{isMapFn, ...}) = isMapFn
    fun needsWorld (FV{needsW, ...}) = needsW
    fun hasGlobals (FV{hasG, ...}) = hasG

  end
