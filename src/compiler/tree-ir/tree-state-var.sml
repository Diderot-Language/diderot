(* tree-state-var.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TreeStateVar : sig

    type t = TreeIR.state_var

    val new : {
            name : string,              (* variable name *)
            ty : TreeTypes.t,           (* variable's TreeIR type *)
            apiTy : APITypes.t,         (* specifies the external type of the variable *)
            output : bool,              (* true for output variables *)
            varying : bool,             (* does the variable get updated? *)
            shared : bool               (* is the variable accessed by other strands? *)
          } -> t

    val name : t -> string
    val qname : t -> string             (* qualified name "sv_" ^ name *)

    val same : t * t -> bool

    val ty : t -> TreeTypes.t

    val apiTy : t -> APITypes.t

    val isOutput : t -> bool

  (* is a variable assigned to in a method *)
    val isVarying : t -> bool

  (* is a variable accessed by other strands *)
    val isShared : t -> bool

  (* should a state variable be included in the shared-variable struct? *)
    val inSharedStruct: t -> bool

    val toString : t -> string

  end = struct

    datatype t = datatype TreeIR.state_var

    fun new {name, ty, output, varying, shared, apiTy} = SV{
            name = name, ty = ty, xty = apiTy,
            output = output, varying = varying, shared = shared
          }

    fun same (SV{name=a, ...}, SV{name=b, ...}) = (a = b)

    fun name (SV{name, ...}) = name
    fun qname (SV{name, ...}) = "sv_" ^ name

    fun ty (SV{ty, ...}) = ty

    fun apiTy (SV{xty, ...}) = xty

    fun isOutput (SV{output, ...}) = output

    fun isVarying (SV{varying, ...}) = varying

    fun isShared (SV{shared, ...}) = shared

    fun inSharedStruct sv = isShared sv andalso isVarying sv

    fun toString (SV{name, ...}) = name

  end
