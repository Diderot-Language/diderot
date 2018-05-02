(* tree-global-var.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TreeGlobalVar : sig

    type t = TreeIR.global_var

    val new : {
            name : string,              (* the variable name *)
            ty : TreeTypes.t,           (* *)
            input : bool,               (* true for inputs *)
            output : bool,              (* true for global outputs *)
            varying : bool,             (* true if this variable is modified in the global update
                                         * phase during execution
                                         *)
            apiTy : APITypes.t option   (* specifies external type for inputs and outputs *)
          } -> t

    val name : t -> string
    val qname : t -> string             (* qualified name "gv_" ^ name *)

    val same : t * t -> bool

    val ty : t -> TreeTypes.t

    val apiTy : t -> APITypes.t

    val isInput : t -> bool
    val isOutput : t -> bool
    val isVarying : t -> bool

    val toString : t -> string

  end = struct

    datatype t = datatype TreeIR.global_var

    fun new {name, ty, input, output, varying, apiTy} = GV{
            name = name,
            ty = ty,
            xty = apiTy,
            input = input,
            output = output,
            varying = varying
          }

    fun same (GV{name=a, ...}, GV{name=b, ...}) = (a = b)

    fun name (GV{name, ...}) = name
    fun qname (GV{name, ...}) = "gv_" ^ name

    fun ty (GV{ty, ...}) = ty

    fun apiTy (GV{xty = SOME ty, ...}) = ty
      | apiTy (GV{name, ...}) = raise Fail("missing API Type for global " ^ name)

    fun isInput (GV{input, ...}) = input
    fun isOutput (GV{output, ...}) = output
    fun isVarying (GV{varying, ...}) = varying

    fun toString (GV{name, ...}) = "globals." ^ name

  end
