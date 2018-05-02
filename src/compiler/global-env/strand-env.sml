(* strand-env.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure StrandEnv : sig

  (* the environment that describes the external view of a strand *)
    type t

  (* create a new strand environment from its name and list of parameters *)
    val new : Atom.atom * AST.var list -> t

  (* return the name of the strand *)
    val strandName : t -> Atom.atom

  (* return the parameter types *)
    val paramTys : t -> Types.ty list

  (* lookup a strand state variable *)
    val findStateVar : t * Atom.atom -> AST.var option

  (* insert a strand state variable into the given strand environment *)
    val insertStateVar : t * Atom.atom * AST.var -> unit

  (* lookup the "pos" variable that is required for spatial queries *)
    val findPosVar : t -> AST.var option

  (* record the spatial dimension for this strand (i.e., the dimension of the "pos" variable) *)
    val recordSpaceDim : t * int -> unit

  (* get the spatial dimension for this strand *)
    val getSpaceDim : t -> int option

  end = struct

    structure ATbl = AtomTable

    datatype t = SE of {
        name : Atom.atom,                       (* the strand's name *)
        paramTys : Types.ty list,               (* the types of the strand's parameters *)
        env : AST.var ATbl.hash_table,          (* the strand's state variables *)
        spatialDim : int option ref             (* the dimension of the space that the strand *)
                                                (* is positioned in (for queries) *)
      }

    fun new (name, params) = SE{
          name = name,
          paramTys = List.map Var.monoTypeOf params,
          env = ATbl.mkTable(32, Fail "strand-env"),
          spatialDim = ref NONE
        }

    fun strandName (SE{name, ...}) = name

    fun paramTys (SE{paramTys=tys, ...}) = tys

    fun findStateVar (SE{env, ...}, x) = ATbl.find env x

    fun insertStateVar (SE{env, ...}, x, x') = ATbl.insert env (x, x')

    fun findPosVar (SE{env, ...}) = ATbl.find env (Atom.atom "pos")

    fun recordSpaceDim (SE{spatialDim, ...}, d) = spatialDim := SOME d

    fun getSpaceDim (SE{spatialDim, ...}) = !spatialDim

  end
