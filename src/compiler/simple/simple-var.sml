(* simple-var.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure SimpleVar : sig

    type t

    datatype kind = datatype Var.kind

  (* define a new variable *)
    val new : string * kind * SimpleTypes.ty -> t

  (* make a copy of a variable (same name and type) *)
    val copy : t * kind -> t

  (* return the variable's name (as it appears in the source) *)
    val nameOf : t -> string

  (* return the variable's type scheme *)
    val typeOf : t -> SimpleTypes.ty

  (* return the variable's kind *)
    val kindOf : t -> kind

  (* return a unique string representation of the variable *)
    val uniqueNameOf : t -> string

  (* return true if a variable is a global (i.e., constant, input, or global) *)
    val isGlobal : t -> bool

  (* return true if variable has global scope; i.e., is either a basis variable or a global *)
    val hasGlobalScope : t -> bool

  (* define a property for variables *)
    val newProp : (t -> 'a) -> {
            clrFn  : t -> unit,
            getFn  : t -> 'a,
            peekFn : t -> 'a option,
            setFn  : t * 'a -> unit
          }
  (* define a boolean property for variables *)
    val newFlag : unit -> { getFn : t -> bool, setFn : t * bool -> unit }

  (* are two variables the same? *)
    val same : t * t -> bool

  (* compare two variables *)
    val compare : t * t -> order

  (* return a hash code for a variable *)
    val hash : t -> word

  (* return the string representation of the variable's kind *)
    val kindToString : kind -> string

    structure Map : ORD_MAP where type Key.ord_key = t
    structure Set : ORD_SET where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype kind = datatype Var.kind

    datatype t = V of {
        name : string,          (* print name of variable *)
        id : Stamp.t,           (* unique ID *)
        kind : kind,            (* variable kind *)
        ty : SimpleTypes.ty,    (* type *)
        props : PropList.holder (* property list *)
      }

    fun nameOf (V{name, ...}) = name
    fun typeOf (V{ty, ...}) = ty
    fun kindOf (V{kind, ...}) = kind
    fun uniqueNameOf (V{name, id, ...}) = name ^ Stamp.toString id

    fun new (name, kind, ty) = let
        (* convert single-quote characters to something that C++ will accept *)
          val name = CharVector.map (fn #"'" => #"p" | c => c) name
          val id = Stamp.new()
          in
            V{name=name, id=id, kind=kind, ty=ty, props=PropList.newHolder()}
          end

    fun copy (V{name, ty, ...}, kind) = new (name, kind, ty)

  (* return true if a variable is a global (i.e., constant, input, or global) *)
    fun isGlobal (V{kind, ...}) = (case kind
           of ConstVar => true
            | InputVar => true
            | GlobalVar => true
            | FunVar => raise Fail "unexpected FunVar"
            | _ => false
          (* end case *))

  (* return true if variable has global scope; i.e., is either a basis variable or a global *)
    fun hasGlobalScope (V{kind, ...}) = (case kind
           of BasisVar => true
            | ConstVar => true
            | InputVar => true
            | GlobalVar => true
            | FunVar => raise Fail "unexpected FunVar"
            | _ => false
          (* end case *))

    fun newProp initFn = PropList.newProp (fn (V{props, ...}) => props, initFn)
    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

    fun compare (V{id=a, ...}, V{id=b, ...}) = Stamp.compare(a, b)
    fun same (V{id=a, ...}, V{id=b, ...}) = Stamp.same(a, b)
    fun hash (V{id, ...}) = Stamp.hash id

    fun kindToString kind = (case kind
          of BasisVar => "basis variable"
           | ConstVar => "constant"
           | InputVar => "input variable"
           | GlobalVar => "global variable"
           | FunVar => raise Fail "unexpected FunVar"
           | FunParam => "function parameter"
           | StrandParam => "strand parameter"
           | StrandStateVar => "state variable"
           | StrandOutputVar => "strand output variable"
           | LocalVar => "local variable"
           | IterVar => "iteration variable"
          (* end case *))

    local
      structure V =
        struct
          type ord_key = t
          val compare = compare
        end
    in
    structure Map = RedBlackMapFn (V)
    structure Set = RedBlackSetFn (V)
    end

    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        val hashVal = hash
        val sameKey = same
      end)

  end
