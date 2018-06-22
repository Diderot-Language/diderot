(* var.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure Var : sig

    type t

  (* AST variable kinds *)
    datatype kind
      = BasisVar                (* predefined basis variable (see basis-vars.sml) *)
      | ConstVar                (* 'const' variable *)
      | InputVar                (* 'input' variable *)
      | GlobalVar               (* variable defined at global scope *)
      | FunVar                  (* user-defined function *)
      | FunParam                (* parameter to function *)
      | StrandParam             (* parameter to strand definition *)
      | StrandStateVar          (* strand state variable *)
      | StrandOutputVar         (* strand output variable *)
      | LocalVar                (* local variable in method *)
      | IterVar                 (* iteration variable *)

  (* define a new monomorphic variable *)
    val new : Atom.atom * Error.span * kind * Types.ty -> t

  (* define a new polymorphic variable; this function is used to define Basis variables,
   * but not user-variables, so we omit the location and kind information.
   *)
    val newBasis : Atom.atom * Types.scheme -> t

  (* make a copy of a variable (same name and type) *)
    val copy : t -> t

  (* return the variable's atom name (as it appears in the source) *)
    val atomNameOf : t -> Atom.atom
    
  (* return the variable's name (as it appears in the source) *)
    val nameOf : t -> string

  (* return the variable's type scheme *)
    val typeOf : t -> Types.scheme

  (* return the variable's type; this function raise Fail if the variable is polymorphic *)
    val monoTypeOf : t -> Types.ty

  (* return the variable's kind *)
    val kindOf : t -> kind

  (* return a unique string representation of the variable *)
    val uniqueNameOf : t -> string

  (* return the variable's binding location; this function returns UNKNOWN for basis variables *)
    val locationOf : t -> Error.span

  (* return true if the variable is a primitive variable (i.e., defined in the Basis) *)
    val isPrim : t -> bool

  (* return true if the variable is a compile-time constant *)
    val isConst : t -> bool

  (* return true if the variable is an output variable *)
    val isOutput : t -> bool

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
    val kindToString : t -> string

    structure Map : ORD_MAP where type Key.ord_key = t
    structure Set : ORD_SET where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

  (* AST variable kinds *)
    datatype kind
      = BasisVar                (* predefined basis variable (see basis-vars.sml) *)
      | ConstVar                (* 'const' variable *)
      | InputVar                (* 'input' variable *)
      | GlobalVar               (* variable defined at global scope *)
      | FunVar                  (* user-defined function *)
      | FunParam                (* parameter to function *)
      | StrandParam             (* parameter to strand definition *)
      | StrandStateVar          (* strand state variable *)
      | StrandOutputVar         (* strand output variable *)
      | LocalVar                (* local variable in method *)
      | IterVar                 (* iteration variable *)

    datatype t = V of {
        atomname : Atom.atom,
        name : string,          (* print name of variable *)
        id : Stamp.t,           (* unique ID *)
        loc : Error.span,       (* the location where this variable is bound *)
        kind : kind,            (* variable kind *)
        ty : Types.scheme,      (* type scheme *)
        props : PropList.holder (* property list *)
      }

    fun atomNameOf (V{atomname, ...}) = atomname
    fun nameOf (V{name, ...}) = name
    fun typeOf (V{ty, ...}) = ty
    fun monoTypeOf (V{ty=([], ty), ...}) = ty
      | monoTypeOf (V{name, ...}) = raise Fail(name ^ " is not monomorphic")
    fun kindOf (V{kind, ...}) = kind
    fun uniqueNameOf (V{name, id, ...}) = name ^ Stamp.toString id
    fun locationOf (V{loc, ...}) = loc

    fun newPoly (name, loc, kind, scheme) = let
          val id = Stamp.new()
          in
            V{atomname = name, name=Atom.toString name, id=id, loc=loc, kind=kind, ty=scheme, props=PropList.newHolder()}
          end

    fun newBasis (name, scheme) = newPoly (name, (0, 0), BasisVar, scheme)

    fun new (name, loc, kind, ty) = newPoly (name, loc, kind, ([], ty))

    fun copy (V{atomname, name, kind, ty, ...}) =
          V{atomname=atomname, name=name, id=Stamp.new(), loc=(0, 0), kind=kind, ty=ty, props=PropList.newHolder()}

    fun isPrim (V{kind = BasisVar, ...}) = true
      | isPrim _ = false

  (* return true if the variable is a compile-time constant *)
    fun isConst (V{kind=ConstVar, ...}) = true
      | isConst _ = false

  (* return true if the variable is an output variable *)
    fun isOutput (V{kind=StrandOutputVar, ...}) = true
      | isOutput _ = false

    fun newProp initFn = PropList.newProp (fn (V{props, ...}) => props, initFn)
    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

    fun compare (V{id=a, ...}, V{id=b, ...}) = Stamp.compare(a, b)
    fun same (V{id=a, ...}, V{id=b, ...}) = Stamp.same(a, b)
    fun hash (V{id, ...}) = Stamp.hash id

    fun kindToString (V{kind, ...}) = (case kind
          of BasisVar => "basis variable"
           | ConstVar => "constant"
           | InputVar => "input variable"
           | GlobalVar => "global variable"
           | FunVar => "function"
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
