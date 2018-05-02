(* tree-var.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TreeVar : sig

    type t = TreeIR.var

    val new : string * TreeTypes.t -> t

    val name : t -> string

    val toString : t -> string

    val ty : t -> TreeTypes.t

    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t

  end = struct

    datatype t = datatype TreeIR.var

    fun new (name, ty) = V{
            name = name,
            id = Stamp.new(),
            ty = ty
          }

    fun name (V{name, ...}) = name

    fun toString (V{name, id, ...}) = concat[name, "$", Stamp.toString id]

    fun ty (V{ty, ...}) = ty

    local
      structure VarOrd =
        struct
          type ord_key = t
          fun compare (V{id=a, ...}, V{id=b, ...}) = Stamp.compare(a, b)
        end
    in
    structure Set = RedBlackSetFn (VarOrd)
    structure Map = RedBlackMapFn (VarOrd)
    end (* local *)

  end
