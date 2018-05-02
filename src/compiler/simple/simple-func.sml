(* simple-func.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure SimpleFunc : sig

    type t

    type func_ty = SimpleTypes.ty * SimpleTypes.ty list

    val new : string * SimpleTypes.ty * SimpleTypes.ty list -> t

  (* return the function's name (as it appears in the source) *)
    val nameOf : t -> string

  (* return the function's type scheme *)
    val typeOf : t -> func_ty

  (* return the function's ressult type *)
    val resultTypeOf : t -> SimpleTypes.ty

  (* return a unique string representation of the function *)
    val uniqueNameOf : t -> string

  (* increment the use count *)
    val use : t -> t
  (* decrement the use count and return the current count *)
    val decCnt : t -> int
  (* return the use count *)
    val useCount : t -> int

  (* define a property for functions *)
    val newProp : (t -> 'a) -> {
            clrFn  : t -> unit,
            getFn  : t -> 'a,
            peekFn : t -> 'a option,
            setFn  : t * 'a -> unit
          }
  (* define a boolean property for functions *)
    val newFlag : unit -> { getFn : t -> bool, setFn : t * bool -> unit }

  (* are two functions the same? *)
    val same : t * t -> bool

  (* compare two functions *)
    val compare : t * t -> order

  (* return a hash code for a function *)
    val hash : t -> word

    structure Map : ORD_MAP where type Key.ord_key = t
    structure Set : ORD_SET where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

    val tyToString : func_ty -> string

  end = struct

    type func_ty = SimpleTypes.ty * SimpleTypes.ty list

    datatype t = F of {
        name : string,                  (* print name of variable *)
        id : Stamp.t,                   (* unique ID *)
        ty : SimpleTypes.ty,            (* return type *)
        paramTys : SimpleTypes.ty list, (* parameter types *)
        useCnt : int ref,               (* number of uses of the function *)
        props : PropList.holder         (* property list *)
      }

    fun new (name, resTy, paramTys) = let
          val id = Stamp.new()
          in
            F{
                name=name, id=id,
                ty=resTy, paramTys=paramTys,
                useCnt=ref 0, props=PropList.newHolder()
              }
          end

    fun nameOf (F{name, ...}) = name
    fun typeOf (F{ty, paramTys, ...}) = (ty, paramTys)
    fun resultTypeOf (F{ty, ...}) = ty
    fun uniqueNameOf (F{name, id, ...}) = name ^ Stamp.toString id

  (* increment the use count *)
    fun use (f as F{useCnt, ...}) = (useCnt := !useCnt + 1; f)
  (* decrement the use count *)
    fun decCnt (F{useCnt, ...}) = let val n = !useCnt - 1 in useCnt := n; n end
  (* return the use count *)
    fun useCount (F{useCnt, ...}) = !useCnt

    fun newProp initFn = PropList.newProp (fn (F{props, ...}) => props, initFn)
    fun newFlag () = PropList.newFlag (fn (F{props, ...}) => props)

    fun compare (F{id=a, ...}, F{id=b, ...}) = Stamp.compare(a, b)
    fun same (F{id=a, ...}, F{id=b, ...}) = Stamp.same(a, b)
    fun hash (F{id, ...}) = Stamp.hash id

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

    fun tyToString (resTy, paramTys) = let
          fun tysToString [] = "()"
            | tysToString [ty] = SimpleTypes.toString ty
            | tysToString tys = String.concat[
                  "(", String.concatWithMap " * " SimpleTypes.toString tys, ")"
                ]
          in
            String.concat[tysToString paramTys, " -> ", SimpleTypes.toString resTy]
          end

  end
