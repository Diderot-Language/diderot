(* int-lit.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *)

structure IntLit :> sig

    type t = IntInf.int

  (* set the precision of tje Diderot "int" type to be 64-bits *)
    val setDiderotLongInt : unit -> unit

    val fromInt : int -> t

  (* minimum and maximum values that are representable as Diderot ints *)
    val minInt : unit -> t
    val maxInt : unit -> t

  (* arithmetic *)
    val add : t * t -> t
    val sub : t * t -> t
    val mul : t * t -> t
    val divide : t * t -> t
    val modulo : t * t -> t
    val neg : t -> t

  (* is an integer in range for the Diderot "int" type? *)
    val inRange : t -> bool

  (* equality, comparisons, and hashing functions *)
    val same : (t * t) -> bool
    val compare : (t * t) -> order
    val hash : t -> word

    val toString : t -> string

  end = struct

    type t = IntInf.int

    val fromInt = IntInf.fromInt

  (* set the precision of tje Diderot "int" type to be 64-bits *)
    val isLongInt = ref false
    fun setDiderotLongInt () = isLongInt := true

  (* minimum and maximum values that are representable as Diderot ints *)
    fun minInt () : t = if !isLongInt then ~0x8000000000000000 else ~0x80000000
    fun maxInt () : t = if !isLongInt then  0x7fffffffffffffff else  0x7fffffff

  (* arithmetic *)
    val add = IntInf.+
    val sub = IntInf.-
    val mul = IntInf.*
    val divide = IntInf.quot
    val modulo = IntInf.rem
    val neg = IntInf.~

  (* range checks *)
    fun inRange (n : IntInf.int) = if !isLongInt
          then (~0x8000000000000000 <= n) andalso (n <= 0x7fffffffffffffff)
          else (~0x80000000 <= n) andalso (n <= 0x7fffffff)

  (* equality, comparisons, and hashing functions *)
    fun same (a : IntInf.int, b) = (a = b)
    val compare = IntInf.compare
    fun hash i = Word.fromInt(IntInf.toInt(IntInf.andb(i, 0xfffffff)))

    fun toString i = if (i < 0)
          then "-" ^ IntInf.toString(IntInf.~ i)
          else IntInf.toString i

  end
