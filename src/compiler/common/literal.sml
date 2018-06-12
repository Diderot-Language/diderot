(* literal.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Literal values.
 *)

structure Literal : sig

    datatype t
      = Int of IntLit.t                 (* integer literals *)
      | Real of RealLit.t               (* real literals *)
      | String of string                (* strings *)
      | Bool of bool                    (* booleans *)
      | InVar of string                 (* field function input variable *)

    val toString : t -> string

    val same : (t * t) -> bool
    val compare : (t * t) -> order
    val hash : t -> word

  (* some standard constants *)
    val trueLit : t
    val falseLit : t

  (* integer literals from ML ints *)
    val intLit : int -> t

    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t
      = Int of IntLit.t                 (* integer literals *)
      | Real of RealLit.t               (* real literals *)
      | String of string                (* strings *)
      | Bool of bool                    (* booleans *)
      | InVar of string 
      
    fun toString (Int i) = IntLit.toString i
      | toString (Real flt) = RealLit.toString flt
      | toString (String s) = concat["\"", String.toCString s, "\""]
      | toString (Bool true) = "true"
      | toString (Bool false) = "false"
      | toString (InVar s) = concat["\"", String.toCString s, "\""]

    fun same (Int i1, Int i2) = IntLit.same(i1, i2)
      | same (Real f1, Real f2) = RealLit.same(f1, f2)
      | same (String s1, String s2) = (s1 = s2)
      | same (Bool b1, Bool b2) = (b1 = b2)
      | same (InVar s1, InVar s2) = (s1 = s2)
      | same _ = false

    fun compare (Int i1, Int i2) = IntLit.compare(i1, i2)
      | compare (Real f1, Real f2) = RealLit.compare(f1, f2)
      | compare (String s1, String s2) = String.compare(s1, s2)
      | compare (Bool false, Bool true) = LESS
      | compare (Bool true, Bool false) = GREATER
      | compare (Bool _, Bool _) = EQUAL
      | compare (Int _, _) = LESS
      | compare (_, Int _) = GREATER
      | compare (Real _, _) = LESS
      | compare (_, Real _) = GREATER
      | compare (String _, _) = LESS
      | compare (_, String _) = GREATER
      | compare (InVar s1, InVar s2) = String.compare(s1, s2)
      
  (* for hash codes, use the low-order 4 bits for a type code *)
    local
      val intCd = 0w5
      val floatCd = 0w7
      val stringCd = 0w13
      val boolCd = 0w17
      fun h (hash, base) = Word.<<(hash, 0w4) + base
    in
    fun hash (Int i) = h(IntLit.hash i, intCd)
      | hash (Real f) = h(RealLit.hash f, floatCd)
      | hash (String s) = h(HashString.hashString s, stringCd)
      | hash (Bool false) = h(0w1, boolCd)
      | hash (Bool true) = h(0w3, boolCd)
      | hash (InVar s) = h(HashString.hashString s, stringCd)
      
    end (* local *)

  (* some standard constants *)
    val trueLit = Bool true
    val falseLit = Bool false

    fun intLit i = Int(IntInf.fromInt i)

    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        val hashVal = hash
        val sameKey = same
      end)

  end
