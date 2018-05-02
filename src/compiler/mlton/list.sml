(* list.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

signature LIST_EXT =
  sig

    include LIST

    val appi : (int * 'a -> unit) -> 'a list -> unit
    val mapi : (int * 'a -> 'b) -> 'a list -> 'b list
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val findi : (int * 'a -> bool) -> 'a list -> (int * 'a) option

    val revMap : ('a -> 'b) -> 'a list -> 'b list

  end

structure ListExt : LIST_EXT =
  struct

    open List

    fun appi f xs = let
          fun appf ([], _) = ()
            | appf (x::xs, i) = (f (i, x); appf (xs, i+1))
          in
            appf (xs, 0)
          end

    fun mapi f xs = let
          fun mapf ([], _, ys) = List.rev ys
            | mapf (x::xs, i, ys) = mapf (xs, i+1, f(i, x)::ys)
          in
            mapf (xs, 0, [])
          end

    fun foldli f init xs = let
          fun foldf ([], _, acc) = acc
            | foldf (x::xs, i, acc) = foldf (xs, i+1, f(i, x, acc))
          in
            foldf (xs, 0, init)
          end

    fun findi pred l = let
          fun find (_, []) = NONE
            | find (i, x::xs) = if pred(i, x) then SOME(i, x) else find(i+1, xs)
          in
            find (0, l)
          end

    fun revMap f l = let
          fun mapf (x::xs, ys) = mapf (xs, f x :: ys)
            | mapf ([], ys) = ys
          in
            mapf (l, [])
          end

  end
