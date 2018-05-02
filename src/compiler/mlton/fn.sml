(* fn.sig
 *
 * ============================================================================
 * Copyright (c) 2015 John Reppy (http://cs.uchicago.edu/~jhr)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 * ============================================================================
 *
 * Reference code for SML Basis Library Proposal 2015-005.
 *)

signature FN =
  sig

    val id       : 'a -> 'a
    val const    : 'a -> 'b -> 'a
    val apply    : ('a -> 'b) * 'a -> 'b
    val o        : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val curry    : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
    val uncurry  : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
    val flip     : ('a * 'b -> 'c) -> ('b * 'a -> 'c)
    val repeat   : int -> ('a -> 'a) -> ('a -> 'a)
    val equal    : ''a -> ''a -> bool
    val notEqual : ''a -> ''a -> bool

  end

structure Fn : FN =
  struct

    fun id x = x

    fun const x y = x

    fun apply (f, x) = f x

    val op o = General.o

    fun curry f x y = f(x, y)

    fun uncurry f (x, y) = f x y

    fun flip f (x, y) = f (y, x)

    fun repeat n = if (n < 0)
          then raise Domain
          else (fn f => let
            fun repeatF (0, x) = x
              | repeatF (n, x) = repeatF (n-1, f x)
            in
              fn x => repeatF (n, x)
            end)

    fun equal a b = (a = b)

    fun notEqual a b = (a <> b)

  end
