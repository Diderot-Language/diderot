(* vector-layout.sml
 *
 * Functions for splitting LowIR vectors into TreeIR composite vectors.
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* hardware support:
 * SSE          128 bits
 * AVX          256 bits (?)
 * AVX2         256 bits
 * AVX512       512 bits
 *)

structure VectorLayout : sig

  (* the layout of a vector onto target-supported vectors. *)
    type t = {
        wid : int,              (* total width of the vector *)
        padded : bool,          (* true if sum of pieces > wid; pad will be in last piece *)
        pieces : int list       (* list of pieces; these will all be supported vector widths *)
      }

    val toString : t -> string

    val same : t * t -> bool

    val hash : t -> word

  (* layout for real (i.e., 1-wide vector) *)
    val realLayout : t

  (* given a list of native vector sizes in ascending order, return a function for
   * mapping vector widths to TreeTypes.VecTy values.
   *)
    val layout : int list -> int -> t

  (* layout for a scalar target *)
    val scalar : int -> t

  (* `gccVectorSizes realIsDouble`
   * returns a list of vector sizes that are valid for GCC vector extensions assuming
   * a maximum width of 512 bits (AVX512).  The argument should be true if a Diderot
   * real value is double precision.
   *)
    val gccVectorSizes : bool -> int list

  (* given a list of native vector sizes in ascending order, return a function for
   * testing the validity of a layout.
   *)
    val valid : int list -> t -> bool

  end = struct

  (* the layout of a vector onto target-supported vectors. *)
    type t = {
        wid : int,              (* total width of the vector *)
        padded : bool,          (* true if sum of pieces > wid; pad will be in last piece *)
        pieces : int list       (* list of pieces; these will all be supported vector widths *)
      }

  (* layout for real (i.e., 1-wide vector) *)
    val realLayout = {wid = 1, padded = false, pieces = [1]}

    fun toString {wid, padded, pieces} = String.concat [
            Int.toString wid, "{", String.concatWithMap "," Int.toString pieces, "}"
          ]

  (* we assume that the width uniquely determines the rest of the layout *)
    fun same (l1 : t, l2 : t) = (#wid l1 = #wid l2)

    fun hash (l : t) = Word.fromInt(#wid l)

    fun layout (sizes : int list) = let
        (* find smallest supported vector width that is >= n; return the largest
         * size if n is bigger than the largest supported vector.
         *)
          fun find (n, []) = raise Fail "impossible"
            | find (n, [sz]) = sz
            | find (n, sz::szs) = if (n <= sz) then sz else find(n, szs)
        (* split n into pieces *)
          fun split (n, pieces) = let
                val sz = find (n, sizes)
                val pieces = sz :: pieces
                val m = n - sz
                in
                  if (m = 0)
                    then (false, rev pieces)
                  else if (m < 0)
                    then (true, rev pieces)
                    else split (m, pieces)
                end
          in
            fn n => let
                val (padded, pieces) = split (n, [])
                in
                  {wid=n, padded=padded, pieces=pieces}
                end
          end

    fun scalar w = {wid=w, padded=false, pieces=List.tabulate(w, fn _ => 1)}

    local
      val log2MaxWidth = 4      (* maxWidth == log2(512/32) *)
      fun twoToThe n = Word.toIntX(Word.<<(0w1, Word.fromInt n))
    in
    fun gccVectorSizes false = List.tabulate (log2MaxWidth, twoToThe)
      | gccVectorSizes true = List.tabulate (log2MaxWidth-1, twoToThe)
    end (* local *)

    fun valid (sizes : int list) = let
          fun validSize sz = List.exists (fn sz' => (sz = sz')) sizes
          fun check {wid, padded, pieces} = let
                fun chkPieces ([], totWid) =
                      (wid = totWid) orelse (padded andalso (wid < totWid))
                  | chkPieces (p::ps, totWid) =
                      validSize p andalso chkPieces (ps, p+totWid)
                in
                  (wid > 0) andalso chkPieces (pieces, 0)
                end
          in
            check
          end

  end
