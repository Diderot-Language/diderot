(* avail-rhs.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

(* tracking available MidIR rhs expressions *)
structure AvailRHS = AvailRHSFn (
    val phase = "high-to-mid"
    structure IR = MidIR)
