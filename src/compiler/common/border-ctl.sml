(* border-ctl.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Image boarder controls.  I.e., what to do when an image index is out of bounds.
 *)

structure BorderCtl =
  struct

    datatype 'var t
      = Default of 'var         (* uses default value for out-of-bounds indices *)
      | Clamp                   (* clamp indices to image dimensions *)
      | Mirror                  (* reflect indices at border *)
      | Wrap                    (* wrap indices at border *)

    fun fmt v2s (Default x) = concat["Default(", v2s x, ")"]
      | fmt _ Clamp = "clamp"
      | fmt _ Mirror = "mirror"
      | fmt _ Wrap = "wrap"

    fun map f ctl = (case ctl
           of Default x => Default(f x)
            | Clamp => Clamp
            | Mirror => Mirror
            | Wrap => Wrap
          (* end case *))

  end
