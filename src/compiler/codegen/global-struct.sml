(* global-struct.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure GlobalStruct : sig

    val genGlobals : {
            cvtTy : TreeTypes.t -> CLang.ty,    (* convert to target type *)
            name : string,                      (* the struct name *)
            fields : TreeIR.global_var list     (* the globals that define the fields *)
          } -> CLang.decl

  end = struct

    fun genGlobals {cvtTy, name, fields} = let
          fun mkField (TreeIR.GV{name, ty, ...}) = (cvtTy ty, name)
          in
            CLang.D_StructDef(SOME name, List.map mkField fields, NONE)
          end

  end
