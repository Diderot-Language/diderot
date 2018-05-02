(* gen-globals.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *)

structure GenGlobals : sig

  (* generate the struct declaration for the global variables (if present) *)
    val gen : {
            env : CodeGenEnv.t,
            consts : TreeIR.global_var list,
            inputs : TreeIR.input list,
            globals : TreeIR.global_var list
          } -> CLang.decl list

  end = struct

    structure GV = TreeGlobalVar
    structure CL = CLang
    structure RN = CxxNames
    structure Env = CodeGenEnv
    structure ToCxx = TreeToCxx

  (* make a field declaration for a global *)
    fun mkField env gv = CL.D_Var([], ToCxx.trType(env, GV.ty gv), [], GV.qname gv, NONE)

  (* global images and dynamic sequences require constructor/destructor functions *)
    fun needsConstr gv = (case GV.ty gv
           of TreeTypes.ImageTy _ => true
            | TreeTypes.SeqTy(_, NONE) => true
            | _ => false
          (* end case *))

  (* generate a statement to free the image data of an image *)
    fun deleteGlobal (gv, stms) = (case GV.ty gv
           of TreeTypes.ImageTy _ =>
                CL.mkExpStm(
                  CL.mkDispatch(CL.mkIndirect(CL.mkVar "this", GV.qname gv), "unregister_global", []))
                :: stms
            | _ => stms
          (* end case *))

    fun gen {env, consts, inputs, globals} =
          if #hasGlobals(Env.target env) orelse #hasConsts(Env.target env)
            then let
            (* collect all globals *)
              val gvs = List.foldr (fn (inp, gvs) => Inputs.varOf inp :: gvs) globals inputs
              val gvs = List.foldr (op ::) gvs consts
            (* convert to fields *)
              val fields = List.map (mkField env) gvs
            (* if there is a global image or dynamic sequence, then we need both a constructor
             * and destuctor.
             *)
              val fields = (case List.filter needsConstr gvs
                     of [] => fields
                      | gvs => let
                          val body = CL.mkBlock(List.foldr deleteGlobal [] gvs)
                          val ctor = CL.D_Constr([], [], "globals", [], NONE)
                          val dtor = CL.D_Destr([], [], "globals", SOME body)
                          in
(* FIXME: include the ctor once we have changed the init_consts function into the
 * globals::globals() constructor.
 *)
                            fields @ [dtor]
                          end
                    (* end case *))
              in
                [CL.D_ClassDef{
                    name = "globals", args = NONE, from = NONE,
                    public = fields, protected = [], private = []
                  }]
              end
            else []

  end
