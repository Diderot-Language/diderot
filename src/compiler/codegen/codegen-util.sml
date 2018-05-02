(* codegen-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure CodeGenUtil : sig

  (* generate unique variable names *)
    val tmpVar : unit -> CLang.var
    val freshVar : string -> CLang.var

  (* is an expression "simple" (i.e., a variable or literal)? *)
    val isSimple : TreeIR.exp -> bool

  (* translate an API type to a C type suitable for external use *)
(*
    val trAPIType : CodeGenEnv.t * APITypes.t -> CLang.ty
*)

    datatype create_loop
      = ForLoop of TreeVar.t * TreeIR.exp * TreeIR.exp  (* loop over integer range *)
      | ForeachLoop of TreeVar.t * TreeIR.exp           (* loop over dynamic sequence *)

  (* decompose strand creation *)
    val decomposeCreate : TreeIR.create -> {
            dim : int option,           (* grid dimension; NONE for collections *)
            locals : TreeVar.t list,    (* local variables from create code *)
            prefix : TreeIR.stm list,   (* assignments to the locals *)
            loops : create_loop list,   (* loop nest *)
            body : {                    (* decomposed strand creation *)
                locals : TreeVar.t list,
                stms : TreeIR.stm list,
                newStm : Atom.atom * TreeIR.exp list
              }
          }

  end = struct

    structure IR = TreeIR
    structure CL = CLang
    structure Env = CodeGenEnv

  (* generate new variables *)
    local
      val count = ref 0
    in
    fun freshVar prefix = let
          val n = !count
          in
            count := n+1;
            concat[prefix, "_", Int.toString n]
          end
    fun tmpVar () = freshVar "tmp"
    end (* local *)

    fun isSimple e = (case e
           of IR.E_Global _ => true
            | IR.E_State(NONE, _) => true
            | IR.E_State(SOME e, _) => isSimple e
            | IR.E_Var _ => true
            | IR.E_Lit _ => true
            | _ => false
          (* end case *))

  (* translate API types to C types *)
    fun trAPIType (env, ty) = (case ty
           of APITypes.IntTy => Env.intTy env
            | APITypes.BoolTy => Env.boolTy env
            | APITypes.TensorTy[] => Env.realTy env
            | APITypes.TensorTy dd => CL.T_Array(Env.realTy env, SOME(List.foldl Int.* 1 dd))
            | APITypes.StringTy => CL.charPtr (* const?? *)
            | APITypes.ImageTy(dim, _) => CL.T_Ptr(CL.T_Named "nrrd")
            | APITypes.SeqTy(ty, NONE) => CL.T_Ptr(CL.T_Named "nrrd")
            | APITypes.SeqTy(ty, SOME n) => CL.T_Array(trAPIType(env, ty), SOME n)
          (* end case *))

    datatype create_loop
      = ForLoop of TreeVar.t * TreeIR.exp * TreeIR.exp  (* loop over integer range *)
      | ForeachLoop of TreeVar.t * TreeIR.exp           (* loop over dynamic sequence *)

  (* decompose strand creation into the initialization of the iterations and the
   *)
    fun decomposeCreate (Create.Create{dim, code}) = let
          fun isLoop (IR.S_For _) = true
            | isLoop (IR.S_Foreach _) = true
            | isLoop _ = false
          fun getPrefix ([], _) = raise Fail "ill-formed create"
            | getPrefix ([stm], prefix) = if isLoop stm
                then (prefix, stm)
                else raise Fail "ill-formed create"
            | getPrefix (stm::stms, prefix) = (case stm
                 of IR.S_Assign _ => getPrefix (stms, stm::prefix)
                  | _ => raise Fail "ill-formed create"
                (* end case *))
          fun splitBody (TreeIR.Block{locals, body}) = let
                fun split ([], stms) = raise Fail "ill-formed create loop body"
                  | split ([IR.S_New(strand, args)], stms) =
                      {locals = !locals, stms = List.rev stms, newStm = (strand, args)}
                  | split (IR.S_New _ :: _, _) = raise Fail "unexpected new"
                  | split (stm::stms, stms') = split(stms, stm::stms')
                in
                  split (body, [])
                end
          fun splitLoops (blk as TreeIR.Block{locals, body}, lvs, prefix, lps) =
                if List.exists isLoop body
                  then let
                    val (prefix, loopStm) = getPrefix (body, prefix)
                    fun next (blk, lp) =
                          splitLoops (blk, List.revAppend(!locals, lvs), prefix, lp::lps)
                    in
                      case loopStm
                       of IR.S_For(x, lo, hi, blk) => next (blk, ForLoop(x, lo, hi))
                        | IR.S_Foreach(x, seq, blk) => next (blk, ForeachLoop(x, seq))
                        | _ => raise Fail "impossible"
                      (* end case *)
                    end
                  else {
                      dim = dim,
                      locals = List.rev lvs,
                      prefix = List.rev prefix,
                      loops = List.rev lps,
                      body = splitBody blk
                    }
          in
            splitLoops (code, [], [], [])
          end

  end
