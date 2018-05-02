(* type-to-cxx.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure TypeToCxx : sig

  (* the scope in which a type will appear *)
    datatype namespace
      = NSTopLevel      (* top-level namespace *)
      | NSDiderot       (* Diderot library scope ("diderot::") *)
      | NSProgram       (* User-program scope (specified by "--namespace" option) *)

  (* translate a type for use inside the program's namespace scope
   * `trType (env, ty)` is the same as `trQType (env, Program, ty)`
   *)
    val trType : CodeGenEnv.t * TreeTypes.t -> CLang.ty

  (* translate a type for use outside the program's namespace scope *)
    val trQType : CodeGenEnv.t * namespace * TreeTypes.t -> CLang.ty

    val dynseqTy : CodeGenEnv.t * TreeTypes.t -> CLang.ty

    val imageTy : CodeGenEnv.t * ImageInfo.t -> CLang.ty

  end = struct

    structure Ty = TreeTypes
    structure CL = CLang
    structure Env = CodeGenEnv
    structure RN = CxxNames

    datatype namespace = NSTopLevel | NSDiderot | NSProgram

  (* translate to C++ image type *)
    fun trImageTy mk (env, info) = let
          val realTy = Env.realTy env
          val sampleTy = CL.T_Num(ImageInfo.sampleTy info)
        (* HACK: we fake the integer template parameter as a named type *)
          val voxSz = CL.T_Named(Int.toString(List.foldl Int.* 1 (ImageInfo.voxelShape info)))
          in
            mk (
              concat["image", Int.toString(ImageInfo.dim info), "d"],
              [realTy, sampleTy, voxSz])
          end

    fun trQType (env, ns, ty) = let
          val progNS = #namespace(Env.target env)
          fun diderotQ s = (case ns
                 of NSDiderot => CL.T_Named s
                  | _ => CL.T_Named("diderot::" ^ s)
                (* end case *))
          fun diderotTQ (s, args) = (case ns
                 of NSDiderot => CL.T_Template(s, args)
                  | _ => CL.T_Template("diderot::" ^ s, args)
                (* end case *))
          fun programQ s = (case ns
                 of NSProgram => CL.T_Named s
                  | _ => CL.T_Named(concat[progNS, "::",  s])
                (* end case *))
          fun tr ty = (case ty
               of Ty.BoolTy => CL.boolTy
                | Ty.StringTy => CL.T_Named "std::string"
                | Ty.IntTy => Env.intTy env
                | (Ty.VecTy(1, 1)) => Env.realTy env
                | (Ty.VecTy(d, _)) => CL.T_Named("vec" ^ Int.toString d)
                | (Ty.TensorTy []) => Env.realTy env
                | (Ty.TensorTy dd) => programQ(RN.tensorStruct dd)
                | (Ty.TensorRefTy dd) => programQ(RN.tensorRefStruct dd)
                | (Ty.TupleTy tys) => raise Fail "FIXME: TupleTy"
                | (Ty.SeqTy(ty, NONE)) => diderotTQ("dynseq", [tr ty])
                | (Ty.SeqTy(ty, SOME n)) =>
                    diderotTQ("array", [tr ty, CL.T_Named(Int.toString n)])
                | (Ty.ImageTy info) => trImageTy diderotTQ (env, info)
                | (Ty.StrandIdTy _) => programQ "strand_array::sid_t"
              (* end case *))
          in
            tr ty
          end

    val imageTy = trImageTy (fn (ty, args) => CL.T_Template("diderot::" ^ ty, args))

    fun trType (env, ty) = (case ty
           of Ty.BoolTy => CL.boolTy
            | Ty.StringTy => CL.T_Named "std::string"
            | Ty.IntTy => Env.intTy env
            | (Ty.VecTy(1, 1)) => Env.realTy env
            | (Ty.VecTy(d, _)) => CL.T_Named("vec" ^ Int.toString d)
            | (Ty.TensorTy []) => Env.realTy env
            | (Ty.TensorTy dd) => RN.tensorTy dd
            | (Ty.TensorRefTy dd) => RN.tensorRefTy dd
            | (Ty.TupleTy tys) => raise Fail "FIXME: TupleTy"
            | (Ty.SeqTy(ty, NONE)) => dynseqTy (env, ty)
            | (Ty.SeqTy(ty, SOME n)) =>
                CL.T_Template("diderot::array", [trType(env, ty), CL.T_Named(Int.toString n)])
            | (Ty.ImageTy info) => imageTy (env, info)
            | (Ty.StrandIdTy _) => CL.T_Named "strand_array::sid_t"
          (* end case *))

  (* dynamic sequence of the specified element type *)
    and dynseqTy (env, ty) = CL.T_Template("diderot::dynseq", [trType(env, ty)])

  end
