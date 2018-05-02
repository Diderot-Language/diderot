(* output-util.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *)

structure OutputUtil : sig

    datatype kind = Global | Shared | Local

    type output_info = {
        name : string,          (* the variable's name *)
        ty : APITypes.t,        (* the variables API type *)
        kind : kind             (* what kind of output variable? (always Shared or Local for now) *)
      }

  (* gather the outputs of a program *)
    val gatherOutputs : TargetSpec.t * TreeIR.program -> output_info list

  (* return information about the output type.  This is a tuple
   *
   *    (c-type, nrrd-type, nrrd-axis-kind, # elements)
   *)
    val infoOf : CodeGenEnv.t * APITypes.t -> (CLang.ty * NrrdEnums.ty * NrrdEnums.axis_kind * int)

  end = struct

    structure IR = TreeIR
    structure SVar = TreeStateVar
    structure Ty = APITypes
    structure Nrrd = NrrdEnums
    structure Env = CodeGenEnv
    structure CL = CLang

    datatype kind = Global | Shared | Local

    type output_info = {
        name : string,          (* the variable's name *)
        ty : APITypes.t,        (* the variables API type *)
        kind : kind             (* what kind of output variable? (always Shared or Local for now) *)
      }

    fun gatherOutputs (spec, IR.Program{strand=IR.Strand{state, ...}, ...}) = let
          fun kindOf x =
                if TargetSpec.dualState spec
                andalso SVar.isShared x andalso SVar.isVarying x
                  then Shared
                  else Local
          fun getOutput x = if SVar.isOutput x
                then SOME{name = SVar.name x, ty = SVar.apiTy x, kind = kindOf x}
                else NONE
          in
            List.mapPartial getOutput state
          end

    fun infoOf (env, ty) = (case ty
           of Ty.IntTy => if #longint(Env.target env)
                then (CL.int64, Nrrd.TypeLLong, Nrrd.KindScalar, 1)
                else (CL.int32, Nrrd.TypeInt, Nrrd.KindScalar, 1)
            | Ty.BoolTy => (CL.uint8, Nrrd.TypeUChar, Nrrd.KindScalar, 1)
            | Ty.TensorTy [] => if #double(Env.target env)
                then (CL.double, Nrrd.TypeDouble, Nrrd.KindScalar, 1)
                else (CL.float, Nrrd.TypeFloat, Nrrd.KindScalar, 1)
            | Ty.TensorTy dims => let
                val (axisKind, nElems) = (case dims
                       of [2] => (Nrrd.Kind2Vector, 2)
                        | [3] => (Nrrd.Kind3Vector, 3)
                        | [4] => (Nrrd.Kind4Vector, 4)
                        | [2,2] => (Nrrd.Kind2DMatrix, 4)
                        | [3,3] => (Nrrd.Kind3DMatrix, 9)
                        | _ => (Nrrd.KindList, List.foldl Int.* 1 dims)
                      (* end case *))
                in
                  if #double(Env.target env)
                    then (CL.double, Nrrd.TypeDouble, axisKind, nElems)
                    else (CL.float, Nrrd.TypeFloat, axisKind, nElems)
                end
            | Ty.SeqTy(ty, SOME 1) => infoOf (env, ty)
            | Ty.SeqTy(ty, SOME n) => let
                val (elemTy, nrrdTy, axisKind, nElems) = infoOf (env, ty)
                in
                  (elemTy, nrrdTy, Nrrd.KindList, n*nElems)
                end
            | _ => raise Fail(concat["GetOutput.infoOf(", Ty.toString ty, ")"])
          (* end case *))

  end
