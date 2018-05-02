(* gen-library-json.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2017 The University of Chicago
 * All rights reserved.
 *
 * TODO:
 *      add Diderot types for input variables
 *      for the debugger target, we need to generate hooks for the globals
 *      for the debugger target, we need to use "void *" as the type for input get/set functions
 *)

structure GenLibraryJSON : sig

    val gen : {
            env : CodeGenEnv.t,                 (* target information *)
            subs : (string * string) list,      (* substitutions for fragment expansion *)
            rt : JSON.value option,             (* fragment with extra runtime system hooks *)
            strand : TreeIR.strand,
            inputs : TreeGlobalVar.t Inputs.input list,
            outputs : OutputUtil.output_info list
          } -> unit

  end = struct

    structure Ty = APITypes
    structure CL = CLang
    structure Env = CodeGenEnv
    structure J = JSON
    structure JU = JSONUtil
    structure GenLib = GenLibraryInterface
    structure TSV = TreeStateVar

  (* expand the STRING values in a JSON tree *)
    fun expandStrings subs = let
          val expand = StringSubst.expand subs
          fun expandJ jv = (case jv
                 of J.ARRAY vs => J.ARRAY(List.map expandJ vs)
                  | J.OBJECT flds =>
                      J.OBJECT(List.map (fn (lab, v) => (lab, expandJ v)) flds)
                  | J.STRING s => J.STRING(expand s)
                  | _ => jv
                (* end case *))
          in
            expandJ
          end

    val nrrdPtrTy = CL.T_Ptr(CL.T_Named "Nrrd")
    val stringTy = CL.constPtrTy CL.charTy

  (* translate a CLang representation of a C type to JSON *)
    fun ctyToJSON (CL.T_Num rty) = J.STRING(RawTypes.toString rty)
      | ctyToJSON (CL.T_Const ty) = J.OBJECT[
            ("kind", J.STRING "const"),
            ("arg", ctyToJSON ty)
          ]
      | ctyToJSON (CL.T_Ptr ty) = J.OBJECT[
            ("kind", J.STRING "*"),
            ("arg", ctyToJSON ty)
          ]
      | ctyToJSON (CL.T_Array(ty, NONE)) = J.OBJECT[
            ("kind", J.STRING "*"),
            ("arg", ctyToJSON ty)
          ]
      | ctyToJSON (CL.T_Array(ty, SOME n)) = J.OBJECT[
            ("kind", J.STRING "[]"),
            ("size", J.INT(IntInf.fromInt n)),
            ("arg", ctyToJSON ty)
          ]
      | ctyToJSON (CL.T_Named s) = J.STRING s
      | ctyToJSON _ = raise Fail "not a C type"

  (* translate an APIType to its JSON representation *)
    fun apiTyToJSON Ty.IntTy = J.STRING "int"
      | apiTyToJSON Ty.BoolTy = J.STRING "bool"
      | apiTyToJSON (Ty.TensorTy shp) = J.OBJECT[
            ("kind", J.STRING "tensor"),
            ("shape", J.ARRAY(List.map (fn d => JSON.INT(IntInf.fromInt d)) shp))
          ]
      | apiTyToJSON Ty.StringTy = J.STRING "string"
      | apiTyToJSON (Ty.ImageTy(d, shp)) = J.OBJECT[
            ("kind", J.STRING "image"),
            ("dimension", J.INT(IntInf.fromInt d)),
            ("shape", J.ARRAY(List.map (fn d => JSON.INT(IntInf.fromInt d)) shp))
          ]
      | apiTyToJSON (Ty.SeqTy(ty, SOME n)) = J.OBJECT[
            ("kind", J.STRING "seq"),
            ("elem-ty", apiTyToJSON ty),
            ("size", J.INT(IntInf.fromInt n))
          ]
      | apiTyToJSON (Ty.SeqTy(ty, NONE)) = J.OBJECT[
            ("kind", J.STRING "dynseq"),
            ("elem-ty", apiTyToJSON ty)
          ]

    fun mkParam (cty, name) = J.OBJECT[
            ("name", J.STRING name),
            ("param-ty", ctyToJSON cty)
          ]

    fun mkFunc (retTy, name, params) = J.OBJECT[
            ("return-ty", ctyToJSON retTy),
            ("name", J.STRING name),
            ("params", J.ARRAY params)
          ]

    fun mkGetSize (spec, name, params) = J.OBJECT[
            ("return-ty", ctyToJSON(CL.T_Named "size_t")),
            ("name", J.STRING(GenLib.inputGetSize(spec, name))),
            ("params", J.ARRAY params)
          ]

    fun gen {env, subs, rt, strand, inputs, outputs} = let
          val spec = Env.target env
          val boolTy = Env.boolTy env
          val jsonFilename = OS.Path.joinDirFile{
                  dir = #outDir spec,
                  file = OS.Path.joinBaseExt{base = #outBase spec, ext = SOME "json"}
                }
        (* the JSON representation of the world type *)
          val worldPtrTy = J.OBJECT[
                  ("kind", J.STRING "*"),
                  ("arg", J.STRING(TargetSpec.qualifyCId "world_t" spec))
                ]
        (* the JSON representation of the world parameter *)
          val worldParam = J.OBJECT[
                  ("name", J.STRING "wrld"),
                  ("param-ty", worldPtrTy),
                  ("attrs", J.ARRAY[J.STRING "world"])
                ]
        (* generate the JSON representation for an input *)
          fun mkInput (Inputs.INP{var, name, ty,  desc, init}) = let
                val flds = if Inputs.isDefault init
                      then let
                        val fnName = GenLib.inputGet(spec, name)
                      (* convert the input type to a by-reference C type *)
(* FIXME: factor out the common part of this translation in GenLibraryInterface *)
                        val outTy = (case ty
                               of Ty.BoolTy => CL.T_Ptr(GenLib.toCType(env, ty))
                                | Ty.StringTy => CL.T_Ptr CL.charPtr
                                | Ty.IntTy => CL.T_Ptr(GenLib.toCType(env, ty))
                                | Ty.TensorTy[] => CL.T_Ptr(GenLib.toCType(env, ty))
                                | Ty.TensorTy _=> GenLib.toCType(env, ty)
                                | Ty.SeqTy(_, SOME _) => GenLib.toCType(env, ty)
                                | Ty.SeqTy(_, NONE) => CL.T_Ptr CL.voidPtr
                                | Ty.ImageTy _ => CL.T_Ptr CL.voidPtr
                              (* end case *))
                        val flds = if Ty.hasDynamicSize ty
                              then [("get-size", mkGetSize (spec, name, [worldParam]))]
                              else []
                        in
                          ("has-default", J.BOOL true) ::
                          ("get", mkFunc (CL.voidTy, fnName, [worldParam, mkParam (outTy, "v")])) ::
                          flds
                        end
                      else [
                          ("has-default", J.BOOL false)
                        ]
                val flds = let
                    (* prototypes for setting an image or dynamic sequence from a nrrd *)
                      fun loadPrototypes () = let
                            val set1 = mkFunc (boolTy, GenLib.inputSetByName(spec, name), [
                                    worldParam, mkParam (stringTy, "s")
                                  ])
                            val set2 = mkFunc (boolTy, GenLib.inputSet(spec, name), [
                                    worldParam, mkParam (nrrdPtrTy, "data")
                                  ])
                            in
                              ("set", J.ARRAY[set1, set2]) :: flds
                            end
                      in
                        case ty
                         of Ty.ImageTy _ => loadPrototypes()
                          | Ty.SeqTy(_, NONE) => loadPrototypes()
                          | _ => ("set", mkFunc (boolTy, GenLib.inputSet(spec, name), [
                                worldParam, mkParam (GenLib.toCType(env, ty), "v")
                              ])) :: flds
                        (* end case *)
                      end
                val flds = ("type", apiTyToJSON ty) :: flds
                val flds = (case desc
                       of NONE => flds
                        | SOME desc => ("description", J.STRING desc) :: flds
                      (* end case *))
                in
                  JSON.OBJECT(("name", J.STRING name) :: flds)
                end
        (* generate a JSON object for an output *)
          fun mkOutput {ty, name, kind} = let
                fun mkF snapshot = let
                      val fnName = if snapshot
                            then GenLib.snapshotGet(spec, name)
                            else GenLib.outputGet(spec, name)
                      val params = [mkParam(nrrdPtrTy, "data")]
                      val params = (case ty
                             of Ty.SeqTy(_, NONE) => mkParam(nrrdPtrTy, "lengths") :: params
                              | _ => params
                            (* end case *))
                      in
                        mkFunc (boolTy, fnName, worldParam :: params)
                      end
                val flds = if (#snapshot spec)
                      then [("snapshot", mkF true), ("get", mkF false)]
                      else [("get", mkF false)]
                val flds = ("type", apiTyToJSON ty) :: flds
                in
                  JSON.OBJECT(("name", J.STRING name) :: flds)
                end
        (* specialize the generic part of the JSON *)
          val api = expandStrings subs Fragments.jsonBody
        (* add the program-specific stuff to the JSON representation *)
          val api = JU.append(api, [JU.SEL "inputs"], List.map mkInput inputs)
          val api = JU.append(api, [JU.SEL "outputs"], List.map mkOutput outputs)
        (* include information about the strand when the target is the debugger *)
          val api = if TargetSpec.isDebugger spec
                then let
                  val TreeIR.Strand{name, state, ...} = strand
                  val prefix = TargetSpec.qualifyCId (Atom.toString name ^ "_get_") spec
                  val prefix' = TargetSpec.qualifyCId (Atom.toString name ^ "_get_size") spec
                (* make the JSON object for a strand-state-variable debugger hook *)
                  fun svToJSON sv = let
                        val params = [
                                worldParam,
                                mkParam (CL.T_Num RawTypes.RT_UInt32, "id")
                              ]
                        val flds = if Ty.hasDynamicSize(TSV.apiTy sv)
                              then [("get-size", mkGetSize (spec, prefix' ^ TSV.name sv, params))]
                              else []
                        in
                          J.OBJECT(
                            ("name", J.STRING(TSV.name sv)) ::
                            ("type", apiTyToJSON(TSV.apiTy sv)) ::
                            ("get", mkFunc (CL.constVoidPtr, prefix ^ TSV.name sv, params)) ::
                            flds)
                        end
                  val strandObj = J.OBJECT[
                          ("name", J.STRING(Atom.toString name)),
                          ("state-vars", J.ARRAY(List.map svToJSON state))
                        ]
                  in
                    JU.insert(api, [], "strand", strandObj)
                  end
                else api
          val outS = TextIO.openOut jsonFilename
          in
            JSONPrinter.print' {strm = outS, pretty = true} api
          end

  end
