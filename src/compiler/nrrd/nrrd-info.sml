(* nrrd-info.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Information about a NRRD file.
 *
 * TODO:
 *      handle files where the symmetries are exploited.
 *)

structure NrrdInfo : sig

    datatype t = NrrdInfo of {
        id : OS.FileSys.file_id,        (* ID of nrrd file *)
        dim : int,                      (* dimension of space *)
        ty : RawTypes.t,                (* types of data values *)
        dataKind : NrrdEnums.axis_kind, (* the nrrd axis kind of the data axis *)
        nElems : int,                   (* the number of elements per voxel *)
        sizes : int list                (* number of samples along each axis (not including
                                         * the data axis); we follow the Nrrd convention of
                                         * listing the axes in fast to slow order.
                                         *)
      }

  (* are the underlying files the same? *)
    val same : t * t -> bool

  (* hash value (based on nrrd file ID) *)
    val hash : t -> word

  (* get info from a Nrrd file.  If the file does not exist, then a warning is inserted
   * into the error stream and NONE is returned.
   *)
    val getInfo : Error.err_stream * string -> t option

    val dim : t -> int                  (* dimension of space *)
    val sizes : t -> int list           (* size of each dimension (not including the
                                         * data axis) listed in fast to slow order.
                                         *)
    val voxelInfo : t -> {              (* get information about the data in the file *)
            elemTy : RawTypes.t,          (* type of voxel elements *)
            nElems : int                  (* number of elements per voxel *)
          }
    val voxelSzB : t -> int             (* size in bytes of a voxel *)
    val stride : t -> int               (* for non-scalar data, this returns the *)
                                        (* number of samples between voxels *)
    val sampleTy : t -> RawTypes.t      (* representation type of samples *)

  end = struct

    datatype t = NrrdInfo of {
        id : OS.FileSys.file_id,        (* ID of nrrd file *)
        dim : int,                      (* dimension of space *)
        ty : RawTypes.t,                (* the type of the elements *)
        dataKind : NrrdEnums.axis_kind, (* the nrrd axis kind of the data axis *)
        nElems : int,                   (* the number of elements per voxel *)
        sizes : int list                (* number of samples along each axis (not including
                                         * the data axis); we follow the Nrrd convention of
                                         * listing the axes in fast to slow order.
                                         *)
      }

    fun same (NrrdInfo{id=a, ...}, NrrdInfo{id=b, ...}) = (a = b)

    fun hash (NrrdInfo{id, ...}) = OS.FileSys.hash id

    fun doHeader (errStrm, fileName, header) = let
          fun err msg = (
                Error.error (errStrm, msg);
                raise Error.ERROR)
          fun fields s = String.tokens Char.isSpace s
          fun set (r, v) = (r := SOME v)
          fun get (tag, r) = (case !r of NONE => err ["missing "^tag] | SOME v => v)
          val ty = ref NONE
          val totalDim = ref NONE
          val dim = ref NONE
          val kinds = ref NONE
          val sizes = ref NONE
          fun doValue ("type", v) = (case NrrdEnums.tyFromString v
                 of SOME ty' => set(ty, ty')
                  | NONE => err [
                        "bogus dnorm output: \"", "type: ", String.toString v, "\n"
                      ]
                (* end case *))
            | doValue ("dimension", v) = set (totalDim, valOf(Int.fromString v))
            | doValue ("space dimension", v) = set (dim, valOf(Int.fromString v))
            | doValue ("sizes", v) = let
                fun atoi s = (case Int.fromString s
                       of SOME i => i
                        | NONE => err [
                              "bogus dnorm output: \"", "sizes: ", String.toString v, "\n"
                            ]
                      (* end case *))
                in
                  set (sizes, List.map atoi (fields v))
                end
            | doValue ("space directions", v) = ()
            | doValue ("kinds", v) = let
                fun s2kind s = (case NrrdEnums.kindFromString s
                       of SOME k => k
                        | NONE => err ["axis kind \"", s, "\" not supported"]
                      (* end case *))
                in
                  set (kinds, List.map s2kind (fields v))
                end
            | doValue ("endian", v) = ()
            | doValue ("encoding", v) = ()
            | doValue ("space origin", v) = ()
            | doValue _ = ()
          in
            Log.msg [fileName, " file header:\n"];
            List.app (fn (tag, value) => Log.msg["  ", tag, ": ", value, "\n"]) header;
            List.app doValue header;
            let
            val dim = get ("space dimension", dim)
            val totalDim = get ("dimension", totalDim)
          (* split the sizes into the data axis size, which comes first
           * in the list, and those that span the space's dimensions. Note
           * that for some scalar images, there is no data axis.
           *)
            val (nElems, sizes, dataKind) = if (dim = totalDim)
                  then (1, get ("sizes", sizes), NrrdEnums.KindScalar)
                  else let
                    val (nElems::sizes) = get ("sizes", sizes)
                    in
                      (nElems, sizes, hd (get ("kinds", kinds)))
                    end
            in
              NrrdInfo{
                  id = OS.FileSys.fileId fileName,
                  dim = dim,
                  ty = NrrdEnums.tyToRaw (get ("type", ty)),
                  dataKind = dataKind,
                  nElems = nElems,
                  sizes = sizes
                }
            end
          end

    fun getInfo (errStrm, fileName) = if OS.FileSys.access(fileName, [OS.FileSys.A_READ])
          then (case RunDNorm.run (errStrm, fileName)
             of SOME{version, header} => SOME(doHeader (errStrm, fileName, header))
              | NONE => NONE
            (* end case *))
          else NONE

    fun dim (NrrdInfo{dim, ...}) = dim

    fun sizes (NrrdInfo{sizes, ...}) = sizes

  (* get information about the data in the nrrd file *)
    fun voxelInfo (NrrdInfo{ty, nElems, ...}) = {elemTy = ty, nElems = nElems}

    fun voxelSzB (NrrdInfo{ty, nElems, ...}) = nElems * RawTypes.sizeb ty

    fun stride (NrrdInfo{nElems, ...}) = nElems

    fun sampleTy (NrrdInfo{ty, ...}) = ty

  end
