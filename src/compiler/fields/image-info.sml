(* image-info.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Information about an image object.
 *
 * TODO:
 *      handle images where the symmetries are exploited.
 *)

structure ImageInfo : sig

    type t

  (* Do the images have the same dimension and voxel type? *)
    val sameShape : t * t -> bool

  (* Are two image infos the same? *)
    val same : t * t -> bool

  (* hash value (based on image file ID) *)
    val hash : t -> word

  (* make image info from a proxy Nrrd file, target domain dimension, and range shape;
   * return NONE if the domain and/or range do not match the structure of the nrrd file.
   *)
    val fromNrrd : NrrdInfo.t * int * int list -> t option

  (* create a default image from a domain dimension and range shape. *)
    val mkInfo : int * int list -> t

    val toString : t -> string          (* return a string representation of the info *)
    val dim : t -> int                  (* dimension of space *)
    val sizes : t -> int list           (* size of each dimension (not including the
                                         * data axis) listed in fast to slow order.
                                         *)
    val voxelShape : t -> int list      (* shape of voxels; empty list for scalars *)
    val stride : t -> int               (* for non-scalar images, this returns the *)
                                        (* number of samples between voxels *)
    val sampleTy : t -> RawTypes.t      (* representation type of samples.  If there isn't
                                         * a proxy, then this will be float.
                                         *)
    val hasProxy : t -> bool            (* return true if the image has a proxy *)

  end = struct

  (* Image voxels are tensors of some raw representation type *)
    type voxel_ty = (int list * RawTypes.t)

    datatype t = ImgInfo of {
        stamp : Stamp.t,                (* unique ID *)
        dim : int,                      (* dimension of space *)
        sizes : int list,               (* number of samples along each axis (not including
                                         * the data axis); we follow the Nrrd convention of
                                         * listing the axes in fast to slow order.
                                         *)
        ty : voxel_ty
      }

    fun sameVoxelTy ((dd1, ty1), (dd2, ty2)) = (ty1 = ty2) andalso (dd1 = dd2)

    fun sameShape (ImgInfo{stamp=s1, dim=d1, ty=ty1, ...}, ImgInfo{stamp=s2, dim=d2, ty=ty2, ...}) =
          Stamp.same(s1, s2) orelse ((d1 = d2) andalso sameVoxelTy(ty1, ty2))

    fun same (ImgInfo{stamp=s1, ...}, ImgInfo{stamp=s2, ...}) = Stamp.same(s1, s2)

    fun hash (ImgInfo{stamp, ...}) = Stamp.hash stamp

  (* make image info from a proxy Nrrd file, target domain dimension, and range shape *)
    fun fromNrrd (nrrd, dim, dd) = let
          val {elemTy, nElems} = NrrdInfo.voxelInfo nrrd
          val nElems' = List.foldl (op * ) 1 dd
          in
            if (NrrdInfo.dim nrrd <> dim) orelse (nElems <> nElems')
              then NONE
              else SOME(ImgInfo{
                  stamp = Stamp.new(),
                  dim = dim,
                  sizes = NrrdInfo.sizes nrrd,
                  ty = (dd, elemTy)
                })
          end

    fun mkInfo (dim, dd) = ImgInfo{
            stamp = Stamp.new(),
            dim = dim,
            sizes = [], (* unknown *)
            ty = (dd, RawTypes.RT_Float)
          }

    fun toString (ImgInfo{dim, ty=(dd, rTy), ...}) = let
          val shape = (case dd
                 of [] => "[]"
                  | [d] => concat["[", Int.toString d, "]"]
                  | dd => concat["[", String.concatWith "," (List.map Int.toString dd), "]"]
                (* end case *))
          in
            concat[
                "IMAGE", Int.toString dim, "D<", RawTypes.toString rTy, shape, ">"
              ]
          end

    fun dim (ImgInfo{dim, ...}) = dim

    fun sizes (ImgInfo{sizes, ...}) = sizes

    fun voxelShape (ImgInfo{ty=(dd, _), ...}) = dd

    fun stride (ImgInfo{ty=(dd, rTy), ...}) = List.foldl (op * ) 1 dd

    fun sampleTy (ImgInfo{ty=(_, rTy), ...}) = rTy

    fun hasProxy (ImgInfo{sizes=[], ...}) = false
      | hasProxy _ = true

  end
