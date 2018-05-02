(* nrrd-enums.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2015 The University of Chicago
 * All rights reserved.
 *
 * Support for dealing with some of the enumeration types used in nrrd file headers.
 * This support includes understanding the output of dnorm and converting to the
 * symbolic names used in <teem/nrrdEnums.h>.
 *)

structure NrrdEnums =
  struct

  (* the subset of the axis kinds that Diderot can read and write *)
    datatype axis_kind
      = KindUnknown             (* unknown axis kind *)
      | KindDomain              (* any image domain *)
      | KindSpace               (* spatial domain *)
      | KindList                (* flat vector of values; used to represent tensors that do *)
                                (* not have one of the shapes recoognized by Teem *)
      | KindScalar              (* scalar *)
      | Kind2Vector             (* 2 component vector *)
      | Kind3Vector             (* 3-component vector *)
      | Kind4Vector             (* 4-component vector *)
      | Kind2DMatrix            (* 2x2 matrix: Mxx Mxy Myx Myy *)
      | Kind3DMatrix            (* 3x3 matrix: Mxx Mxy Mxz Myx Myy Myz Mzx Mzy Mzz *)

  (* convert from the string representation of an axis kind (see
   * http://teem.sourceforge.net/nrrd/format.html#kinds for info)
   *)
    fun kindFromString s = (case s
           of "none" => SOME KindUnknown
            | "domain" => SOME KindDomain
            | "space" => SOME KindSpace
            | "list" => SOME KindList
            | "scalar" => SOME KindScalar
            | "2-vector" => SOME Kind2Vector
            | "3-vector" => SOME Kind3Vector
            | "4-vector" => SOME Kind4Vector
            | "2D-matrix" => SOME Kind2DMatrix
            | "3D-matrix" => SOME Kind3DMatrix
            | _ => NONE
          (* end case *))

  (* convert an axis kind to its nrrd enum constant *)
    fun kindToEnum k = (case k
           of KindUnknown => "nrrdKindUnknown"
            | KindDomain => "nrrdKindDomain"
            | KindSpace => "nrrdKindSpace"
            | KindList => "nrrdKindList"
            | KindScalar => "nrrdKindScalar"
            | Kind2Vector => "nrrdKind2Vector"
            | Kind3Vector => "nrrdKind3Vector"
            | Kind4Vector => "nrrdKind4Vector"
            | Kind2DMatrix => "nrrdKind2DMatrix"
            | Kind3DMatrix => "nrrdKind3DMatrix"
          (* end case *))

  (* nrrd types *)
    datatype ty
      = TypeChar        (* 8-bit signed integer *)
      | TypeUChar       (* 8-bit unsigned integer *)
      | TypeShort       (* 16-bit signed integer *)
      | TypeUShort      (* 16-bit unsigned integer *)
      | TypeInt         (* 32-bit signed integer *)
      | TypeUInt        (* 32-bit unsigned integer *)
      | TypeLLong       (* 64-bit signed integer *)
      | TypeULLong      (* 64-bit unsigned integer *)
      | TypeFloat       (* 32-bit floating-point number *)
      | TypeDouble      (* 64-bit floating-point number *)

  (* convert from the string representation of a nrrd type (see
   * http://teem.sourceforge.net/nrrd/format.html#type for info)
   *)
    fun tyFromString s = (case s
           of "signed char" => SOME TypeChar
            | "int8" => SOME TypeChar
            | "int8_t" => SOME TypeChar
            | "uchar" => SOME TypeUChar
            | "unsigned char" => SOME TypeUChar
            | "uint8" => SOME TypeUChar
            | "uint8_t" => SOME TypeUChar
            | "short" => SOME TypeShort
            | "short int" => SOME TypeShort
            | "signed short" => SOME TypeShort
            | "signed short int" => SOME TypeShort
            | "int16" => SOME TypeShort
            | "int16_t" => SOME TypeShort
            | "ushort" => SOME TypeUShort
            | "unsigned short" => SOME TypeUShort
            | "unsigned short int" => SOME TypeUShort
            | "uint16" => SOME TypeUShort
            | "uint16_t" => SOME TypeUShort
            | "int" => SOME TypeInt
            | "signed int" => SOME TypeInt
            | "int32" => SOME TypeInt
            | "int32_t" => SOME TypeInt
            | "uint" => SOME TypeUInt
            | "unsigned int" => SOME TypeUInt
            | "uint32" => SOME TypeUInt
            | "uint32_t" => SOME TypeUInt
            | "longlong" => SOME TypeLLong
            | "long long" => SOME TypeLLong
            | "long long int" => SOME TypeLLong
            | "signed long long" => SOME TypeLLong
            | "signed long long int" => SOME TypeLLong
            | "int64" => SOME TypeLLong
            | "int64_t" => SOME TypeLLong
            | "ulonglong" => SOME TypeULLong
            | "unsigned long long" => SOME TypeULLong
            | "unsigned long long int" => SOME TypeULLong
            | "uint64" => SOME TypeULLong
            | "uint64_t" => SOME TypeULLong
            | "float" => SOME TypeFloat
            | "double" => SOME TypeDouble
            | _ => NONE
          (* end case *))

  (* convert a nrrd type to its nrrd enum constant *)
    fun tyToEnum ty = (case ty
           of TypeChar => "nrrdTypeChar"
            | TypeUChar => "nrrdTypeUChar"
            | TypeShort => "nrrdTypeShort"
            | TypeUShort => "nrrdTypeUShort"
            | TypeInt => "nrrdTypeInt"
            | TypeUInt => "nrrdTypeUInt"
            | TypeLLong => "nrrdTypeLLong"
            | TypeULLong => "nrrdTypeULLong"
            | TypeFloat => "nrrdTypeFloat"
            | TypeDouble => "nrrdTypeDouble"
          (* end case *))

  (* convert a nrrd type to a RawType.ty *)
    fun tyToRaw ty = (case ty
           of TypeChar => RawTypes.RT_Int8
            | TypeUChar => RawTypes.RT_UInt8
            | TypeShort => RawTypes.RT_Int16
            | TypeUShort => RawTypes.RT_UInt16
            | TypeInt => RawTypes.RT_Int32
            | TypeUInt => RawTypes.RT_UInt32
            | TypeLLong => RawTypes.RT_Int64
            | TypeULLong => RawTypes.RT_UInt64
            | TypeFloat => RawTypes.RT_Float
            | TypeDouble => RawTypes.RT_Double
          (* end case *))

    fun rawToTy rty = (case rty
           of RawTypes.RT_Int8 => TypeChar
            | RawTypes.RT_UInt8 => TypeUChar
            | RawTypes.RT_Int16 => TypeShort
            | RawTypes.RT_UInt16 => TypeUShort
            | RawTypes.RT_Int32 => TypeInt
            | RawTypes.RT_UInt32 => TypeUInt
            | RawTypes.RT_Int64 => TypeLLong
            | RawTypes.RT_UInt64 => TypeULLong
            | RawTypes.RT_Float => TypeFloat
            | RawTypes.RT_Double => TypeDouble
          (* end case *))
 
  end
