(* raw-types.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * This file defines the representation of the raw scalar types used to
 * define image data in the NRRD file format.
 *)

structure RawTypes =
  struct

  (* raw numeric types as supported by NRRD *)
    datatype t
      = RT_Int8 | RT_UInt8
      | RT_Int16 | RT_UInt16
      | RT_Int32 | RT_UInt32
      | RT_Int64 | RT_UInt64
      | RT_Float | RT_Double

    fun same (a : t, b) = (a = b)

    fun hash RT_Int8    = 0w17
      | hash RT_UInt8   = 0w59
      | hash RT_Int16   = 0w103
      | hash RT_UInt16  = 0w157
      | hash RT_Int32   = 0w211
      | hash RT_UInt32  = 0w269
      | hash RT_Int64   = 0w331
      | hash RT_UInt64  = 0w389
      | hash RT_Float   = 0w449
      | hash RT_Double  = 0w509

    fun fromString ty = (case ty
           of "signed char" => RT_Int8
            | "unsigned char" => RT_UInt8
            | "short" => RT_Int16
            | "unsigned short" => RT_UInt16
            | "int" => RT_Int32
            | "unsigned int" => RT_UInt32
            | "long long int" => RT_Int64
            | "unsigned long long int" => RT_UInt64
            | "float" => RT_Float
            | "double" => RT_Double
            | _ => raise Fail("unknown raw type "^ty)
          (* end case *))

    fun toString ty = (case ty
           of RT_Int8 => "signed char"
            | RT_UInt8 => "unsigned char"
            | RT_Int16 => "short"
            | RT_UInt16 => "unsigned short"
            | RT_Int32 => "int"
            | RT_UInt32 => "unsigned int"
            | RT_Int64 => "long long int"
            | RT_UInt64 => "unsigned long long int"
            | RT_Float => "float"
            | RT_Double => "double"
          (* end case *))

  (* return the size of the type in bytes *)
    fun sizeb ty = (case ty
           of RT_Int8 => 1
            | RT_UInt8 => 1
            | RT_Int16 => 2
            | RT_UInt16 => 2
            | RT_Int32 => 4
            | RT_UInt32 => 4
            | RT_Int64 => 8
            | RT_UInt64 => 8
            | RT_Float => 4
            | RT_Double => 8
          (* end case *))

  end
