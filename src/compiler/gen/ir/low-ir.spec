# specification of operators for LowIR version of the IR.  Each line (other than comments)
# specifies an operator using five fields, which are separated by ":".  The fields are
#       name
#       argument type           (optional)
#       result arity
#       arity
#       comment                 (optional)
#
# Operations with effects are denoted by a "!" as the first character of the line.
#
# integer arithmetic operations
IAdd : : 1 : 2 : integer addition
ISub : : 1 : 2 : integer subtraction
IMul : : 1 : 2 : integer multiplication
IDiv : : 1 : 2 : integer division
IMod : : 1 : 2 : integer modulo
INeg : : 1 : 1 : integer negation
#
# scalar arithmetic operations
RAdd : : 1 : 2 : scalar (real) addition
RSub : : 1 : 2 :
RMul : : 1 : 2 :
RDiv : : 1 : 2 :
RNeg : : 1 : 1 :
#
# comparisons (integer and scalar)
LT : ty : 1 : 2 :
LTE : ty : 1 : 2 :
EQ : ty : 1 : 2 :
NEQ : ty : 1 : 2 :
GT : ty : 1 : 2 :
GTE : ty : 1 : 2 :
BAnd : : 1 : 2 : boolean and
BOr : : 1 : 2 : boolean or
BNot : : 1 : 1 : boolean negation
Abs : ty : 1 : 1 :
Max : ty : 1 : 2 :
Min : ty : 1 : 2 :
#
# RClamp<ty>(lo, hi, x) -- clamps x to the range lo..hi
RClamp : : 1 : 3 : clamp argument to range
# RLerp<ty>(a, b, t) -- computes a + t*(b-a)
RLerp : : 1 : 3 : linear interpolation between 0 and 1
#
# vector arithmetic operations (int is arity)
VAdd   : int : 1 : 2 : vector addition
VSub   : int : 1 : 2 : vector subtraction
VScale : int : 1 : 2 : vector scaling
VMul   : int : 1 : 2 : vector element-wise multiplication
VNeg   : int : 1 : 2 : vector negation
VSum   : int : 1 : 1 : sum elements of a vector
VDot   : int : 1 : 2 : dot product of two vectors
# VIndex<d,w,i> -- project i'th element (0-based) of vector with type VecTy(d,w)
VIndex  : int * int : 1 : 1 : project
### tensor operations
#
# TensorIndex<ty,idxs>(T) returns the scalar T[idxs], where T has type ty
# and the indices are 0-based.
TensorIndex : ty * shape : 1 : 1 :
#
# ProjectLast<ty,idxs>(T) returns the vector T[idxs,:], where the indices are 0-based
ProjectLast : ty * shape : 1 : 1 :
#
### matrix operations
#
EigenVecs2x2 : : 2 : 1 : Eigen vectors and values for 2x2 matrix
EigenVecs3x3 : : 2 : 1 : Eigen vectors and values for 3x3 matrix
EigenVals2x2 : : 1 : 1 : Eigen values for 2x2 matrix
EigenVals3x3 : : 1 : 1 : Eigen values for 3x3 matrix
# Zero<ty>() -- zero tensor
Zero : ty : 1 : 0 : all zeros tensor
#
### tuple operations
#
# Select<ty,i>(u)  -- select ith element of tuple; ty is tuple type
Select : ty * int : 1 : 1 :
#
### operations on sequences
#
# Subscript<ty>(u,i) -- select ith element of sequence; ty is type of sequence
Subscript : ty : 1 : 2 :
# MkDynamic<ty,n> -- make a sequence with type ty[n] into a dynamic sequence
!MkDynamic : ty * int : 1 : 1 : make a fixed-length sequence dynamic
!Append : ty : 1 : 2 : append an element onto a dynamic sequence
!Prepend : ty : 1 : 2 : prepend an element onto a dynamic sequence
!Concat : ty : 1 : 2 : concatenate two dynamic sequences
# Range(lo,hi) -- create a sequence with values [lo, lo+1, ..., hi]
Range : : 1 : 2 : create a range sequence
# Length<ty> -- return the length of a sequence with type ty[]
Length : ty : 1 : 1 : return the length of a dynamic sequence
#
# SphereQuery<dim,seqTy>(pos, radius)
SphereQuery : int * ty : 1 : 2 : find strands within a sphere
#
Sqrt   : : 1 : 1 : returns the sqrt
Cos    : : 1 : 1 : returns the cosine
ArcCos : : 1 : 1 : returns the arccosine
Sin    : : 1 : 1 : returns the sine
ArcSin : : 1 : 1 : returns the arcsine
Tan    : : 1 : 1 : returns the tangent
ArcTan : : 1 : 1 : returns the arctangent
Exp    : : 1 : 1 : returns "e" raised to its argument
Sign   : : 1 : 1 : returns -1, 0, or 1 depending on the sign of its argument
#
#ifwrap
IfWrap:: 1 :3 : #if expression. creates in-line function else.
# compute integral parts of reals
Ceiling : int : 1 : 1 : compute real ceiling of a vector
Floor : int : 1 : 1 : compute real floor of a vector
Round : int : 1 : 1 : compute real rounding to nearest integral real of a vector
Trunc : int : 1 : 1 : compute real truncation to integral real of a vector
#
### conversions; the real to int form works on vectors
IntToReal : : 1 : 1 :
RealToInt : int : 1 : 1 : cast reals to ints and vectors to int sequences
#
### Strand operations
#
# NumStrands<S>() -- denotes the number of strands in the strand set S
#
NumStrands : StrandSets.t : 1 : 0 :
#
# Strands<ty,S>() -- denotes the list of strands in the strand set S; ty is the strand type
Strands : ty * StrandSets.t : 1 : 0 :
#
### image operations
#
Transform : ImageInfo.t : 1 : 1 : Pulls transformation matrix from image.
Translate : ImageInfo.t : 1 : 1 : Pulls translation vector from image.
#
# ControlIndex<I,ctl,d>(V,i) -- border control for the d'th axis of the image V and index i.
# This operation returns the wrapped/mirrored/clamped index, which can then be used to compute
# the address of the voxel.
ControlIndex : ImageInfo.t * idxctl * int : 1 : 2 :
#
# LoadVoxel<I>(V,offp) -- load a voxel value from the address `a+offp`, where `a` is
# the base address of the image `V` and `offp` is the offset (in datum-size units) of
# the voxel to be loaded.
LoadVoxel : ImageInfo.t : 1 : 2 : load a voxel value
#
# Inside<I,s>(x,V) -- tests to see if the image-space position x is inside the domain of V.
# I is the image info for V, s is the size of the sample to be loaded
Inside : ImageInfo.t * int : 1 : 2 :
#
# IndexInside<I,s>(n,V) -- tests to see if the index sequence n is inside the domain of V.
# I is the image info for V, s is the size of the sample to be loaded (see LoadVoxels)
IndexInside : ImageInfo.t * int : 1 : 2 :
#
# ImageDim<I,i>(V) -- returns the i'th dimension of the image
ImageDim : ImageInfo.t * int : 1 : 1 :
#
### other operations
#
# nrrd file loading
LoadSeq   : ty * string : 1 : 0 : load sequence from nrrd file
LoadImage : ty * string : 1 : 0 : load image from nrrd file
#
# Kill all active strands
!KillAll : : 0 : 0 :
#
# Stabilize all active strands
!StabilizeAll : : 0 : 0 :
#
# printing support for debugging
!Print : tys : 0 : * : print strings
#
# unlifted math functions
MathFn : MathFns.t : 1 : * : math function
