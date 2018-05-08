# specification of operators for MidIR version of the IR.  Each line (other than comments)
# specifies an operator using five fields, which are separated by ":".  The fields are
#       name
#       argument type           (optional)
#       result arity
#       arity
#       comment                 (optional)
#
# Operations with effects are denoted by a "!" as the first character of the line.
#
# integer operations
IAdd : : 1 : 2 : integer addition
ISub : : 1 : 2 : integer subtraction
IMul : : 1 : 2 : integer multiplication
IDiv : : 1 : 2 : integer division
IMod : : 1 : 2 : integer modulo
INeg : : 1 : 1 : integer negation
IAbs : : 1 : 1 : integer abs()
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
Max : ty : 1 : 2 :
Min : ty : 1 : 2 :
### tensor operations
#
# TensorIndex<ty,idxs>(T) returns the scalar T[idxs], where T has type ty
TensorIndex : ty * shape : 1 : 1 :
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
# compute integral parts of reals
Ceiling : int : 1 : 1 : compute real ceiling of a vector
Floor : int : 1 : 1 : compute real floor of a vector
Round : int : 1 : 1 : compute real rounding to nearest integral real of a vector
Trunc : int : 1 : 1 : compute real truncation to integral real of a vector
#
### conversions; the real to int forms are vector ops
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
### image/kernel operations
#
# BuildPos<s>(x) -- builds a vector of 2s kernel arguments at positions [x-s, .., x+(s-1)]
BuildPos : int : 1 : 1 : compute vector of kernel arguments
#
# EvalKernel<d,h,k>(u) -- computes h^(k)(u_i) for 1<i<d, where d is the size of vector u.
EvalKernel : int * Kernel.t * int : 1 : 1 : evaluate a kernel function application
#
# Kernel<h,k>() -- represents the k'th derivative of the kernel h.
Kernel    : Kernel.t * int  : 1 : 0 :
#
Transform  : ImageInfo.t : 1 : 1 : Pulls transformation matrix from image.
Translate  : ImageInfo.t : 1 : 1 : Pulls translation vector from image.
#
# LoadVoxels<I,s>(V,n) -- loads a tensor of voxels from the image V, where I is the
# image info for V, s is size of the sample (i.e., twice the kernel support), and
# n is a sequence of integer indices that specifies the corner of the loaded tensor.
# If V has the type "image(d)[shp]", then n has type "int[n]" and the resulting tensor
# has the type "tensor[shp,s^d]".
LoadVoxels : ImageInfo.t * int : 1 : 2 : load a cube of voxels
#
# LoadVoxelsWithCtl<I,s,ctl>(V,n) -- like LoadVoxels, but it also uses the specified
# index control to deal with out-of-bounds indices.
LoadVoxelsWithCtl : ImageInfo.t * int * idxctl : 1 : 2 : load a cube of voxels
#
# Inside<I,s>(x,V) -- tests to see if the image-space position x is inside the domain of V.
# I is the image info for V, s is the size of the sample to be loaded (see LoadVoxels)
Inside : ImageInfo.t * int : 1 : 2 :
#
# IndexInside<I,s>(n,V) -- tests to see if the index sequence n is inside the domain of V.
# I is the image info for V, s is the size of the sample to be loaded (see LoadVoxels)
IndexInside : ImageInfo.t * int : 1 : 2 :
#
# ImageDim<I,i>(V) -- returns the i'th dimension of the image
ImageDim : ImageInfo.t * int : 1 : 1 :
#
# BorderCtlDefault<I>(V,t) -- wrap the image V with the border control that returns a default value t
BorderCtlDefault : ImageInfo.t : 1 : 2 :
# BorderCtlClamp<I>(V) -- wrap the image V with the border control that clamps the index
BorderCtlClamp : ImageInfo.t : 1 : 1 :
# BorderCtlMirror<I>(V) -- wrap the image V with the border control that mirrors the index
BorderCtlMirror : ImageInfo.t : 1 : 1 :
# BorderCtlWrap<I>(V) -- wrap the image V with the border control that wraps the index
BorderCtlWrap : ImageInfo.t : 1 : 1 :
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
