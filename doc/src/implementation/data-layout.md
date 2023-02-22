# Data Layout in Diderot

This note describes the data layout conventions used in the Diderot
implementation for images and tensors.

## Tensors

The axes of tensors and tensor fields are listed from slowest to fastest (which is the opposite of nrrd files).

### Tensor construction

The CFG IR expression

```
T = CONS[T_1, ..., T_d]
```

will construct a tensor with type `tensor[d_1,...,d_n,d]`, where the `T_i`
tensors have the type `tensor[d_1,...,d_n]`.  Thus the dimensions of a tensor
are listed from slowest to fastest (the opposite of Nrrd file headers).

### Tensor indexing

If we have

````diderot
tensor[d_1,...,d_n] T;
````

then the expression

````diderot
T[i_1,...,i_n]
````

is translated to the following address arithmetic:

````
T + i_n + d_n * (i_{n-1} + d_{n-1} * ( ... d_2 * i_1) ... )
````

## Differentiation

The gradiant operator returns a field of higher order than its argument.
For example

````diderot
field#2(3)[2] F;
field#1(3)[2,3] G = ∇ F;
tensor[2,3] T = G(x);
````

Again, the dimensions are slowest to fastest, so T can be thought of
as a two-element array of three-vectors.

## Images and Nrrd Files

Image values are represented using the [Nrrd file format](http://teem.sourceforge.net/nrrd/format.html)
from teem.

### `LoadVoxels`

The nrrd convention is to list indices from fastest to slowest (opposite
the C convention).  If we are sampling a `4x4` grid of voxels from an image
with type `image(2)[2])`, then the data will have the following layout in the nrrd file:
````
    ... x00 y00 x10 y10 x20 y20 x30 y30 ...
    ... x01 y01 x11 y11 x21 y21 x31 y31 ...
    ... x02 y02 x12 y12 x22 y22 x32 y32 ...
    ... x03 y03 x13 y13 x23 y23 x33 y33 ...
````

When we load these voxels into memory, however, we swizzle them to fit the type `tensor[2,4,4]`.
The following IR expression shows how the voxel tensor would be constructed:

```
CONS[
  CONS[
    CONS[x00, x10, x20, x30],
    CONS[x01, x11, x21, x31],
    CONS[x02, x12, x22, x32],
    CONS[x03, x13, x23, x33]],
  CONS[
    CONS[y00, y10, y20, y30],
    CONS[y01, y11, y21, y31],
    CONS[y02, y12, y22, y32],
    CONS[y03, y13, y23, y33]]
]
```
