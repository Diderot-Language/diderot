/*! \file image.cxx
 *
 * Utility code to support images in Diderot.  Most of the actual implementation can be
 * found in diderot/image-inst.hxx; this file just contains the shared non-template code.
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include <cstdlib>
#include "diderot/image.h"

namespace diderot {

    namespace __details {

        void load_transform_1d (const Nrrd *nin, double &s, double &t)
        {
          // compute the offset to the first space axis
            int base = nin->dim - nin->spaceDim;

          // Image axis Scaling and Rotation
            s = nin->axis[base+0].spaceDirection[0];

          // Image location
            t = nin->spaceOrigin[0];
        }


        void load_transform_2d (const Nrrd *nin, mat2x2<double> &m, real2<double> &t)
        {
#define M(i,j)  m[(i)*2+(j)]

          // compute the offset to the first space axis
            int base = nin->dim - nin->spaceDim;

          // Image axis Scaling and Rotation
            M(0,0) = nin->axis[base+0].spaceDirection[0];
            M(1,0) = nin->axis[base+0].spaceDirection[1];
            M(0,1) = nin->axis[base+1].spaceDirection[0];
            M(1,1) = nin->axis[base+1].spaceDirection[1];

          // Image location
            t[0] = nin->spaceOrigin[0];
            t[1] = nin->spaceOrigin[1];

#undef M
        }

        inline double det2 (double a, double b, double c, double d)
        {
            return (a*d - b*c);
        }

        void invert2x2 (mat2x2<double> &i, mat2x2<double> &m)
        {
            double scale = 1.0 / det2(m[0], m[1], m[2], m[3]);

            i[0] =  scale * m[3];
            i[1] = -scale * m[1];
            i[2] = -scale * m[2];
            i[3] =  scale * m[0];
        }

        void load_transform_3d (const Nrrd *nin, mat3x3<double> &m, real3<double> &t)
        {
#define M(i,j)  m[3*(i)+(j)]

          // compute the offset to the first space axis
            int base = nin->dim - nin->spaceDim;

          // Image axis Scaling and Rotation
            M(0,0) = nin->axis[base+0].spaceDirection[0];
            M(1,0) = nin->axis[base+0].spaceDirection[1];
            M(2,0) = nin->axis[base+0].spaceDirection[2];
            M(0,1) = nin->axis[base+1].spaceDirection[0];
            M(1,1) = nin->axis[base+1].spaceDirection[1];
            M(2,1) = nin->axis[base+1].spaceDirection[2];
            M(0,2) = nin->axis[base+2].spaceDirection[0];
            M(1,2) = nin->axis[base+2].spaceDirection[1];
            M(2,2) = nin->axis[base+2].spaceDirection[2];

          // Image location
            t[0] = nin->spaceOrigin[0];
            t[1] = nin->spaceOrigin[1];
            t[2] = nin->spaceOrigin[2];

#undef M
        }

        /*! \brief compute the inverse of \arg m, storing the result in \arg i.
         *  \param m the matrix to invert
         *  \param i the inverted matrix
         */
        void invert3x3 (mat3x3<double> &i, mat3x3<double> &m)
        {
            double det =
                    ( m[0]*m[4]*m[8]
                    + m[3]*m[7]*m[2]
                    + m[6]*m[1]*m[5]
                    - m[6]*m[4]*m[2]
                    - m[3]*m[1]*m[8]
                    - m[0]*m[7]*m[5]);
            double scale = 1.0 / det;

            i[0] = scale * det2 (m[4], m[5], m[7], m[8]);
            i[1] = scale * det2 (m[2], m[1], m[8], m[7]);
            i[2] = scale * det2 (m[1], m[2], m[4], m[5]);
            i[3] = scale * det2 (m[5], m[3], m[8], m[6]);
            i[4] = scale * det2 (m[0], m[2], m[6], m[8]);
            i[5] = scale * det2 (m[2], m[0], m[5], m[3]);
            i[6] = scale * det2 (m[3], m[4], m[6], m[7]);
            i[7] = scale * det2 (m[1], m[0], m[7], m[6]);
            i[8] = scale * det2 (m[0], m[1], m[3], m[4]);
        }

    } // namespace __details

/******************** nrrd_proxy ********************/

    bool nrrd_proxy::check_nrrd (struct world_base *wrld, const Nrrd *nin)
    {
      // compute the offset to the first space axis
        int base = nin->dim - nin->spaceDim;

      // check that the image format is what we expect
        if (nin->spaceDim != this->_dim) {
            wrld->error ("nrrd has unexpected dimension %d, expected %d\n",
                nin->spaceDim, this->_dim);
            return true;
        }

      // check the number of elements per voxel
        int nElems = 1;
        for (int i = 0;  i < base;  i++) {
            nElems *= nin->axis[i].size;
        }
        if (nElems != this->_voxelSz) {
            wrld->error ("nrrd has %d elements per voxel (expected %d)\n",
                nElems, this->_voxelSz);
            return true;
        }

      // check the axis kinds and sizes
        bool bad = false;
        for (int i = 0;  i < this->_dim;  i++) {
            if (nin->axis[base+i].kind != nrrdKindSpace) {
                wrld->error ("nrrd axis[%d] has kind %s (expected %s)\n",
                    i, airEnumStr(nrrdKind, nin->axis[base+i].kind),
                    airEnumStr(nrrdKind, nrrdKindSpace));
                bad = true;
            }
            if (nin->axis[base+i].size != this->_sizes[i]) {
                wrld->error ("nrrd axis[%d] has size %d (expected %d)\n",
                    i, nin->axis[base+i].size, this->_sizes[i]);
                bad = true;
            }
        }

        return bad;
    }

} // namespace Diderot
