/*! \file image-inst.hxx
 *
 * This file contains the template code for implementing the image template classes.
 * It is included by the "options.hxx" file so that code can be instantiated.
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_IMAGE_INST_HXX_
#define _DIDEROT_IMAGE_INST_HXX_

#ifndef _DIDEROT_IMAGE_HXX_
# error image-inst.hxx should not be directly included
#endif

namespace diderot {

/******************** 1D images ********************/

    namespace __details {

        void load_transform_1d (const Nrrd *nin, double &s, double &t);

    } // namespace __details

    template <typename REAL, typename TY, int VOXSZ>
    bool image1d<REAL,TY,VOXSZ>::load (struct world_base *wrld, std::string const &name, nrrd_proxy *info)
    {
        Nrrd *nin = wrld->load_nrrd_file (name);
        if (nin == nullptr)
            return true;
        else {
          // normalize the meta-data first
            if (wrld->normalize_nrrd_meta_data (nin)) {
                return true;
            }
            bool sts = this->load (wrld, nin, info);
            nrrdNix (nin);
            return sts;
        }
    }

    template <typename REAL, typename TY, int VOXSZ>
    bool image1d<REAL,TY,VOXSZ>::load (struct world_base *wrld, const Nrrd *nin, nrrd_proxy *info)
    {
      // compute the offset to the first space axis
        int base = nin->dim - nin->spaceDim;

      // check validity of nin
        if (info != nullptr) {
            if (info->check_nrrd(wrld, nin)) {
                return true;
            }
        }
        else {
            nrrd_proxy proxy(nin->axis[base].size, VOXSZ);
            if (proxy.check_nrrd(wrld, nin)) {
                return true;
            }
        }

      // check the voxel element type
        if (nin->type != image1d<REAL,TY,VOXSZ>::traits::type) {
            wrld->error ("nrrd has unexpected sample type %s; expected %s\n",
                ((0 <= nin->type) && (nin->type < nrrdTypeLast))
                    ? __details::nrrd_type_info[nin->type].name
                    : "unknown",
                __details::nrrd_type_info[image1d<REAL,TY,VOXSZ>::traits::type].name);
            return true;
        }

        this->_img = new __details::image1d<REAL>(
            nin->axis[base+0].size,
            nrrdElementSize(nin) * nrrdElementNumber(nin),
            nin->data);

      // from the Nrrd file, we load the scaling and translation
        double s, t;

        __details::load_transform_1d (nin, s, t);

        this->_img->_s = static_cast<REAL>(1.0 / s);
        this->_img->_t = static_cast<REAL>(-t / s);

        return false;
    }

/******************** 2D images ********************/

    namespace __details {

        void load_transform_2d (const Nrrd *nin, mat2x2<double> &m, real2<double> &t);

        /*! \brief compute the inverse of \arg m, storing the result in \arg i.
         *  \param m the matrix to invert
         *  \param i the inverted matrix
         */
        void invert2x2 (mat2x2<double> &i, mat2x2<double> &m);

    } // namespace __details

    template <typename REAL, typename TY, int VOXSZ>
    bool image2d<REAL,TY,VOXSZ>::load (struct world_base *wrld, std::string const &name, nrrd_proxy *info)
    {
        Nrrd *nin = wrld->load_nrrd_file (name);
        if (nin == nullptr)
            return true;
        else {
          // normalize the meta-data first
            if (wrld->normalize_nrrd_meta_data (nin)) {
                return true;
            }
            bool sts = this->load (wrld, nin, info);
            nrrdNix (nin);
            return sts;
        }
    }

    template <typename REAL, typename TY, int VOXSZ>
    bool image2d<REAL,TY,VOXSZ>::load (struct world_base *wrld, const Nrrd *nin, nrrd_proxy *info)
    {
      // compute the offset to the first space axis
        int base = nin->dim - nin->spaceDim;

      // check validity of nin
        if (info != nullptr) {
            if (info->check_nrrd(wrld, nin)) {
                return true;
            }
        }
        else {
            nrrd_proxy proxy(nin->axis[base].size, nin->axis[base+1].size, VOXSZ);
            if (proxy.check_nrrd(wrld, nin)) {
                return true;
            }
        }
        
      // check the voxel element type
        if (nin->type != image2d<REAL,TY,VOXSZ>::traits::type) {
            wrld->error ("nrrd has unexpected sample type %s; expected %s\n",
                ((0 <= nin->type) && (nin->type < nrrdTypeLast))
                    ? __details::nrrd_type_info[nin->type].name
                    : "unknown",
                __details::nrrd_type_info[image2d<REAL,TY,VOXSZ>::traits::type].name);
            return true;
        }

        this->_img = new __details::image2d<REAL>(
            nin->axis[base+0].size,
            nin->axis[base+1].size,
            nrrdElementSize(nin) * nrrdElementNumber(nin),
            nin->data);

      // from the Nrrd file, we load the affine image-to-world transform matrix
        mat2x2<double> m;               // rotation and scaling
        real2<double> t;                // translation part of the transform
        __details::load_transform_2d (nin, m, t);

      // compute inverse of m, which is the transform from the world basis to the image basis
        mat2x2<double> mInv;
        __details::invert2x2 (mInv, m);

      // copy inverted matrix into the image data structure
        for (int i = 0;  i < 2;  i++) {
            for (int j = 0;  j < 2;  j++) {
                REAL r = static_cast<REAL>(mInv[2*i+j]);
                this->_img->_w2i[2*i+j] = r;
                this->_img->_w2iT[2*j+i] = r;   // transpose
            }
        }

      // transform the translation vector: inv([M t]) = [inv(M) -inv(M)t]
        for (int i = 0;  i < 2;  i++) {
            double sum = 0;
            for (int j = 0;  j < 2;  j++) {
                sum += mInv[2*i+j] * t[j];
            }
            this->_img->_tVec[i] = static_cast<REAL>(-sum);
        }

        return false;
    }

/******************** 3D images ********************/

    namespace __details {

        void load_transform_3d (const Nrrd *nin, mat3x3<double> &m, real3<double> &t);

        /*! \brief compute the inverse of \arg m, storing the result in \arg i.
         *  \param m the matrix to invert
         *  \param i the inverted matrix
         */
        void invert3x3 (mat3x3<double> &i, mat3x3<double> &m);

    } // namespace __details

    template <typename REAL, typename TY, int VOXSZ>
    bool image3d<REAL,TY,VOXSZ>::load (struct world_base *wrld, std::string const &name, nrrd_proxy *info)
    {
        Nrrd *nin = wrld->load_nrrd_file (name);
        if (nin == nullptr)
            return true;
        else {
          // normalize the meta-data first
            if (wrld->normalize_nrrd_meta_data (nin)) {
                return true;
            }
            bool sts = this->load (wrld, nin, info);
            nrrdNix (nin);
            return sts;
        }
    }

    template <typename REAL, typename TY, int VOXSZ>
    bool image3d<REAL,TY,VOXSZ>::load (struct world_base *wrld, const Nrrd *nin, nrrd_proxy *info)
    {
      // compute the offset to the first space axis
        int base = nin->dim - nin->spaceDim;

      // check validity of nin
        if (info != nullptr) {
            if (info->check_nrrd(wrld, nin)) {
                return true;
            }
        }
        else {
            nrrd_proxy proxy(nin->axis[base].size, nin->axis[base+1].size, nin->axis[base+2].size, VOXSZ);
            if (proxy.check_nrrd(wrld, nin)) {
                return true;
            }
        }

      // check the voxel element type
        if (nin->type != image3d<REAL,TY,VOXSZ>::traits::type) {
            wrld->error ("nrrd has unexpected sample type %s; expected %s\n",
                ((0 <= nin->type) && (nin->type < nrrdTypeLast))
                    ? __details::nrrd_type_info[nin->type].name
                    : "unknown",
                __details::nrrd_type_info[image3d<REAL,TY,VOXSZ>::traits::type].name);
            return true;
        }

        this->_img = new __details::image3d<REAL>(
            nin->axis[base+0].size,
            nin->axis[base+1].size,
            nin->axis[base+2].size,
            nrrdElementSize(nin) * nrrdElementNumber(nin),
            nin->data);

      // from the Nrrd file, we load the affine image-to-world transform matrix
        mat3x3<double> m;               // rotation and scaling
        real3<double> t;                // translation part of the transform
        __details::load_transform_3d (nin, m, t);

      // compute inverse of m, which is the transform from the world basis to the image basis
        mat3x3<double> mInv;
        __details::invert3x3 (mInv, m);

      // copy results into the image data structure
        for (int i = 0;  i < 3;  i++) {
            for (int j = 0;  j < 3;  j++) {
                REAL r = static_cast<REAL>(mInv[3*i+j]);
                this->_img->_w2i[3*i+j]  = r;
                this->_img->_w2iT[3*j+i] = r;   // transpose
            }
        }

      // transform the translation vector: inv([M t]) = [inv(M) -inv(M)t]
        for (int i = 0;  i < 3;  i++) {
            double sum = 0;
            for (int j = 0;  j < 3;  j++) {
                sum += mInv[3*i+j] * t[j];
            }
            this->_img->_tVec[i] = static_cast<REAL>(-sum);
        }

        return false;
    }

/******************** image_traits ********************/

  // instantiate the traits by sample type
#   define IMAGE_TRAITS(TY, NRRD_TY)                    \
        template<>                                      \
        struct image_traits<TY> {                       \
            using value_type = TY;                      \
            static const int type = NRRD_TY;            \
        }

    IMAGE_TRAITS(int8_t, nrrdTypeChar);
    IMAGE_TRAITS(uint8_t, nrrdTypeUChar);
    IMAGE_TRAITS(int16_t, nrrdTypeShort);
    IMAGE_TRAITS(uint16_t, nrrdTypeUShort);
    IMAGE_TRAITS(int32_t, nrrdTypeInt);
    IMAGE_TRAITS(uint32_t, nrrdTypeUInt);
    IMAGE_TRAITS(int64_t, nrrdTypeLLong);
    IMAGE_TRAITS(uint64_t, nrrdTypeULLong);
    IMAGE_TRAITS(float, nrrdTypeFloat);
    IMAGE_TRAITS(double, nrrdTypeDouble);

#undef IMAGE_TRAITS

} // namespace diderot

#endif // !_DIDEROT_IMAGE_INST_HXX_
