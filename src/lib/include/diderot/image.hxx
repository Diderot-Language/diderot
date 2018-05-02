/*! \file image.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_IMAGE_HXX_
#define _DIDEROT_IMAGE_HXX_

#include <string>
#include <teem/nrrd.h>

#ifndef _DIDEROT_BASE_HXX_
#include "base.hxx"
#endif
#ifndef _DIDEROT_UTIL_HXX_
#include "util.hxx"
#endif
#ifndef _DIDEROT_WORLD_HXX_
#include "world.hxx"
#endif

namespace diderot {

    namespace __details {

      //! clamp an index between 0 and size-1
        inline int index_clamp (int size, int idx)
        {
//            return (idx < 0) ? 0 : (idx < size) ? idx : (size-1);
            idx = (idx < 0) ? 0 : (idx < size) ? idx : (size-1);
            assert ((0 <= idx) && (idx < size));
            return idx;
        }

      // mirror an index if it is out of bounds
        inline int index_mirror (int size, int idx)
        {
            if (idx < 0) {
                idx = -1 - idx;
            }
            if (idx < size) {
                assert ((0 <= idx) && (idx < size));
                return idx;
            }
          // here size <= idx
            int div = idx / size;
            int rem = idx - size * div;
            idx = (div & 1) ? (size - 1) - rem : rem;
            assert ((0 <= idx) && (idx < size));
            return idx;
        }

      // wrap an index if it is out of bounds
        inline int index_wrap (int size, int idx)
        {
            if (idx < 0) {
                return (size - 1) + (idx + 1) % size;
            }
            else if (idx < size) {
                return idx;
            }
            else {
                return idx % size;
            }
        }

      //! \brief base class for image-data objects.  Wraps the raw image data pointer
      //! with a reference count.
      //!
      //! The reference count is used to count global references, which arise from image
      //! creation and from border control wrappers.  Note that we _do not_ count
      //! parameters and local vars, since that would require atomic operations, which
      //! have proven to be a serious sequential bottleneck.
      //!
        struct image_base {
            void                *_data;         //!< the raw image data
            uint32_t            _refCnt;        //!< a count of the _global_ references

            image_base () : _data(nullptr), _refCnt(0) { }
            image_base (void *data) : _data(data), _refCnt(0) { }
            ~image_base ()
            {
                if (this->_data != nullptr) {
                    std::free (this->_data);
                }
            }

          //! decrement the reference count
          //! \return true if the count has gone to zero and the image-data object
          //!   should be deleted.
            bool dec_count ()
            {
                return (--this->_refCnt <= 0);
            }

          //! increment the reference count
            void inc_count ()
            {
                this->_refCnt++;
            }
        };

        template <typename REAL>
        struct image1d : public image_base {
            uint32_t            _dim;           //!< dimension (== 1)
            uint32_t            _size[1];
            size_t              _dataSzb;       //!< size of data in bytes
            REAL                _s;             //!< scaling from world-space to image-space
            REAL                _t;             //!< translation from world-space to image-space

            image1d (uint32_t sz1, size_t nbytes, void *data)
              : image_base(data), _dim(1), _size{sz1}, _dataSzb(nbytes)
            { }
            ~image1d () { }

        };

        template <typename REAL>
        struct image2d : public image_base {
            uint32_t            _dim;           //!< dimension (== 2)
            uint32_t            _size[2];       //!< sizes (fast to slow)
            size_t              _dataSzb;       //!< size of data in bytes
            mat2x2<REAL>        _w2i;           //!< affine tranform from world space to index space.
                                                //!  This is the inverse of the index to world-space
                                                //!  transform that is loaded from the Nrrd file.
            real2<REAL>          _tVec;         //!< translation part of world to index transform
            mat2x2<REAL>        _w2iT;          //!< transpose w2i

            image2d (uint32_t sz1, uint32_t sz2, size_t nbytes, void *data)
              : image_base(data), _dim(2), _size{sz1, sz2}, _dataSzb(nbytes)
            { }
            ~image2d () { }

        };

        template <typename REAL>
        struct image3d : public image_base {
            uint32_t            _dim;           //!< dimension (== 3)
            uint32_t            _size[3];       //!< sizes (fast to slow)
            size_t              _dataSzb;       //!< size of data in bytes
            mat3x3<REAL>        _w2i;           //!< affine tranform from world space to index space.
                                                //!  This is the inverse of the index to world-space
                                                //!  transform that is loaded from the Nrrd file.
            real3<REAL>          _tVec;         //!< translation part of world to index transform
            mat3x3<REAL>        _w2iT;          //!< transpose w2i

            image3d (uint32_t sz1, uint32_t sz2, uint32_t sz3, size_t nbytes, void *data)
              : image_base(data), _dim(3), _size{sz1, sz2, sz3}, _dataSzb(nbytes)
            { }
            ~image3d () { }

        };

    } // namespace __details

    template <typename TY> struct image_traits;
    // using value_type = TY;
    // static const int type = ...;

  //! information about a nrrd file that loaded by the compiler
  //! (see compiler/nrrd/nrrd-info.sml)
    struct nrrd_proxy {
        int             _dim;           //!< dimension
        int             _sizes[3];      //!< dimensions
        int             _voxelSz;       //!< number of elements per voxel

        nrrd_proxy (int x, int vSz) : _dim(1), _sizes{x, 0, 0}, _voxelSz(vSz) { }
        nrrd_proxy (int x, int y, int vSz) : _dim(2), _sizes{x, y, 0}, _voxelSz(vSz) { }
        nrrd_proxy (int x, int y, int z, int vSz) : _dim(3), _sizes{x, y, z}, _voxelSz(vSz) { }

      // check a nrrd against the expected properties
        bool check_nrrd (struct world_base *wrld, const Nrrd *nin);
    };

  // 1D images with sample type TY[VOXSZ] and Diderot real type REAL
    template <typename REAL, typename TY, int VOXSZ>
    class image1d {
      public:

        using traits = image_traits<TY>;

        image1d () : _img(nullptr) { }
        image1d (const image1d &img) : _img(img._img) { }
        ~image1d () { }

        bool load (struct world_base *wrld, std::string const &name, nrrd_proxy *info = nullptr);
        bool load (struct world_base *wrld, const Nrrd *nin, nrrd_proxy *info = nullptr);

      // track global references to images for memory management
        void register_global ();
        void unregister_global ();

        bool inside (int idx, int s) const;
        int clamp (int dim, int idx) const;
        int mirror (int dim, int idx) const;
        int wrap (int dim, int idx) const;

        uint32_t size (uint32_t dim) const;

        const TY *base_addr () const;
        REAL operator[] (uint32_t idx) const;

        REAL world2image () const;
        REAL translate () const;

      private:
        __details::image1d<REAL> *_img;
    };

  // 2D images with sample type TY[VOXSZ] and Diderot real type REAL
    template <typename REAL, typename TY, int VOXSZ>
    class image2d {
      public:

        using traits = image_traits<TY>;

        image2d () : _img(nullptr) { }
        image2d (const image2d &img) : _img(img._img) { }
        ~image2d () { }

        bool load (struct world_base *wrld, std::string const &name, nrrd_proxy *info = nullptr);
        bool load (struct world_base *wrld, const Nrrd *nin, nrrd_proxy *info = nullptr);

      // track global references to images for memory management
        void register_global ();
        void unregister_global ();

        bool inside (array<int,2> const & idx, int s) const;
        int clamp (int dim, int idx) const;
        int mirror (int dim, int idx) const;
        int wrap (int dim, int idx) const;

        uint32_t size (uint32_t dim) const;

        const TY *base_addr () const;
        REAL operator[] (uint32_t idx) const;

        mat2x2<REAL> const &world2image () const;
        real2<REAL> const &translate () const;

      private:
        __details::image2d<REAL> *_img;
    };

  // 3D images with sample type TY[VOXSZ] and Diderot real type REAL
    template <typename REAL, typename TY, int VOXSZ>
    class image3d {
      public:

        using traits = image_traits<TY>;

        image3d () : _img(nullptr) { }
        image3d (const image3d &img) : _img(img._img) { }
        ~image3d () { }

        bool load (struct world_base *wrld, std::string const &name, nrrd_proxy *info = nullptr);
        bool load (struct world_base *wrld, const Nrrd *nin, nrrd_proxy *info = nullptr);

      // track global references to images for memory management
        void register_global ();
        void unregister_global ();

        bool inside (array<int,3> const &idx, int s) const;
        int clamp (int dim, int idx) const;
        int mirror (int dim, int idx) const;
        int wrap (int dim, int idx) const;

        uint32_t size (uint32_t dim) const;

        const TY *base_addr () const;
        REAL operator[] (uint32_t idx) const;

        mat3x3<REAL> const &world2image () const;
        real3<REAL> const &translate () const;

      private:
        __details::image3d<REAL> *_img;
    };

  /***** image1d inline functions *****/
    template <typename REAL, typename TY, int VOXSZ>
    inline void image1d<REAL,TY,VOXSZ>::register_global ()
    {
        this->_img->inc_count();
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline void image1d<REAL,TY,VOXSZ>::unregister_global ()
    {
        if ((this->_img != nullptr) && this->_img->dec_count()) { delete this->_img; }
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline bool image1d<REAL,TY,VOXSZ>::inside (int idx, int s) const
    {
      // NOTE: we cast to signed int to handle the case with it is less than s!
        return ((s-1 <= idx) && (idx < static_cast<int>(this->_img->_size[0] - s)));
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image1d<REAL,TY,VOXSZ>::clamp (int dim, int idx) const
    {
        return __details::index_clamp(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image1d<REAL,TY,VOXSZ>::mirror (int dim, int idx) const
    {
        return __details::index_mirror(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image1d<REAL,TY,VOXSZ>::wrap (int dim, int idx) const
    {
        return __details::index_wrap(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline uint32_t image1d<REAL,TY,VOXSZ>::size (uint32_t dim) const
    {
        return this->_img->_size[dim];
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline const TY *image1d<REAL,TY,VOXSZ>::base_addr () const
    {
        return reinterpret_cast<TY *>(this->_img->_data);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline REAL image1d<REAL,TY,VOXSZ>::operator[] (uint32_t idx) const
    {
        assert ((0 <= idx) && (idx < this->_img->_dataSzb/sizeof(REAL)));
        return static_cast<REAL>(this->base_addr()[idx]);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline REAL image1d<REAL,TY,VOXSZ>::world2image () const
    {
        return this->_img->_s;
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline REAL image1d<REAL,TY,VOXSZ>::translate () const
    {
        return this->_img->_t;
    }


  /***** image2d inline functions *****/
    template <typename REAL, typename TY, int VOXSZ>
    inline void image2d<REAL,TY,VOXSZ>::register_global ()
    {
        this->_img->inc_count();
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline void image2d<REAL,TY,VOXSZ>::unregister_global ()
    {
        if ((this->_img != nullptr) && this->_img->dec_count()) { delete this->_img; }
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline bool image2d<REAL,TY,VOXSZ>::inside (array<int,2> const &idx, int s) const
    {
      // NOTE: we cast to signed int to handle the case with it is less than s!
        return ((s-1 <= idx[0]) && (idx[0] < static_cast<int>(this->_img->_size[0] - s))
            &&  (s-1 <= idx[1]) && (idx[1] < static_cast<int>(this->_img->_size[1] - s)));
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image2d<REAL,TY,VOXSZ>::clamp (int dim, int idx) const
    {
        return __details::index_clamp(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image2d<REAL,TY,VOXSZ>::mirror (int dim, int idx) const
    {
        return __details::index_mirror(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image2d<REAL,TY,VOXSZ>::wrap (int dim, int idx) const
    {
        return __details::index_wrap(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline uint32_t image2d<REAL,TY,VOXSZ>::size (uint32_t dim) const
    {
        return this->_img->_size[dim];
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline const TY *image2d<REAL,TY,VOXSZ>::base_addr () const
    {
        return reinterpret_cast<TY *>(this->_img->_data);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline REAL image2d<REAL,TY,VOXSZ>::operator[] (uint32_t idx) const
    {
        assert ((0 <= idx) && (idx < this->_img->_dataSzb/sizeof(TY)));
        return static_cast<REAL>(this->base_addr()[idx]);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline mat2x2<REAL> const &image2d<REAL,TY,VOXSZ>::world2image () const
    {
        return this->_img->_w2i;
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline real2<REAL> const &image2d<REAL,TY,VOXSZ>::translate () const
    {
        return this->_img->_tVec;
    }


  /***** image2d inline functions *****/
    template <typename REAL, typename TY, int VOXSZ>
    inline void image3d<REAL,TY,VOXSZ>::register_global ()
    {
        this->_img->inc_count();
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline void image3d<REAL,TY,VOXSZ>::unregister_global ()
    {
        if ((this->_img != nullptr) && this->_img->dec_count()) { delete this->_img; }
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline bool image3d<REAL,TY,VOXSZ>::inside (array<int,3> const &idx, int s) const
    {
      // NOTE: we cast to signed int to handle the case with it is less than s!
        return ((s-1 <= idx[0]) && (idx[0] < static_cast<int>(this->_img->_size[0] - s))
            &&  (s-1 <= idx[1]) && (idx[1] < static_cast<int>(this->_img->_size[1] - s))
            &&  (s-1 <= idx[2]) && (idx[2] < static_cast<int>(this->_img->_size[2] - s)));
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image3d<REAL,TY,VOXSZ>::clamp (int dim, int idx) const
    {
        return __details::index_clamp(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image3d<REAL,TY,VOXSZ>::mirror (int dim, int idx) const
    {
        return __details::index_mirror(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline int image3d<REAL,TY,VOXSZ>::wrap (int dim, int idx) const
    {
        return __details::index_wrap(this->_img->_size[dim], idx);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline uint32_t image3d<REAL,TY,VOXSZ>::size (uint32_t dim) const
    {
        return this->_img->_size[dim];
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline const TY *image3d<REAL,TY,VOXSZ>::base_addr () const
    {
        return reinterpret_cast<TY *>(this->_img->_data);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline REAL image3d<REAL,TY,VOXSZ>::operator[] (uint32_t idx) const
    {
        return static_cast<REAL>(this->base_addr()[idx]);
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline mat3x3<REAL> const &image3d<REAL,TY,VOXSZ>::world2image () const
    {
        return this->_img->_w2i;
    }

    template <typename REAL, typename TY, int VOXSZ>
    inline real3<REAL> const &image3d<REAL,TY,VOXSZ>::translate () const
    {
        return this->_img->_tVec;
    }

} // namespace diderot

#include "image-inst.hxx"

#endif //! _DIDEROT_IMAGE_HXX_
