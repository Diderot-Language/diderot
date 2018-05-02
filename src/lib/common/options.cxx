/*! \file options.cxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include "diderot/options.hxx"

#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstring>
#include <teem/air.h>
#include <teem/hest.h>

namespace diderot {

    namespace __details {

        /*! The base struct for representing information about a single command-line option.
         */
        struct option_base {
            std::string         _name;          //! the option name
            std::string         _desc;          //! the option description
            bool                _hasDflt;       //! true if the option has a default value
            void                *_gv;           //! pointer to the global variable being set

            option_base (std::string const &name, std::string const &desc, void *gv, bool hasDflt)
                : _name(name), _desc(desc), _hasDflt(hasDflt), _gv(gv)
            { }

            virtual ~option_base () { }

          // register_opt the option with hest
            virtual void register_opt (hestOpt **) = 0;
          // copy the value specified on the command-line to the global input variable
            virtual void init_input () = 0;
        };

        /********** flag options **********/

        struct option_flag : public option_base {
            int                 _val;

            option_flag (std::string const &name, std::string const &desc, void *gv)
                : option_base (name, desc, gv, false)
            { }
            ~option_flag () { }

            bool *var () const { return reinterpret_cast<bool *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_flag::register_opt (hestOpt **hOpt)
        {
            hestOptAdd (
                hOpt, this->_name.c_str(), nullptr, airTypeInt, 0, 0, &this->_val,
                nullptr, this->_desc.c_str());
        }

        void option_flag::init_input ()
        {
            *this->var() = (this->_val ? true : false);
        }

        /********** boolean options **********/

        struct option_bool : public option_base {
            int                 _val;

            option_bool (std::string const &name, std::string const &desc, bool *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt)
            { }
            ~option_bool () { }

            bool *var () const { return reinterpret_cast<bool *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_bool::register_opt (hestOpt **hOpt)
        {
            hestOptAdd (
                hOpt, this->_name.c_str(), "bool", airTypeBool, 1, 1, &this->_val,
                this->_hasDflt ? (*this->var() ? "true" : "false") : nullptr,
                this->_desc.c_str());
        }

        void option_bool::init_input ()
        {
            *this->var() = (this->_val ? true : false);
        }

        /********** 32-bit integer options **********/

        struct option_int32 : public option_base {
            int32_t             _val;

            option_int32 (std::string const &name, std::string const &desc, int32_t *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt)
            { }
            ~option_int32 () { }

            int32_t *var () const { return reinterpret_cast<int32_t *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_int32::register_opt (hestOpt **hOpt)
        {
            if (this->_hasDflt) {
                std::ostringstream os;
                os << *this->var();
                hestOptAdd (
                    hOpt, this->_name.c_str(), "int", airTypeInt, 1, 1, &this->_val,
                    os.str().c_str(), this->_desc.c_str());
            }
            else {
                hestOptAdd (
                    hOpt, this->_name.c_str(), "int", airTypeInt, 1, 1, &this->_val,
                    nullptr, this->_desc.c_str());
            }
        }

        void option_int32::init_input ()
        {
            *this->var() = this->_val;
        }

        /********** 32-bit unsigned integer options **********/

        struct option_uint32 : public option_base {
            uint32_t            _val;

            option_uint32 (std::string const &name, std::string const &desc, uint32_t *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt)
            { }
            ~option_uint32 () { }

            uint32_t *var () const { return reinterpret_cast<uint32_t *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_uint32::register_opt (hestOpt **hOpt)
        {
            if (this->_hasDflt) {
                std::ostringstream os;
                os << *this->var();
                hestOptAdd (
                    hOpt, this->_name.c_str(), "int", airTypeUInt, 1, 1, &this->_val,
                    os.str().c_str(), this->_desc.c_str());
            }
            else {
                hestOptAdd (
                    hOpt, this->_name.c_str(), "int", airTypeUInt, 1, 1, &this->_val,
                    nullptr, this->_desc.c_str());
            }
        }

        void option_uint32::init_input ()
        {
            *this->var() = this->_val;
        }

        /********** 64-bit integer options **********/

        struct option_int64 : public option_base {
            int64_t             _val;

            option_int64 (std::string const &name, std::string const &desc, int64_t *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt)
            { }
            ~option_int64 () { }

            int64_t *var () const { return reinterpret_cast<int64_t *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_int64::register_opt (hestOpt **hOpt)
        {
            if (this->_hasDflt) {
                std::ostringstream os;
                os << *this->var();
                hestOptAdd (
                    hOpt, this->_name.c_str(), "int", airTypeLongInt, 1, 1, &this->_val,
                    os.str().c_str(), this->_desc.c_str());
            }
            else {
                hestOptAdd (
                    hOpt, this->_name.c_str(), "int", airTypeLongInt, 1, 1, &this->_val,
                    nullptr, this->_desc.c_str());
            }
        }

        void option_int64::init_input ()
        {
            *this->var() = this->_val;
        }

        /********** 32-bit floating-point options **********/

        struct option_real32 : public option_base {
            int                 _dim;
            float               _val[4];

            option_real32 (std::string const &name, std::string const &desc, int dim, float *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt), _dim(dim)
            { }
            ~option_real32 () { }

            float *var () const { return reinterpret_cast<float *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_real32::register_opt (hestOpt **hOpt)
        {
            const char *valName = nullptr;
            if (this->_dim == 1) { valName = "x"; }
            else if (this->_dim == 2) { valName = "x y"; }
            else if (this->_dim == 3) { valName = "x y z"; }
            else if (this->_dim == 4) { valName = "x y z w"; }
            else {
                std::cerr << "illegal dimension " << this->_dim << " for " << this->_name << "\n";
                exit (1);
            }

            if (this->_hasDflt) {
                std::ostringstream os;
                os << this->var()[0];
                for (int i = 1;  i < this->_dim;  i++) {
                    os << " " << this->var()[i];
                }
                hestOptAdd (
                    hOpt, this->_name.c_str(), valName, airTypeFloat, this->_dim, this->_dim,
                    &this->_val, os.str().c_str(), this->_desc.c_str());
            }
            else {
                hestOptAdd (
                    hOpt, this->_name.c_str(), valName, airTypeFloat, this->_dim, this->_dim,
                    &this->_val, nullptr, this->_desc.c_str());
            }
        }

        void option_real32::init_input ()
        {
            for (int i = 0;  i < this->_dim;  i++) {
                this->var()[i] = this->_val[i];
            }
        }

        /********** 64-bit floating-point options **********/

        struct option_real64 : public option_base {
            int                 _dim;
            double              _val[4];

            option_real64 (std::string const &name, std::string const &desc, int dim, double *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt), _dim(dim)
            { }
            ~option_real64 () { }

            double *var () const { return reinterpret_cast<double *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_real64::register_opt (hestOpt **hOpt)
        {
            const char *valName = nullptr;
            if (this->_dim == 1) { valName = "x"; }
            else if (this->_dim == 2) { valName = "x y"; }
            else if (this->_dim == 3) { valName = "x y z"; }
            else if (this->_dim == 4) { valName = "x y z w"; }
            else {
                std::cerr << "illegal dimension " << this->_dim << " for " << this->_name << "\n";
                exit (1);
            }

            if (this->_hasDflt) {
                std::ostringstream os;
                os << this->var()[0];
                for (int i = 1;  i < this->_dim;  i++) {
                    os << " " << this->var()[i];
                }
                hestOptAdd (
                    hOpt, this->_name.c_str(), valName, airTypeDouble, this->_dim, this->_dim, &this->_val,
                    os.str().c_str(), this->_desc.c_str());
            }
            else {
                hestOptAdd (
                    hOpt, this->_name.c_str(), valName, airTypeDouble, this->_dim, this->_dim, &this->_val,
                    nullptr, this->_desc.c_str());
            }
        }

        void option_real64::init_input ()
        {
            for (int i = 0;  i < this->_dim;  i++) {
                this->var()[i] = this->_val[i];
            }
        }

        /********** string options **********/

        struct option_string : public option_base {
            char                *_val;

            option_string (std::string const &name, std::string const &desc, void *gv, bool hasDflt)
                : option_base (name, desc, gv, hasDflt)
            { }
            ~option_string () { }

            std::string *var () const { return reinterpret_cast<std::string *>(this->_gv); }

            void register_opt (hestOpt **);
            void init_input ();
        };

        void option_string::register_opt (hestOpt **hOpt)
        {
            if (this->_hasDflt) {
                hestOptAdd (
                    hOpt, this->_name.c_str(), "str", airTypeString, 1, 1, &this->_val,
                    this->var()->c_str(), this->_desc.c_str());
            }
            else {
                hestOptAdd (
                    hOpt, this->_name.c_str(), "str", airTypeString, 1, 1, &this->_val,
                    nullptr, this->_desc.c_str());
            }
        }

        void option_string::init_input ()
        {
            *this->var() = this->_val;
        }

    } // namespace __details

    /******************** class options ********************/

    options::options (const char *info)
        : _progInfo((info == nullptr) ? "" : info)
    {
    }

    options::~options ()
    {
        for (auto it = this->_opts.begin();  it != this->_opts.end();  it++) {
            delete (*it);
        }
    }

    void options::addFlag (std::string const &name, std::string const &desc, bool *gv)
    {
        this->_opts.push_back (new __details::option_flag(name, desc, gv));
    }

    void options::add (std::string const &name, std::string const &desc, bool *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_bool(name, desc, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, int32_t *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_int32(name, desc, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, uint32_t *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_uint32(name, desc, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, int64_t *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_int64(name, desc, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, float *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_real32(name, desc, 1, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, double *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_real64(name, desc, 1, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, std::string *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_string(name, desc, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, int dim, float *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_real32(name, desc, dim, gv, hasDflt));
    }

    void options::add (std::string const &name, std::string const &desc, int dim, double *gv, bool hasDflt)
    {
        this->_opts.push_back (new __details::option_real64(name, desc, dim, gv, hasDflt));
    }

    void options::process (int argc, const char **argv)
    {
        static const char *me = "Diderot_ProcessOptions";

        airArray *mop = airMopNew();
        hestOpt *hopt = nullptr;
        hestParm *hparam = hestParmNew();
        hparam->noArgsIsNoProblem = AIR_TRUE;
        airMopAdd (mop, hparam, (airMopper)hestParmFree, airMopAlways);

      // add the "--help" option
        int32_t helpFlg;
        hestOptAdd (
            &hopt, "h,help", nullptr, airTypeInt, 0, 0, &helpFlg,
            "false", "print usage information and exit");

        for (auto it = this->_opts.begin();  it != this->_opts.end();  it++) {
            (*it)->register_opt (&hopt);
        }

      // scan for the "-h" or "--help" option.  We have to do this manualy because hest
      // doesn't support a help flag mechanism
        for (int i = 1;  i < argc;  i++) {
            if (strcmp(argv[i], "-h") == 0) {
              // short help message
                hestUsage (stderr, hopt, argv[0], hparam);
                exit (0);
            }
            else if (strcmp(argv[i], "--help") == 0) {
              // long help message
                hestUsage (stderr, hopt, argv[0], hparam);
                hestGlossary (stderr, hopt, hparam);
                exit (0);
            }
        }

     // if we get here, there wasn't a "-h" or "--help" option, so do regular processing
        hestParseOrDie (
            hopt, argc-1, argv+1, hparam, argv[0],
            this->_progInfo.c_str(),
            AIR_TRUE, AIR_TRUE, AIR_TRUE);
        airMopAdd(mop, hopt, (airMopper)hestOptFree, airMopAlways);
        airMopAdd(mop, hopt, (airMopper)hestParseFree, airMopAlways);

      // copy option values back into input variables
        for (auto it = this->_opts.begin();  it != this->_opts.end();  it++) {
            (*it)->init_input ();
        }

        airMopOkay(mop);

    }

} // namespace diderot
