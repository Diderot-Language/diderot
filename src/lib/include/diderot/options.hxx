/*! \file options.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_OPTIONS_HXX_
#define _DIDEROT_OPTIONS_HXX_

#ifndef _DIDEROT_CONFIG_H_
#include "config.h"
#endif

#include <vector>
#include <string>

namespace diderot {

 // options_base is the base class for type-specific option info
    namespace __details { struct option_base; }

    class options {
      public:
        options (const char *info = nullptr);
        ~options ();

        void addFlag (std::string const &name, std::string const &desc, bool *gv);
        void add (std::string const &name, std::string const &desc, bool *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, int32_t *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, uint32_t *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, int64_t *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, float *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, double *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, int dim, float *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, int dim, double *gv, bool hasDflt);
        void add (std::string const &name, std::string const &desc, std::string *gv, bool hasDflt);

        void process (int argc, const char **argv);

      private:
        std::string _progInfo;
        std::vector<__details::option_base *> _opts;
    };

} // namespace diderot

#endif // !_DIDEROT_OPTIONS_HXX_
