dnl Process this file with autoconf to produce a configure script.
dnl
dnl This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
dnl
dnl COPYRIGHT (c) 2019 The University of Chicago
dnl All rights reserved.
dnl

AC_INIT(diderot,2.0,)

AC_PREREQ(2.60)
AC_COPYRIGHT([[COPYRIGHT (c) 2019 The University of Chicago]])
AC_CONFIG_SRCDIR(src/compiler/options/ctl.sml)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

dnl
dnl set the following to the name of the branch
dnl
BRANCH="master"
AC_SUBST(BRANCH)

dnl
dnl include additional macros
dnl
sinclude(config/ax_check_compile_flag.m4)
sinclude(config/ax_gcc_func_attribute.m4)
sinclude(config/ax_gcc_x86_cpuid.m4)
sinclude(config/ax_gcc_x86_avx_xgetbv.m4)
sinclude(config/ax_ext.m4)
sinclude(config/acx_pthread.m4)
sinclude(config/check_smlnj.m4)
sinclude(config/check_smlnj_heap_suffix.m4)
sinclude(config/ax_check_framework.m4)
sinclude(config/ax_cxx_compile_stdcxx.m4)

dnl
dnl get host information
dnl
AC_CANONICAL_HOST

#################### Paths configuration ####################

DIDEROT_ROOT=`pwd`
DIDEROT_DOCDIR=$DIDEROT_ROOT/doc
DIDEROT_SRCDIR=$DIDEROT_ROOT/src
DIDEROT_LIBDIR=$DIDEROT_ROOT/lib
DIDEROT_BINDIR=$DIDEROT_ROOT/bin
DIDEROT_MKDIR=$DIDEROT_ROOT/mk

AC_SUBST(DIDEROT_ROOT)
AC_SUBST(DIDEROT_DOCDIR)
AC_SUBST(DIDEROT_SRCDIR)
AC_SUBST(DIDEROT_LIBDIR)
AC_SUBST(DIDEROT_BINDIR)
AC_SUBST(DIDEROT_MKDIR)

#################### end Paths configuration ####################

dnl check for standard programs
dnl
AC_PROG_CPP
AC_PROG_CC([clang gcc cc])
AC_PROG_CXX([clang++ g++ c++])
AX_CXX_COMPILE_STDCXX(11,[],[mandatory])
AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl extra compiler options
dnl
CXXFLAGS_BASE="-Wreturn-type -Wuninitialized"

AC_LANG_PUSH([C++])
AX_GCC_FUNC_ATTRIBUTE(noreturn)
AC_LANG_POP([C++])

dnl check for SIMD support
dnl
AX_EXT

AC_ARG_ENABLE([32-bit],
  AC_HELP_STRING([--enable-32-bit], [configure Diderot to produce 32-bit executables]),
  [CXXFLAG_Mxx="-m32"],
  [CXXFLAG_Mxx="-m64"])

CXXFLAGS="$CXXFLAG_Mxx $CXXFLAGS_BASE"
AC_SUBST(CXXFLAG_Mxx)
AC_SUBST(CXXFLAGS_BASE)

dnl
dnl run the compiler tests in 64-bit mode
dnl
save_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS $CXXFLAG_Mxx"

dnl
dnl check for gcc builtin atomic operations
dnl
AC_MSG_CHECKING([for builtin atomic operations])
AC_LINK_IFELSE(
   [AC_LANG_PROGRAM([], [int foo1; int foo2 = __sync_fetch_and_add(&foo1, 1);])],
   AC_MSG_RESULT(yes)
   AC_DEFINE(
    [HAVE_BUILTIN_ATOMIC_OPS], 1,
    [Define to 1 if gcc compiler provides atomic operations.]),
   AC_MSG_RESULT(no))

dnl
dnl check endianess
dnl
AC_C_BIGENDIAN

#
# determine the sizes of the standard C types
AC_CHECK_SIZEOF([int])
SIZEOF_C_INT=$ac_cv_sizeof_int
AC_CHECK_SIZEOF([long])
SIZEOF_C_LONG=$ac_cv_sizeof_long
AC_CHECK_SIZEOF([float])
SIZEOF_C_FLOAT=$ac_cv_sizeof_float
AC_CHECK_SIZEOF([double])
SIZEOF_C_DOUBLE=$ac_cv_sizeof_double

AC_SUBST(SIZEOF_C_INT)
AC_SUBST(SIZEOF_C_LONG)
AC_SUBST(SIZEOF_C_FLOAT)
AC_SUBST(SIZEOF_C_DOUBLE)

dnl
dnl restore CFLAGS
dnl
CFLAGS=$save_CFLAGS

dnl
dnl check for <stdbool.h>, which we use in the C API
dnl
AC_HEADER_STDBOOL

dnl
dnl the C boolean type to use in generated library headers
dnl
if test x"$ac_cv_header_stdbool_h" = xyes ; then
  BOOLTY="bool"
elif test "x$ac_cv_type__Bool" = xyes; then
  BOOLTY="_Bool"
else
  BOOLTY="int"
fi
AC_SUBST(BOOLTY)

dnl
dnl check for pthreads support
dnl
ACX_PTHREAD

dnl
dnl check for various library routines etc.
dnl
AC_CHECK_FUNCS(posix_memalign)
if test x"$ac_cv_func_posix_memalign" != xyes ; then
  AC_CHECK_FUNCS(memalign)
  if test x"$ac_cv_func_posix_memalign" != xyes ; then
    AC_CHECK_FUNCS(valloc)
  fi
fi

AC_CHECK_FUNCS(mach_absolute_time)
AC_CHECK_TYPES([struct timespec])
AC_CHECK_FUNCS(pthread_barrier_init)

dnl
dnl check for clock_gettime.  On Linux, this requires librt.
dnl
save_LIBS=$LIBS
LIBS="-lrt $LIBS"
AC_MSG_CHECKING([for clock_gettime])
AC_LINK_IFELSE(
  [AC_LANG_PROGRAM(
    [[#include <time.h>]],
    [[struct timespec t; clock_gettime(CLOCK_REALTIME, &t); ]])],
  [ ac_cv_func_clock_gettime=yes;
    AC_DEFINE(HAVE_CLOCK_GETTIME, [1], [is clock_gettime available?])],
  [ac_cv_func_clock_gettime=no])
AC_MSG_RESULT($ac_cv_func_clock_gettime)
if test "$ac_cv_func_clock_gettime" = "no" ; then
  LIBS=$save_LIBS
fi

AC_CHECK_FUNCS(pthread_getcpuclockid)
AC_CHECK_FUNCS(sigtimedwait nanosleep)

dnl
dnl check for pthread_setaffinity_np
dnl
save_LIBS=$LIBS
save_CFLAGS=$CFLAGS
LIBS="$PTHREAD_LIBS $LIBS"
CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
AC_MSG_CHECKING([for pthread_setaffinity_np])
AC_LINK_IFELSE(
  [AC_LANG_PROGRAM(
    [[#include <pthread.h>]],
    [[cpu_set_t s; pthread_setaffinity_np(pthread_self(), sizeof(s), &s); ]])],
  [ ac_cv_func_pthread_setaffinity_np=yes;
    AC_DEFINE(HAVE_PTHREAD_SETAFFINITY_NP, [1], [is pthread_setaffinity_np available?])],
  [ac_cv_func_pthread_setaffinity_np=no])
AC_MSG_RESULT($ac_cv_func_pthread_setaffinity_np)
LIBS=$save_LIBS
CFLAGS=$save_CFLAGS

dnl
dnl check for Linux NUMA support (libnuma)
dnl
AC_CHECK_LIB(numa,numa_available)
if test "$ac_cv_lib_numa_numa_available" = "yes" ; then
  PTHREAD_LIBS="$PTHREAD_LIBS -lnuma"
fi

dnl check for /proc/cpuinfo
dnl
AC_CHECK_FILES(/proc/cpuinfo)

dnl check for getcpu and/or sched_getcpu
dnl
hasGetCPU=no;
AC_CHECK_HEADERS([linux/getcpu.h])
if test "x$ac_cv_header_linux_getcpu_h" = xyes; then
  AC_CHECK_FUNCS([getcpu])
  hasGetCPU=yes;
else
  # getcpu is usually called using syscall
  #
  AC_MSG_CHECKING([SYS_getcpu])
  AC_RUN_IFELSE([AC_LANG_SOURCE([[
    #define _GNU_SOURCE
    #include <unistd.h>
    #include <sys/syscall.h>
    int main () {
      unsigned cpu, node;
      return syscall (SYS_getcpu, &cpu, &node, 0);
    }
    ]])],[hasGetCPU=yes],[])
  AC_MSG_RESULT([$hasGetCPU])
  if test x"$hasGetCPU" = xyes ; then
    AC_DEFINE([HAVE_SYS_GETCPU],[1],[Define if SYS_getcpu is defined in <sys/syscall.h>])
  fi
fi
if test x"$hasGetCPU" != xyes ; then
AC_CHECK_FUNCS([sched_getcpu])
fi

#################### Linking configuration ####################

dnl FIXME: this works for now, but we should probably switch to libtool
dnl
AC_PATH_PROG(LD, [ld], [none])
if test x$LD = xnone ; then
  AC_MSG_ERROR([cannot find ld command])
fi
AC_SUBST(LD)

dnl
dnl OS-specific linking issues
dnl
LD_NEEDS_RPATH=false;
CXXFLAG_fPIC=""
LDFLAG_LIBM=""
case "$host_os" in
  darwin*)
  # On Mac OS X we check for the presence of homebrew, macports, and fink.
  # we set up the search paths in priority homebrew > macports > fink.
    if test -x /sw/bin/fink ; then
      LDPATHS="-L/sw/lib"
    fi
    if test -x /opt/local/bin/port ; then
      LDPATHS="-L/opt/local/lib $LDPATHS"
    fi
    if test -d /usr/local/Cellar -a /usr/local/bin/brew ; then
      LDPATHS="-L/usr/local/lib $LDPATHS"
    fi
    DSLEXT="dylib"
    LD_STATIC=$LD
    LD_DYNAMIC="$CXX -dynamiclib"
  ;;
  linux*)
  # On Linux we include the -rpath option to pick up dynamically-loaded libraries
  # and the -fPIC compiler flag to generate position-indenpendent code
    LD_NEEDS_RPATH=true
    LDFLAG_LIBM="-lm"
    CXXFLAG_fPIC="-fPIC"
    DSLEXT="so"
    LD_STATIC=$LD
    LD_DYNAMIC="$CXX -shared"
  ;;
esac

AC_SUBST(LD_STATIC)
AC_SUBST(LD_DYNAMIC)
AC_SUBST(LDPATHS)
AC_SUBST(LD_NEEDS_RPATH)
AC_SUBST(LDFLAG_LIBM)
AC_SUBST(CXXFLAG_fPIC)
AC_SUBST(DSLEXT)

#################### end Linking configuration ####################

#################### Logging configuration ####################

LOGGING_ENABLED=false
AC_ARG_ENABLE([event-logging],
  AC_HELP_STRING([--enable-event-logging], [include event-logging support for parallel target]),
  [LOGGING_ENABLED=true],
  [LOGGING_ENABLED=false])

AC_SUBST(LOGGING_ENABLED)

#################### end Logging configuration ####################

#################### Diderot Debugger configuration ####################

DEBUGGER_ENABLED=false
AC_ARG_ENABLE([debugger],
  AC_HELP_STRING([--enable-debugger], [build the Diderot debugger (experimental)]),
  [DEBUGGER_ENABLED=true],
  [DEBUGGER_ENABLED=false])

AC_SUBST(DEBUGGER_ENABLED)

#################### end Diderot Debugger configuration ####################

#################### CUDA configuration ####################

AC_ARG_ENABLE([cuda],
  AC_HELP_STRING([--enable-cuda], [include support for targeting CUDA]))

if test "${enable_cuda+set}" = set ; then
dnl
dnl Look for NVIDIA's nvcc compiler
dnl
  AC_MSG_NOTICE([checking for CUDA and nvcc])
  AC_PATH_PROG(NVCC, nvcc, no, $PATH)
  if test x"$NVCC" = xno ; then
    AC_MSG_WARN([nvcc not found])
    CUDA_ENABLED=false
    NVCC=":"
  else
    AC_MSG_RESULT([$NVCC])
    CUDA_ENABLED=true
  fi
else
  NVCC=":"
  CUDA_ENABLED=false
fi
AC_SUBST(NVCC)
AC_SUBST(CUDA_ENABLED)

#################### end CUDA configuration ####################

#################### OpenCL configuration ####################

AC_ARG_ENABLE([opencl],
  AC_HELP_STRING([--enable-opencl], [include support for targeting OpenCL]))

if test "${enable_opencl+set}" = set ; then

dnl
dnl check for OpenCL support
dnl
  LIBS_CL="none"
  case "$host_os" in
    darwin*) # On Mac OS X we check for installed frameworks
      AX_CHECK_FRAMEWORK([OpenCL], [LIBS_CL="-framework OpenCL"], [])
    ;;
    *)
      AC_CHECK_LIB([OpenCL],[clGetPlatformIDs],[LIBS_CL="-lOpenCL"])
    ;;
  esac
  if test x"$LIBS_CL" = xnone ; then
    AC_MSG_WARN([no OpenCL library found])
    CL_ENABLED=false
    CL_VERSION=0
  else
    CL_ENABLED=true
    CPPFLAGS_CL=""
    CL_HEADER=""
    AC_CHECK_HEADERS([CL/cl.h OpenCL/cl.h], [HAVE_CL_H="yes"; CL_HEADER="$ac_header"; break], [HAVE_CL_H="no"])
    if test x"$HAVE_CL_H" = xno ; then
      # check for AMD's SDK
      AC_MSG_CHECKING([for cl.h in AMD's SDK])
      if test -d /opt/AMDAPP/include/CL ; then
        HAVE_CL_H="yes"
        CL_HEADER="/opt/AMDAPP/include/CL/cl.h"
        AC_DEFINE([HAVE_CL_CL_H])
        CPPFLAGS_CL="-I/opt/AMDAPP/include"
      fi
      AC_MSG_RESULT([$HAVE_CL_H])
    fi
    if test x"$HAVE_CL_H" = xno ; then
      # check for NVIDIA's SDK
      AC_MSG_CHECKING([for NVIDIA's SDK cl.h])
      CUDA_HOME=none
      for CUDA in cuda cuda-7.0 cuda-6.5 cuda-6.0 ; do
        if test -d /usr/local/$CUDA/include/CL ; then
          CUDA_HOME=/usr/local/$CUDA
          break
        fi
      done
      if test x"$CUDA_HOME" != xnone ; then
        HAVE_CL_H="yes"
        CL_HEADER="$CUDA_HOME/include/CL/cl.h"
        AC_DEFINE([HAVE_CL_CL_H])
        CPPFLAGS_CL="-I$CUDA_HOME/include"
      fi
      AC_MSG_RESULT([$HAVE_CL_H])
    fi
    if test x"$HAVE_CL_H" = xno ; then
      AC_MSG_WARN([no cl.h found])
      CL_ENABLED=false
      CL_VERSION=0
      SIZEOF_CL_INT=0
      SIZEOF_CL_LONG=0
      SIZEOF_CL_FLOAT=0
      SIZEOF_CL_DOUBLE=0
    else
      # check the cl.h header for which version of OpenCL is supported
      AC_LANG([C])
      save_CPPFLAGS=$CPPFLAGS
      CPPFLAGS="$CPPFLAGS_CL $CPPFLAGS"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
# ifdef HAVE_CL_CL_H
#   include <CL/cl.h>
# elif defined(HAVE_OPENCL_CL_H)
#   include <OpenCL/cl.h>
# else
#   error no cl.h
# endif]],
[[#if defined(CL_VERSION_1_2)
  printf("120\n");
#elif defined(CL_VERSION_1_1)
  printf("110\n");
#elif defined(CL_VERSION_1_0)
  printf("100\n");
#else
  printf("0\n");
#endif
        ]])],
        [CL_VERSION=`./conftest$EXEEXT`], [CL_VERSION=0])
dnl
dnl check to see how we access the host-side vector types.  For OpenCL 1.1,
dnl the specification specifies the behavior, but it was unspecified in 1.0.
dnl
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
  #include "$CL_HEADER"
  ]],
        [[cl_float4 x; x[0] = 0.0;]])],
        [AC_DEFINE([CL_HOST_VECTORS_ARE_ARRAYS],[1],[host vector types are arrays])],
  dnl
  dnl not arrays, so try the OpenCL 1.1 structure representation
  dnl
        [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "$CL_HEADER"
]],
        [[cl_float4 x; x.s[0] = 0.0;]])],
        [AC_DEFINE([CL_HOST_VECTORS_ARE_STRUCTS],[1],[host vector types are structs])],
        [AC_MSG_ERROR([unable to figure out host types for OpenCL vectors])])])
    # restore saved CPPFLAGS
      CPPFLAGS=$save_CPPFLAGS
    # substitutions
      AC_SUBST(LIBS_CL)
      AC_SUBST(CPPFLAGS_CL)
    #
    # determine the sizes of the host-side OpenCL types
    AC_CHECK_SIZEOF([cl_int], [], [#include "$CL_HEADER"])
    SIZEOF_CL_INT=$ac_cv_sizeof_cl_int
    AC_CHECK_SIZEOF([cl_long], [], [#include "$CL_HEADER"])
    SIZEOF_CL_LONG=$ac_cv_sizeof_cl_long
    AC_CHECK_SIZEOF([cl_float], [], [#include "$CL_HEADER"])
    SIZEOF_CL_FLOAT=$ac_cv_sizeof_cl_float
    AC_CHECK_SIZEOF([cl_double], [], [#include "$CL_HEADER"])
    SIZEOF_CL_DOUBLE=$ac_cv_sizeof_cl_double
    fi
  fi
else
  CL_ENABLED=false
  CL_VERSION="0"
  SIZEOF_CL_INT="0"
  SIZEOF_CL_LONG="0"
  SIZEOF_CL_FLOAT="0"
  SIZEOF_CL_DOUBLE="0"
fi

AC_SUBST(CL_ENABLED)
AC_SUBST(CL_VERSION)
AC_DEFINE_UNQUOTED(DIDEROT_CL_VERSION,[$CL_VERSION],[version of OpenCL supported by system])

AC_SUBST(SIZEOF_CL_INT)
AC_SUBST(SIZEOF_CL_LONG)
AC_SUBST(SIZEOF_CL_FLOAT)
AC_SUBST(SIZEOF_CL_DOUBLE)

#################### end OpenCL configuration ####################

#################### Teem configuration ####################

dnl get the path to the TEEM installation.  We need this to build the Diderot
dnl runtime code and we use the path when searching for teem in the Diderot
dnl compiler.
dnl

dnl the path can be specified using "--with-teem=..." too
dnl
AC_ARG_WITH(teem,
  AC_HELP_STRING([--with-teem=<path>], [specify path to teem installation]),
  [ac_cv_use_teem=$withval], [ac_cv_use_teem=no])

if test x"$ac_cv_use_teem" = xno ; then
dnl search for Teem in some standard places
dnl
  AC_MSG_CHECKING([for teem])
  TEEM_DIR=none
  POSSIBLE_TEEM_DIRS="/usr/local/teem /usr/local"
  for dir in $POSSIBLE_TEEM_DIRS ; do
    if test -f $dir/include/teem/nrrd.h ; then
      TEEM_DIR=$dir
      AC_MSG_RESULT([$TEEM_DIR])
      break
    fi
  done
  if test $TEEM_DIR = none ; then
    AC_MSG_ERROR([unable to find teem; use --with-teem option to specify location])
  fi
else
  AC_MSG_CHECKING([path to teem ($ac_cv_use_teem)])
  case $ac_cv_use_teem in
    /*) ;;
    *) AC_MSG_ERROR([path to teem must be absolute]) ;;
  esac
  if test -x $ac_cv_use_teem ; then
    TEEM_DIR=$ac_cv_use_teem
    # check that TEEM_DIR actually points to something that looks like teem
    if test -f $TEEM_DIR/include/teem/nrrd.h ; then
      AC_MSG_RESULT([yes])
    else
      TEEM_DIR=none
      AC_MSG_RESULT([invalid path to teem])
    fi
  else
    TEEM_DIR=none
    AC_MSG_RESULT([invalid path to teem])
  fi
fi
if test x"$TEEM_DIR" = xnone ; then
  AC_MSG_ERROR([unable to find teem])
fi
dnl
dnl check that teem supports the nrrdMetaDataNormalize function
dnl
save_LDFLAGS=$LDFLAGS
save_LIBS=$LIBS
LDFLAGS="$LDFLAGS -L$TEEM_DIR/lib"
LIBS="$LIBS -lteem -lm"
AC_CHECK_FUNCS([nrrdMetaDataNormalize])
LDFLAGS=$save_LDFLAGS
LIBS=$save_LIBS

if test "x$ac_cv_func_nrrdMetaDataNormalize" != xyes; then
  AC_MSG_ERROR(["please update your teem installation"])
fi

AC_SUBST(TEEM_DIR)

#################### end Teem configuration ####################

#################### SML configuration ####################

dnl
dnl We support building with both SML/NJ (the default) or MLton.
dnl

dnl first check for --with-mlton=path flag
dnl
AC_ARG_WITH([mlton],
  [AS_HELP_STRING([--with-mlton=<path-to-mlton>],
    [use the MLton Standard ML compiler to build Diderot])],
  [with_mlton=yes],[with_mlton=no])
if test x"$with_mlton" = xyes ; then
  if test x"$ac_cv_path_with_mlton" = x ; then
    # look for mlton in the PATH
    AC_PATH_PROGS(with_mlton, mlton, no)
    if test x"$MLTON" = xno ; then
      AC_MSG_ERROR([unable to find mlton executable; please specify the path])
    fi
  else
    with_mlton=$ac_cv_path_with_mlton
    case $with_mlton in
      /*) ;;
      *) AC_MSG_ERROR([please specify absolute path for mlton executable]) ;;
    esac
    if test ! -x $with_mlton ; then
      AC_MSG_ERROR([invalid path for mlton; $with_mlton is not executable])
    fi
  fi
fi

if test x"$with_mlton" = xno ; then
  # we are using SML/NJ
  #

  dnl check that we have a compatible version of SML/NJ
  dnl
  CHECK_SMLNJ([AC_MSG_ERROR([unable to find SML/NJ installation; please add it to your PATH or set SML_CMD])])
  if test $SMLNJ_MAJOR_VERSION -lt 110 \
    -o $SMLNJ_MINOR_VERSION -lt 80
  then
    AC_MSG_ERROR([installation requires SML/NJ version 110.80+ (available from smlnj.org)])
  fi

  dnl if the SMLNJ_PATH (and SMLNJ_CMD) variables are relative paths, then we add the
  dnl $DIDEROT_ROOT as a prefix so that scripts work correctly.
  dnl
  case $SMLNJ_PATH in
    /*) ;; # already absolute
    *)  SMLNJ_PATH="$DIDEROT_ROOT/$SMLNJ_PATH"
        sml=`basename "$SMLNJ_CMD"`
        SMLNJ_CMD="$SMLNJ_PATH"/"$sml"
        AC_MSG_NOTICE([using SMLNJ_PATH=$SMLNJ_PATH])
  esac

  dnl determine the heap-image suffix
  dnl
  CHECK_SMLNJ_HEAP_SUFFIX([
    AC_MSG_ERROR([unsupported configuration ${host_cpu}-${host_os}])])

  dnl look for ml-makedepend
  dnl
  tmpPATH="$SMLNJ_PATH:$PATH"
  AC_PATH_PROG(ML_MAKEDEPEND, ml-makedepend, ":", $tmpPATH)
  AC_SUBST(ML_MAKEDEPEND)

  dnl
  dnl look for ml-build
  dnl
  tmpPATH="$SMLNJ_PATH:$PATH"
  AC_PATH_PROG(ML_BUILD, ml-build, none, $tmpPATH)
  if test $ML_BUILD = none ; then
    AC_MSG_ERROR([ml-build not found])
  fi
  AC_SUBST(ML_BUILD)
  SML=smlnj

  HEAP_IMAGE_DIR=$DIDEROT_BINDIR/.heap
  INSTALL_SMLNJ_WRAPPER=$DIDEROT_BINDIR/install-sml-wrapper.sh
  AC_SUBST(HEAP_IMAGE_DIR)
  AC_SUBST(INSTALL_SMLNJ_WRAPPER)

else

  # we are using MLton
  MLTON=$with_mlton
  AC_SUBST(MLTON)
  SML=mlton

fi
AC_SUBST(SML)

#################### end SML configuration ####################

#################### Documentation tools configuration ####################

dnl check for doxygen
dnl
AC_ARG_WITH([doxygen],
  [AS_HELP_STRING([--with-doxygen=<path>], [specify location of doxygen executable])],
  [DOXYGEN=$with_doxygen],
  [DOXYGEN=none])
if test x$DOXYGEN = xnone ; then
  # see if we can find doxygen in the user's path
  AC_PATH_PROG(DOXYGEN, [doxygen], [none])
fi
if test x$with_doxygen = xyes -o x$with_doxygen = xno ; then
  AC_MSG_ERROR([--with-doxygen option must specify directory argument])
elif test x$DOXYGEN != xnone -a ! -x $DOXYGEN ; then
  AC_MSG_ERROR([doxygen not found at $DOXYGEN])
elif test x$DOXYGEN != xnone ; then
dnl
dnl verify that $DOXYGEN is an absolute path
dnl
  case $DOXYGEN in
    /*) ;;
    *) AC_MSG_ERROR([--with-doxygen path must be absolute]) ;;
  esac
fi
if test $DOXYGEN = xnone ; then
  DOXYGEN=:
fi
AC_SUBST(DOXYGEN)

dnl check for asciidoctor
dnl
AC_PATH_PROGS([ASCIIDOCTOR], [asciidoctor], [none])
AC_SUBST(ASCIIDOCTOR)

#################### end Documentation tools configuration ####################

#################### Diderot Debugger configuration ####################

dnl The debugger is under development
dnl

dnl if the debugger is enabled, then include its makefile and
dnl look for wxwidgets
dnl
if test x"$DEBUGGER_ENABLED" = xtrue ; then
  DEBUGGER_MAKEFILE=src/debugger/build/Makefile
  search_path="$PATH$PATH_SEPARATOR/usr/local/bin$PATH_SEPARATOR/opt/local/bin"
  AC_PATH_PROG([WX_CONFIG], [wx-config], [none], [$search_path])
  if test x"$WX_CONFIG" = xnone ; then
    AC_MSG_ERROR([unable to find wx-config, which is required by the Diderot debugger])
  fi
else
  DEBUGGER_MAKEFILE=
fi

#################### end Diderot Debugger configuration ####################

#################### config.h ####################

AC_DEFINE_UNQUOTED(DIDEROT_INCLUDE_PATH,["$DIDEROT_SRCDIR/lib/include"], [path for Diderot header files])

AH_TOP([
/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2019 The University of Chicago * All rights reserved.
 */

#ifndef _DIDEROT_CONFIG_H_
#define _DIDEROT_CONFIG_H_

])
AH_BOTTOM([

#endif /* !_DIDEROT_CONFIG_H_ */
])

#################### end config.h ####################

#################### write output ####################

dnl The Makefile for the compiler depends on which SML system we are
dnl using.
if test x"$SML" = xmlton ; then
  COMPILER_MAKEFILE_IN=":src/compiler/Makefile_mlton.in"
  FRAG_GEN_MAKEFILE="src/compiler/gen/fragments/Makefile"
else
  COMPILER_MAKEFILE_IN=""
  FRAG_GEN_MAKEFILE=
fi

AC_CONFIG_FILES(
dnl
dnl ***** Makefiles *****
  Makefile
dnl  doc/Makefile
  doc/src/man/Makefile
dnl  src/clinfo/Makefile
  src/compiler/Makefile"$COMPILER_MAKEFILE_IN"
  $FRAG_GEN_MAKEFILE
  $DEBUGGER_MAKEFILE
  src/lib/build/Makefile
  src/lib/build/mk/build.gmk:src/lib/build/mk/build_gmk.in
dnl
dnl ***** SML source files *****
  src/compiler/nrrd/run-dnorm.sml:src/compiler/nrrd/run-dnorm_sml.in
  src/compiler/common/paths.sml:src/compiler/common/paths_sml.in
  src/compiler/common/size-of.sml:src/compiler/common/size-of_sml.in
  src/compiler/options/version.sml:src/compiler/options/version_sml.in
)

AC_CONFIG_HEADERS(src/lib/include/diderot/config.h:config/config_h.in)

dnl
dnl shell scripts
dnl
AC_CONFIG_FILES([
  bin/install-sml-wrapper.sh:config/install-sml-wrapper_sh.in
], [chmod +x bin/install-sml-wrapper.sh])
AC_CONFIG_FILES([
  src/compiler/gen/fragments/mkfrags.sh:src/compiler/gen/fragments/mkfrags_sh.in
], [chmod +x src/compiler/gen/fragments/mkfrags.sh])
AC_CONFIG_FILES([
  src/compiler/gen/fragments/mkmk.sh:src/compiler/gen/fragments/mkmk_sh.in
], [chmod +x src/compiler/gen/fragments/mkmk.sh])

dnl
dnl additional files for logging tools
dnl
if test x"$LOGGING_ENABLED" = xtrue ; then
  AC_CONFIG_FILES([
    src/logging/gen/Makefile
    src/logging/log-analyze/Makefile
    src/logging/log-dump/Makefile
    src/logging/gen/log-gen.sh:src/logging/gen/log-gen_sh.in
    src/logging/gen/main.sml:src/logging/gen/main_sml.in
    src/logging/lib/default-log-paths.hxx:src/logging/lib/default-log-paths_hxx.in
  ], [chmod +x src/logging/gen/log-gen.sh])
fi

dnl
dnl generate makefiles for runtime library builds
dnl
dnl usage: MK_BUILD_DIR(<build-dir>, <target-platform>, <options>)
dnl
dnl where the options are taken from [debug, parallel]
dnl
AC_DEFUN([MK_BUILD_DIR], [
    if test ! -d src/lib/build/$1 ; then
      mkdir src/lib/build/$1
    fi
    [sed -e 's,@BUILD_TARGET@,$1,g' \
      -e 's,@TARGET_PLATFORM@,$2,g' \
      -e 's,@BUILD_OPTIONS@,$3,g' \
      src/lib/build/mk/Makefile.in \
        > src/lib/build/$1/Makefile.in]
    AC_CONFIG_FILES(src/lib/build/$1/Makefile)
  ])

MK_BUILD_DIR(rt-seq, sequential, [])
MK_BUILD_DIR(rt-seq-debug, sequentiagetcpul, [debug])
MK_BUILD_DIR(rt-par, parallel, [parallel])
MK_BUILD_DIR(rt-par-debug, parallel, [parallel debug])

if test x"$LOGGING_ENABLED" = xtrue ; then
  MK_BUILD_DIR(rt-par-log, parallel, [parallel logging])
fi

#if test x"$CL_ENABLED" = xtrue ; then
#  MK_BUILD_DIR(rt-cl-f, opencl, [float])
#  MK_BUILD_DIR(rt-cl-d, opencl, [double])
#  MK_BUILD_DIR(rt-cl-f-debug, opencl, [float debug])
#  MK_BUILD_DIR(rt-cl-d-debug, opencl, [double debug])
#fi

AC_OUTPUT

#################### end write output ####################
