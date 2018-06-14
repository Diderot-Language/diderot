(* fragments.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!
 *)

structure CxxFragments =
  struct

    val cxxHead = "\
          \/*---------- begin cxx-head.in ----------*/\n\
          \/*! \\file @CXXFILE@\n\
          \ *\n\
          \ * Generated from @SRCFILE@.\n\
          \ *\n\
          \ * Command: @DIDEROTC_CMD@ @DIDEROTC_ARGV@\n\
          \ * Version: @DIDEROTC_VERSION@\n\
          \ */\n\
          \/*---------- end cxx-head.in ----------*/\n\
          \"

    val debugIncl = "\
          \/*---------- begin debugger-incl.in ----------*/\n\
          \#define DIDEROT_STANDALONE_EXEC\n\
          \#define @DIDEROT_FLOAT_PRECISION@\n\
          \#define @DIDEROT_INT_PRECISION@\n\
          \#define @DIDEROT_TARGET@\n\
          \#include \"diderot/diderot.hxx\"\n\
          \\n\
          \#ifdef DIDEROT_ENABLE_LOGGING\n\
          \#define IF_LOGGING(...)         __VA_ARGS__\n\
          \#else\n\
          \#define IF_LOGGING(...)\n\
          \#endif\n\
          \\n\
          \static std::string ProgramName = \"@PROG_NAME@\";\n\
          \\n\
          \extern \"C\" {\n\
          \typedef struct @PREFIX@_struct_world @PREFIX@_world_t;\n\
          \}\n\
          \/*---------- end debugger-incl.in ----------*/\n\
          \"

    val eigenvals2x2 = "\
          \/*---------- begin eigenvals2x2.in ----------*/\n\
          \static int eigenvals (tensor_ref_2_2 mat, diderot::array<@REALTY@,2> &eval)\n\
          \{\n\
          \    int roots;\n\
          \\n\
          \  /* copy the given matrix elements */\n\
          \    @REALTY@ M00 = mat[0];\n\
          \    @REALTY@ M01 = mat[1];\n\
          \    @REALTY@ M11 = mat[3];\n\
          \\n\
          \  /* subtract out the eigenvalue mean (we will add it back to evals later);\n\
          \   * helps with numerical stability\n\
          \   */\n\
          \    @REALTY@ mean = @REALTY@(0.5) * (M00 + M11);\n\
          \    M00 -= mean;\n\
          \    M11 -= mean;\n\
          \\n\
          \    @REALTY@ Q = M00 - M11;\n\
          \    @REALTY@ D = @REALTY@(4)*M01*M01 + Q*Q;\n\
          \    if (D > diderot::__details::EPSILON) {\n\
          \      /* two distinct roots */\n\
          \        @REALTY@ vv = @REALTY@(0.5) * std::sqrt(D);\n\
          \        eval[0] = vv;\n\
          \        eval[1] = -vv;\n\
          \        roots = diderot::__details::ROOT_TWO;\n\
          \    }\n\
          \    else {\n\
          \      /* double root */\n\
          \        eval[0] = eval[1] = @REALTY@(0);\n\
          \        roots = diderot::__details::ROOT_DOUBLE;\n\
          \    }\n\
          \\n\
          \  /* add back in the eigenvalue mean */\n\
          \    eval[0] += mean;\n\
          \    eval[1] += mean;\n\
          \\n\
          \    return roots;\n\
          \}\n\
          \/*---------- end eigenvals2x2.in ----------*/\n\
          \"

    val eigenvecs2x2 = "\
          \/*---------- begin eigenvecs2x2.in ----------*/\n\
          \static int eigenvecs (tensor_ref_2_2 mat, diderot::array<@REALTY@,2> &eval, diderot::array<tensor_2,2> &evec)\n\
          \{\n\
          \    int roots;\n\
          \\n\
          \  /* copy the given matrix elements */\n\
          \    @REALTY@ M00 = mat[0];\n\
          \    @REALTY@ M01 = mat[1];\n\
          \    @REALTY@ M11 = mat[3];\n\
          \\n\
          \  /* subtract out the eigenvalue mean (we will add it back to evals later);\n\
          \   * helps with numerical stability\n\
          \   */\n\
          \    @REALTY@ mean = @REALTY@(0.5) * (M00 + M11);\n\
          \    M00 -= mean;\n\
          \    M11 -= mean;\n\
          \\n\
          \    @REALTY@ Q = M00 - M11;\n\
          \    @REALTY@ D = @REALTY@(4)*M01*M01 + Q*Q;\n\
          \    if (D > diderot::__details::EPSILON) {\n\
          \      /* two distinct roots */\n\
          \        @REALTY@ vv = @REALTY@(0.5) * std::sqrt(D);\n\
          \        eval[0] = vv;\n\
          \        eval[1] = -vv;\n\
          \      /* null space of T = M - evec[0]*I ==\n\
          \         [M00 - vv      M01  ]\n\
          \         [  M01      M11 - vv]\n\
          \         is evec[0], but we know evec[0] and evec[1] are orthogonal,\n\
          \         so row span of T is evec[1]\n\
          \      */\n\
          \        @REALTY@ r1[2] = { M00 - vv, M01 };\n\
          \        @REALTY@ r2[2] = { M01, M11 - vv };\n\
          \        if ((r1[0]*r2[0] + r1[1]*r2[1]) > @REALTY@(0)) {\n\
          \            evec[1][0] = r1[0] + r2[0];\n\
          \            evec[1][1] = r1[1] + r2[1];\n\
          \        }\n\
          \        else {\n\
          \            evec[1][0] = r1[0] - r2[0];\n\
          \            evec[1][1] = r1[1] - r2[1];\n\
          \        }\n\
          \        diderot::__details::normalize2 (evec[1]._data);\n\
          \        evec[0][0] = evec[1][1];\n\
          \        evec[0][1] = -evec[1][0];\n\
          \        diderot::__details::normalize2 (evec[0]._data);\n\
          \        roots = diderot::__details::ROOT_TWO;\n\
          \    }\n\
          \    else {\n\
          \      /* double root */\n\
          \        eval[0] = eval[1] = @REALTY@(0.0);\n\
          \      /* use any basis for eigenvectors */\n\
          \        evec[0][0] = @REALTY@(1.0);\n\
          \        evec[0][1] = @REALTY@(0.0);\n\
          \        evec[1][0] = @REALTY@(0.0);\n\
          \        evec[1][1] = @REALTY@(1.0);\n\
          \        roots = diderot::__details::ROOT_DOUBLE;\n\
          \    }\n\
          \\n\
          \    /* add back in the eigenvalue mean */\n\
          \    eval[0] += mean;\n\
          \    eval[1] += mean;\n\
          \\n\
          \    return roots;\n\
          \}\n\
          \/*---------- end eigenvecs2x2.in ----------*/\n\
          \"

    val eigenvals3x3 = "\
          \/*---------- begin eigenvals3x3.in ----------*/\n\
          \// from http://en.wikipedia.org/wiki/Square_root_of_3\n\
          \#define M_SQRT3 @REALTY@(1.732050807568877293527446341506)\n\
          \\n\
          \static int eigenvals (tensor_ref_3_3 const &mat, diderot::array<@REALTY@,3> &eval)\n\
          \{\n\
          \    int roots;\n\
          \\n\
          \  /* copy the given matrix elements */\n\
          \    @REALTY@ M00 = mat[0];\n\
          \    @REALTY@ M01 = mat[1];\n\
          \    @REALTY@ M02 = mat[2];\n\
          \    @REALTY@ M11 = mat[4];\n\
          \    @REALTY@ M12 = mat[5];\n\
          \    @REALTY@ M22 = mat[8];\n\
          \\n\
          \  /* subtract out the eigenvalue mean (we will add it back to evals later);\n\
          \   * helps with numerical stability\n\
          \   */\n\
          \    @REALTY@ mean = (M00 + M11 + M22) / @REALTY@(3);\n\
          \    M00 -= mean;\n\
          \    M11 -= mean;\n\
          \    M22 -= mean;\n\
          \\n\
          \  /*\n\
          \  ** divide out L2 norm of eigenvalues (will multiply back later);\n\
          \  ** this too seems to help with stability\n\
          \  */\n\
          \    @REALTY@ norm = std::sqrt(M00*M00 + 2*M01*M01 + 2*M02*M02 + M11*M11 + 2*M12*M12 + M22*M22);\n\
          \    @REALTY@ rnorm = (norm > diderot::__details::EPSILON) ? @REALTY@(1) / norm : @REALTY@(1);\n\
          \    M00 *= rnorm;\n\
          \    M01 *= rnorm;\n\
          \    M02 *= rnorm;\n\
          \    M11 *= rnorm;\n\
          \    M12 *= rnorm;\n\
          \    M22 *= rnorm;\n\
          \\n\
          \  /* this code is a mix of prior Teem code and ideas from Eberly's\n\
          \   * \"Eigensystems for 3 x 3 Symmetric Matrices (Revisited)\"\n\
          \   */\n\
          \    @REALTY@ Q = (M01*M01 + M02*M02 + M12*M12 - M00*M11 - M00*M22 - M11*M22) / @REALTY@(3);\n\
          \    @REALTY@ QQQ = Q*Q*Q;\n\
          \    @REALTY@ R = @REALTY@(0.5) * (M00*M11*M22 + M02*(2*M01*M12 - M02*M11) - M00*M12*M12 - M01*M01*M22);\n\
          \    @REALTY@ D = QQQ - R*R;\n\
          \    if (D > diderot::__details::EPSILON) {\n\
          \      /* three distinct roots- this is the most common case */\n\
          \        @REALTY@ theta = std::atan2(std::sqrt(D), R) / @REALTY@(3);\n\
          \        @REALTY@ mm = std::sqrt(Q);\n\
          \        @REALTY@ ss = std::sin(theta);\n\
          \        @REALTY@ cc = std::cos(theta);\n\
          \        eval[0] = 2*mm*cc;\n\
          \        eval[1] = mm*(-cc + M_SQRT3 * ss);\n\
          \        eval[2] = mm*(-cc - M_SQRT3 * ss);\n\
          \        roots = diderot::__details::ROOT_THREE;\n\
          \    }\n\
          \  /* else D is near enough to zero */\n\
          \    else if (std::abs(R) > diderot::__details::EPSILON) {\n\
          \      /* one double root and one single root */\n\
          \        @REALTY@ U = std::cbrt(R); /* cube root function */\n\
          \        if (U > 0) {\n\
          \            eval[0] = 2*U;\n\
          \            eval[1] = -U;\n\
          \            eval[2] = -U;\n\
          \        }\n\
          \        else {\n\
          \            eval[0] = -U;\n\
          \            eval[1] = -U;\n\
          \            eval[2] = 2*U;\n\
          \        }\n\
          \        roots = diderot::__details::ROOT_SINGLE_DOUBLE;\n\
          \    }\n\
          \    else {\n\
          \      /* a triple root! */\n\
          \        eval[0] = eval[1] = eval[2] = 0.0;\n\
          \        roots = diderot::__details::ROOT_TRIPLE;\n\
          \    }\n\
          \\n\
          \  /* multiply back by eigenvalue L2 norm */\n\
          \    eval[0] /= rnorm;\n\
          \    eval[1] /= rnorm;\n\
          \    eval[2] /= rnorm;\n\
          \\n\
          \  /* add back in the eigenvalue mean */\n\
          \    eval[0] += mean;\n\
          \    eval[1] += mean;\n\
          \    eval[2] += mean;\n\
          \\n\
          \    return roots;\n\
          \}\n\
          \\n\
          \#undef M_SQRT3\n\
          \/*---------- end eigenvals3x3.in ----------*/\n\
          \"

    val eigenvecs3x3 = "\
          \/*---------- begin eigenvecs3x3.in ----------*/\n\
          \inline @REALTY@ dot3 (tensor_ref_3 const & a, tensor_ref_3 const & b)\n\
          \{\n\
          \    return (a[0]*b[0] + a[1]*b[1] + a[2]*b[2]);\n\
          \}\n\
          \\n\
          \inline void cross3 (tensor_ref_3 const & v1, tensor_ref_3 const & v2, tensor_3 & res)\n\
          \{\n\
          \    res[0] = v1[1]*v2[2] - v1[2]*v2[1];\n\
          \    res[1] = v1[2]*v2[0] - v1[0]*v2[2];\n\
          \    res[2] = v1[0]*v2[1] - v1[1]*v2[0];\n\
          \}\n\
          \\n\
          \inline void nullspace1 (\n\
          \    tensor_ref_3 const & r0,\n\
          \    tensor_ref_3 const & r1,\n\
          \    tensor_ref_3 const & r2,\n\
          \    tensor_3 & res)\n\
          \{\n\
          \    tensor_3 crs;\n\
          \\n\
          \    cross3(r0, r1, res);\n\
          \    cross3(r1, r2, crs);\n\
          \\n\
          \  /* ret += crs or ret -= crs; whichever makes res longer */\n\
          \    if (dot3(res, crs) > 0.0) {\n\
          \\tres[0] += crs[0];\n\
          \\tres[1] += crs[1];\n\
          \\tres[2] += crs[2];\n\
          \    } else {\n\
          \\tres[0] -= crs[0];\n\
          \\tres[1] -= crs[1];\n\
          \\tres[2] -= crs[2];\n\
          \    }\n\
          \\n\
          \    cross3(r0, r2, crs);\n\
          \  /* ret += crs or ret -= crs; whichever makes res longer */\n\
          \    if (dot3(res, crs) > 0.0) {\n\
          \\tres[0] += crs[0];\n\
          \\tres[1] += crs[1];\n\
          \\tres[2] += crs[2];\n\
          \    } else {\n\
          \\tres[0] -= crs[0];\n\
          \\tres[1] -= crs[1];\n\
          \\tres[2] -= crs[2];\n\
          \    }\n\
          \}\n\
          \\n\
          \/*\n\
          \** All vectors are in the same 1D space, we have to find two\n\
          \** mutually vectors perpendicular to that span\n\
          \*/\n\
          \static void nullspace2 (\n\
          \    tensor_ref_3 const & r0,\n\
          \    tensor_ref_3 const & r1,\n\
          \    tensor_ref_3 const & r2,\n\
          \    tensor_3 *rets)  // will point to either evec[0] or evec[1]\n\
          \{\n\
          \    tensor_3 sqr, sum;\n\
          \    int idx;\n\
          \\n\
          \    sum = r0;\n\
          \    if (dot3(sum, r1) > 0) {\n\
          \\tsum[0] += r1[0];\n\
          \\tsum[1] += r1[1];\n\
          \\tsum[2] += r1[2];\n\
          \    } else {\n\
          \\tsum[0] -= r1[0];\n\
          \\tsum[1] -= r1[1];\n\
          \\tsum[2] -= r1[2];\n\
          \    }\n\
          \    if (dot3(sum, r2) > 0) {\n\
          \\tsum[0] += r2[0];\n\
          \\tsum[1] += r2[1];\n\
          \\tsum[2] += r2[2];\n\
          \    } else {\n\
          \\tsum[0] -= r2[0];\n\
          \\tsum[1] -= r2[1];\n\
          \\tsum[2] -= r2[2];\n\
          \    }\n\
          \  // find largest component, to get most stable expression for a perpendicular vector\n\
          \    sqr[0] = sum[0]*sum[0];\n\
          \    sqr[1] = sum[1]*sum[1];\n\
          \    sqr[2] = sum[2]*sum[2];\n\
          \    idx = 0;\n\
          \    if (sqr[0] < sqr[1]) {\n\
          \\tidx = 1;\n\
          \    }\n\
          \    if (sqr[idx] < sqr[2]) {\n\
          \\tidx = 2;\n\
          \    }\n\
          \\n\
          \    if (0 == idx) {\n\
          \      rets[0] = {sum[1] - sum[2], -sum[0], sum[0]};\n\
          \    } else if (1 == idx) {\n\
          \      rets[0] = {-sum[1], sum[0] - sum[2], sum[1]};\n\
          \    } else {\n\
          \      rets[0] = {-sum[2], sum[2], sum[0] - sum[1]};\n\
          \    }\n\
          \\n\
          \    cross3(rets[0], sum, rets[1]);\n\
          \    return;\n\
          \}\n\
          \\n\
          \static int eigenvecs (tensor_ref_3_3 const &mat, diderot::array<@REALTY@, 3> &eval, diderot::array<tensor_3,3> &evec)\n\
          \{\n\
          \    @REALTY@ len, dot;\n\
          \    int roots;\n\
          \\n\
          \  /* copy the given matrix elements */\n\
          \    @REALTY@ M00 = mat[0];\n\
          \    @REALTY@ M01 = mat[1];\n\
          \    @REALTY@ M02 = mat[2];\n\
          \    @REALTY@ M11 = mat[4];\n\
          \    @REALTY@ M12 = mat[5];\n\
          \    @REALTY@ M22 = mat[8];\n\
          \\n\
          \  /*\n\
          \  ** subtract out the eigenvalue mean (will add back to evals later);\n\
          \  ** helps with numerical stability\n\
          \  */\n\
          \    @REALTY@ mean = (M00 + M11 + M22) / @REALTY@(3);\n\
          \    M00 -= mean;\n\
          \    M11 -= mean;\n\
          \    M22 -= mean;\n\
          \\n\
          \  /*\n\
          \  ** divide out L2 norm of eigenvalues (will multiply back later);\n\
          \  ** this too seems to help with stability\n\
          \  */\n\
          \    @REALTY@ norm = std::sqrt(M00*M00 + 2*M01*M01 + 2*M02*M02 + M11*M11 + 2*M12*M12 + M22*M22);\n\
          \    @REALTY@ rnorm = (norm > diderot::__details::EPSILON) ? @REALTY@(1) / norm : @REALTY@(1);\n\
          \    M00 *= rnorm;\n\
          \    M01 *= rnorm;\n\
          \    M02 *= rnorm;\n\
          \    M11 *= rnorm;\n\
          \    M12 *= rnorm;\n\
          \    M22 *= rnorm;\n\
          \\n\
          \  /* this code is a mix of prior Teem code and ideas from Eberly's\n\
          \   * \"Eigensystems for 3 x 3 Symmetric Matrices (Revisited)\"\n\
          \   */\n\
          \    @REALTY@ Q = (M01*M01 + M02*M02 + M12*M12 - M00*M11 - M00*M22 - M11*M22)/@REALTY@(3);\n\
          \    @REALTY@ QQQ = Q*Q*Q;\n\
          \    @REALTY@ R = @REALTY@(0.5)*(M00*M11*M22 + M02*(2*M01*M12 - M02*M11) - M00*M12*M12 - M01*M01*M22);\n\
          \    @REALTY@ D = QQQ - R*R;\n\
          \    if (D > diderot::__details::EPSILON) {\n\
          \      /* three distinct roots- this is the most common case */\n\
          \        @REALTY@ theta = std::atan2(std::sqrt(D), R) / @REALTY@(3);\n\
          \        @REALTY@ mm = std::sqrt(Q);\n\
          \        @REALTY@ ss = std::sin(theta);\n\
          \        @REALTY@ cc = std::cos(theta);\n\
          \        eval[0] = 2*mm*cc;\n\
          \        eval[1] = mm*(-cc + std::sqrt(3.0)*ss);\n\
          \        eval[2] = mm*(-cc - std::sqrt(3.0)*ss);\n\
          \        roots = diderot::__details::ROOT_THREE;\n\
          \    }\n\
          \  /* else D is near enough to zero */\n\
          \    else if (std::abs(R) > diderot::__details::EPSILON) {\n\
          \      /* one double root and one single root */\n\
          \        @REALTY@ U = std::cbrt(R); /* cube root function */\n\
          \        if (U > 0) {\n\
          \            eval[0] = 2*U;\n\
          \            eval[1] = -U;\n\
          \            eval[2] = -U;\n\
          \        } else {\n\
          \            eval[0] = -U;\n\
          \            eval[1] = -U;\n\
          \            eval[2] = 2*U;\n\
          \        }\n\
          \        roots = diderot::__details::ROOT_SINGLE_DOUBLE;\n\
          \    }\n\
          \    else {\n\
          \      /* a triple root! */\n\
          \        eval[0] = eval[1] = eval[2] = 0.0;\n\
          \        roots = diderot::__details::ROOT_TRIPLE;\n\
          \    }\n\
          \/* END #include \"teigen-evals-A.c\" */\n\
          \\n\
          \    tensor_3 ev = tensor_3 { eval[0], eval[1], eval[2] };\n\
          \    if (diderot::__details::ROOT_THREE == roots) {\n\
          \         nullspace1 (\n\
          \\t    tensor_3 { M00 - eval[0], M01, M02 },\n\
          \\t    tensor_3 { M01, M11 - eval[0], M12 },\n\
          \\t    tensor_3 { M02, M12, M22 - eval[0] },\n\
          \\t    evec[0]);\n\
          \        nullspace1 (\n\
          \\t    tensor_3 { M00 - eval[1], M01, M02 },\n\
          \\t    tensor_3 { M01, M11 - eval[1], M12 },\n\
          \\t    tensor_3 { M02, M12, M22 - eval[1] },\n\
          \\t    evec[1]);\n\
          \        nullspace1 (\n\
          \\t    tensor_3 { M00 - eval[2], M01, M02 },\n\
          \\t    tensor_3 { M01, M11 - eval[2], M12 },\n\
          \\t    tensor_3 { M02, M12, M22 - eval[2] },\n\
          \\t    evec[2]);\n\
          \    }\n\
          \    else if (diderot::__details::ROOT_SINGLE_DOUBLE == roots) {\n\
          \        if (eval[1] == eval[2]) {\n\
          \          /* one big (eval[0]) , two small (eval[1,2]) */\n\
          \            nullspace1 (\n\
          \\t\ttensor_3 { M00 - eval[0], M01, M02 },\n\
          \\t\ttensor_3 { M01, M11 - eval[0], M12 },\n\
          \\t\ttensor_3 { M02, M12, M22 - eval[0] },\n\
          \\t\tevec[0]);\n\
          \            nullspace2 (\n\
          \\t\ttensor_3 { M00 - eval[1], M01, M02 },\n\
          \\t\ttensor_3 { M01, M11 - eval[1], M12 },\n\
          \\t\ttensor_3 { M02, M12, M22 - eval[1] },\n\
          \\t\t&evec[1]);\n\
          \        }\n\
          \        else {\n\
          \          /* two big (eval[0,1]), one small (eval[2]) */\n\
          \            nullspace2 (\n\
          \\t\ttensor_3 { M00 - eval[0], M01, M02 },\n\
          \\t\ttensor_3 { M01, M11 - eval[0], M12 },\n\
          \\t\ttensor_3 { M02, M12, M22 - eval[0] },\n\
          \\t\t&evec[0]);\n\
          \            nullspace1 (\n\
          \\t\ttensor_3 { M00 - eval[2], M01, M02 },\n\
          \\t\ttensor_3 { M01, M11 - eval[2], M12 },\n\
          \\t\ttensor_3 { M02, M12, M22 - eval[2] },\n\
          \\t\tevec[2]);\n\
          \        }\n\
          \    }\n\
          \    else {\n\
          \      /* ROOT_TRIPLE == roots; use any basis for eigenvectors */\n\
          \        evec[0] = tensor_3 { 1, 0, 0 };\n\
          \        evec[1] = tensor_3 { 0, 1, 0 };\n\
          \        evec[2] = tensor_3 { 0, 0, 1 };\n\
          \    }\n\
          \  /* we always make sure it's really orthonormal; keeping fixed the\n\
          \   * eigenvector associated with the largest-magnitude eigenvalue\n\
          \   */\n\
          \    if (std::abs(eval[0]) > std::abs(eval[2])) {\n\
          \      /* normalize evec[0] but don't move it */\n\
          \        diderot::__details::normalize3(evec[0]._data);\n\
          \      // compute evec[1] -= scale3(dot3(evec[1], evec[0]), evec[0]);\n\
          \\t@REALTY@ s = dot3(evec[1], evec[0]);\n\
          \\tevec[1][0] -= s*evec[0][0];\n\
          \\tevec[1][1] -= s*evec[0][1];\n\
          \\tevec[1][2] -= s*evec[0][2];\n\
          \        diderot::__details::normalize3(evec[1]._data);\n\
          \        cross3(evec[0], evec[1], evec[2]);\n\
          \    }\n\
          \    else {\n\
          \      /* normalize evec[2] but don't move it */\n\
          \        diderot::__details::normalize3(evec[2]._data);\n\
          \      // compute evec[1] -= scale3(dot3(evec[1], evec[2]), evec[2]);\n\
          \\t@REALTY@ s = dot3(evec[1], evec[2]);\n\
          \\tevec[1][0] -= s*evec[2][0];\n\
          \\tevec[1][1] -= s*evec[2][1];\n\
          \\tevec[1][2] -= s*evec[2][2];\n\
          \        diderot::__details::normalize3(evec[1]._data);\n\
          \        cross3(evec[1], evec[2], evec[0]);\n\
          \    }\n\
          \    /* note that the right-handedness check has been folded into\n\
          \       the code above to enforce orthogonality.  Indeed, some work\n\
          \       could be removed by never really bothering to find all three\n\
          \       eigenvectors; just find two and then use the cross-product.\n\
          \       The next iteration of the code will do that */\n\
          \\n\
          \  /* multiply back by eigenvalue L2 norm */\n\
          \    eval[0] /= rnorm;\n\
          \    eval[1] /= rnorm;\n\
          \    eval[2] /= rnorm;\n\
          \\n\
          \  /* add back in the eigenvalue mean */\n\
          \    eval[0] += mean;\n\
          \    eval[1] += mean;\n\
          \    eval[2] += mean;\n\
          \\n\
          \    return roots;\n\
          \}\n\
          \/*---------- end eigenvecs3x3.in ----------*/\n\
          \"

    val execIncl = "\
          \/*---------- begin exec-incl.in ----------*/\n\
          \#define DIDEROT_STANDALONE_EXEC\n\
          \#define @DIDEROT_FLOAT_PRECISION@\n\
          \#define @DIDEROT_INT_PRECISION@\n\
          \#define @DIDEROT_TARGET@\n\
          \#include \"diderot/diderot.hxx\"\n\
          \\n\
          \#ifdef DIDEROT_ENABLE_LOGGING\n\
          \#define IF_LOGGING(...)         __VA_ARGS__\n\
          \#else\n\
          \#define IF_LOGGING(...)\n\
          \#endif\n\
          \/*---------- end exec-incl.in ----------*/\n\
          \"

    val libCXXIncl = "\
          \/*---------- begin lib-cxx-incl.in ----------*/\n\
          \#include \"@H_FILE@\"\n\
          \#include \"diderot/diderot.hxx\"\n\
          \\n\
          \#ifdef DIDEROT_ENABLE_LOGGING\n\
          \#define IF_LOGGING(...)         __VA_ARGS__\n\
          \#else\n\
          \#define IF_LOGGING(...)\n\
          \#endif\n\
          \\n\
          \static std::string ProgramName = \"@PROG_NAME@\";\n\
          \/*---------- end lib-cxx-incl.in ----------*/\n\
          \"

    val libHXXFoot = "\
          \/*---------- begin lib-hxx-foot.in ----------*/\n\
          \\n\
          \} // namespace @PREFIX@\n\
          \\n\
          \#endif // !@HXX_DEFINE@\n\
          \/*---------- end lib-hxx-foot.in ----------*/\n\
          \"

    val libHXXHead = "\
          \/*---------- begin lib-hxx-head.in ----------*/\n\
          \/*! \\file @HXX_FILE@\n\
          \ *\n\
          \ * C++ interface to library generated from @SRCFILE@.\n\
          \ *\n\
          \ * Command: @DIDEROTC_CMD@ @DIDEROTC_ARGV@\n\
          \ * Version: @DIDEROTC_VERSION@\n\
          \ */\n\
          \\n\
          \#ifndef @HXX_DEFINE@\n\
          \#define @HXX_DEFINE@\n\
          \\n\
          \#define @DIDEROT_FLOAT_PRECISION@\n\
          \#define @DIDEROT_INT_PRECISION@\n\
          \#define @DIDEROT_TARGET@\n\
          \\n\
          \#include <string>\n\
          \#include <cstdint>\n\
          \#include \"teem/nrrd.h\"\n\
          \\n\
          \namespace @PREFIX@ {\n\
          \\n\
          \struct World_t;\n\
          \/*---------- end lib-hxx-head.in ----------*/\n\
          \"

    val namespaceClose = "\
          \/*---------- begin namespace-close.in ----------*/\n\
          \\n\
          \} // namespace @PREFIX@\n\
          \/*---------- end namespace-close.in ----------*/\n\
          \"

    val namespaceOpen = "\
          \/*---------- begin namespace-open.in ----------*/\n\
          \namespace @PREFIX@ {\n\
          \\n\
          \static std::string ProgramName = \"@PROG_NAME@\";\n\
          \\n\
          \struct world;\n\
          \struct @STRAND@_strand;\n\
          \/*---------- end namespace-open.in ----------*/\n\
          \"

    val nrrdSaveHelper = "\
          \/*---------- begin nrrd-save-helper.in ----------*/\n\
          \/* helper function for saving output to nrrd file */\n\
          \inline bool nrrd_save_helper (std::string const &file, Nrrd *nin)\n\
          \{\n\
          \    if (nrrdSave (file.c_str(), nin, nullptr)) {\n\
          \        std::cerr << \"Error saving \\\"\" << file << \"\\\":\\n\" << biffGetDone(NRRD) << std::endl;\n\
          \        return true;\n\
          \    }\n\
          \    else {\n\
          \        return false;\n\
          \    }\n\
          \}\n\
          \\n\
          \/* Helper function for saving dynamic sequence output to a nrrd file.\n\
          \ * Dynamic sequence output is represented by two nrrds: one for lengths\n\
          \ * and one for the actual sequence data.  If the sum of the lengths is\n\
          \ * zero, then the second nrrd will be empty and should not be output,\n\
          \ * since teem barfs on empty nrrds.\n\
          \ */\n\
          \inline bool dynseq_save_helper (std::string const &prefix, Nrrd *lens, Nrrd *data)\n\
          \{\n\
          \    if (nrrd_save_helper(prefix + \"-len.nrrd\", lens)) {\n\
          \        return true;\n\
          \    }\n\
          \  // check for an empty data file; in this case nrrdEmpty will have been called\n\
          \  // on the data nrrd, so we can just check for a 0-dimension object.\n\
          \    if (data->dim == 0) {\n\
          \        std::cerr << \"Warning: all sequences in output are empty, so no '\"\n\
          \            << prefix << \"-data.nrrd' file produced\\n\";\n\
          \        return false;\n\
          \    }\n\
          \  // write the data\n\
          \    return nrrd_save_helper(prefix + \"-data.nrrd\", data);\n\
          \}\n\
          \\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \// version of dynseq_save_helper for snapshots\n\
          \inline bool dynseq_save_helper (\n\
          \    std::string const &prefix,\n\
          \    std::string const &suffix,\n\
          \    Nrrd *lens,\n\
          \    Nrrd *data)\n\
          \{\n\
          \    if (nrrd_save_helper(prefix + \"-len\" + suffix + \".nrrd\", lens)) {\n\
          \        return true;\n\
          \    }\n\
          \  // check for an empty data file; in this case nrrdEmpty will have been called\n\
          \  // on the data nrrd, so we can just check for a 0-dimension object.\n\
          \    std::string dataFile = prefix + \"-data\" + suffix + \".nrrd\";\n\
          \    if (data->dim == 0) {\n\
          \        std::cerr << \"Warning: all sequences in snapshot are empty, so no '\"\n\
          \            << dataFile << \"' file produced\\n\";\n\
          \        return false;\n\
          \    }\n\
          \  // write the data\n\
          \    return nrrd_save_helper(dataFile, data);\n\
          \}\n\
          \#endif // !DIDEROT_EXEC_SNAPSHOT\n\
          \/*---------- end nrrd-save-helper.in ----------*/\n\
          \"

  end
