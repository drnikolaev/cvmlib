cvmlib
======

CVM Class Library

This C++ class library encapsulates concepts of vector and different matrices including square, band, symmetric and hermitian ones in Euclidean space of real and complex numbers. It utilizes BLAS and LAPACK Fortran libraries in order to achieve the best numerical performance possible. Along with basic vector and matrix arithmetic it contains different algorithms including norm computations, elementary transformations, solving of linear systems of kind Ax=b and AX=B, singular value decomposition, matrix rank and determinant computation, non-symmetric and symmetric eigenvalue problem (including Cholesky and Bunch-Kaufman factorization), LU factorization, QR, RQ, LQ and QL factorizations, different linear least square problems solutions, square matrix polynomials, square matrix inversion, pseudo (generalized) inversion and square matrix exponent. All these algorithms are implemented for real and complex numbers. Functional classes, vectors anf matrices are later additions to the library (since ver. 7.0).

Starting from version 8.0 Library implements new features of C++11 Standard and is no longer compatible with older compilers. For example, move constructors deliver better performance by requiring less number of memory allocations and deallocations.

The library is distributed under the Boost Software License, Version 1.0. Current version is 8.1, July 23rd, 2014. The following compilers and integrated development environments are currently supported:

    Intel Parallel Studio XE 2013 and higher for Windows and Linux
    Microsoft Visual Studio 2012
    Microsoft Visual Studio 2013
    gfortran compiler 4.3 and higher
    g++ 4.7.1 and higher
    clang 3.4 and higher
    MinGW environments (4.7.1 and higher): CygWin and TDM-GCC (under Minimal System)

See http://cvmlib.com for binaries and more details.