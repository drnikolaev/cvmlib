cvmlib
======

CVM Class Library.

This C++ class library encapsulates concepts of vector and different matrices including square, band, symmetric and hermitian ones in Euclidean space of real and complex numbers. It utilizes BLAS and LAPACK Fortran libraries in order to achieve the best numerical performance possible. Along with basic vector and matrix arithmetic it contains different algorithms including norm computations, elementary transformations, solving of linear systems of kind Ax=b and AX=B, singular value decomposition, matrix rank and determinant computation, non-symmetric and symmetric eigenvalue problem (including Cholesky and Bunch-Kaufman factorization), LU factorization, QR, RQ, LQ and QL factorizations, different linear least square problems solutions, square matrix polynomials, square matrix inversion, pseudo (generalized) inversion and square matrix exponent. All these algorithms are implemented for real and complex numbers. Functional classes, vectors anf matrices are later additions to the library (since ver. 7.0).
The library is distributed under the Boost Software License, Version 1.0.
See http://cvmlib.com for binaries and more details.