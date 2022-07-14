C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2022
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     g++/icc is messing with fortran functions returning complex numbers,
C     thus wrapping them around

      SUBROUTINE VCDOTU (DOT, N, X, INCX, Y, INCY)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::VCDOTU
CDEC$ ENDIF
      INTEGER N, INCX, INCY
      COMPLEX X(*), Y(*), DOT
      COMPLEX CDOTU

      DOT = CDOTU(N, X, INCX, Y, INCY)

      RETURN
      END !SUBROUTINE VCDOTU

      SUBROUTINE VZDOTU (DOT, N, X, INCX, Y, INCY)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::VZDOTU
CDEC$ ENDIF
      INTEGER N, INCX, INCY
      DOUBLE COMPLEX X(*), Y(*), DOT
      DOUBLE COMPLEX ZDOTU

      DOT = ZDOTU(N, X, INCX, Y, INCY)

      RETURN
      END !SUBROUTINE VZDOTU

      SUBROUTINE VCDOTC (DOT, N, X, INCX, Y, INCY)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::VCDOTC
CDEC$ ENDIF
      INTEGER N, INCX, INCY
      COMPLEX X(*), Y(*), DOT
      COMPLEX CDOTC

      DOT = CDOTC(N, X, INCX, Y, INCY)

      RETURN
      END !SUBROUTINE VCDOTC

      SUBROUTINE VZDOTC (DOT, N, X, INCX, Y, INCY)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::VZDOTC
CDEC$ ENDIF
      INTEGER N, INCX, INCY
      DOUBLE COMPLEX X(*), Y(*), DOT
      DOUBLE COMPLEX ZDOTC

      DOT = ZDOTC(N, X, INCX, Y, INCY)

      RETURN
      END !SUBROUTINE VZDOTC

