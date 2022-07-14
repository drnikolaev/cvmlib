C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2022
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Working array size calculator
C

      INTEGER FUNCTION NPOLY (M, N)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::NPOLY
CDEC$ ENDIF
      INTEGER M, N
      INTEGER FLOOR, CEILING

      NPOLY = 0
      IF (M .GT. 0 .AND. N .GT. 1) THEN
          NPOLY  = (FLOOR (DFLOAT (N - 1) /
     1              DFLOAT (CEILING (DSQRT (DFLOAT (N - 1)))))
     2              + 2) * M * M
      ENDIF
      RETURN
      END !FUNCTION NPOLY

