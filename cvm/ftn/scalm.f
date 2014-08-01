C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2013
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Matrix scaling routines
C
C     Input/Output parameters:
C
C     M   - rows (int)(input)
C     N   - columns (int)(input)
C     S   - scale factor (real)(input)
C     A   - matrix to be scaled (real)(input, output)
C     LDA - leading dimesion of A (int)(input)

      SUBROUTINE SSCALM (M, N, S, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::SSCALM
CDEC$ ENDIF
      INTEGER M, N, LDA
      REAL S
      REAL A(LDA*N)
      INTEGER I

      IF (M .EQ. LDA) THEN
          CALL SSCAL (M * N, S, A, 1)
      ELSE
          DO 10 I = 0, N-1
              CALL SSCAL (M, S, A(I*LDA+1), 1)
10        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE SSCALM

      SUBROUTINE DSCALM (M, N, S, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::DSCALM
CDEC$ ENDIF
      INTEGER M, N, LDA
      DOUBLE PRECISION S
      DOUBLE PRECISION A(LDA*N)
      INTEGER I

      IF (M .EQ. LDA) THEN
          CALL DSCAL (M * N, S, A, 1)
      ELSE
          DO 10 I = 0, N-1
              CALL DSCAL (M, S, A(I*LDA+1), 1)
10        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE DSCALM

      SUBROUTINE CSCALM (M, N, S, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::CSCALM
CDEC$ ENDIF
      INTEGER M, N, LDA
      COMPLEX S
      COMPLEX A(LDA*N)
      INTEGER I

      IF (M .EQ. LDA) THEN
          CALL CSCAL (M * N, S, A, 1)
      ELSE
          DO 10 I = 0, N-1
              CALL CSCAL (M, S, A(I*LDA+1), 1)
10        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE CSCALM

      SUBROUTINE ZSCALM (M, N, S, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::ZSCALM
CDEC$ ENDIF
      INTEGER M, N, LDA
      DOUBLE COMPLEX S
      DOUBLE COMPLEX A(LDA*N)
      INTEGER I

      IF (M .EQ. LDA) THEN
          CALL ZSCAL (M * N, S, A, 1)
      ELSE
          DO 10 I = 0, N-1
              CALL ZSCAL (M, S, A(I*LDA+1), 1)
10        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE ZSCALM
