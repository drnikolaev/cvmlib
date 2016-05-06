C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2016
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Matrix infinity norm routines
C
C     Input/Output parameters:
C
C     M   - rows (int)(input)
C     N   - columns (int)(input)
C     A   - matrix (real)(input)
C     LDA - leading dimesion of A (int)(input)

      REAL FUNCTION SINFNM (M, N, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::SINFNM
CDEC$ ENDIF
      INTEGER M, N, LDA
      REAL A(LDA*N)
      INTEGER I
      REAL S
      INTEGER ISAMAX

      SINFNM = 0.
      IF (M .EQ. LDA) THEN
          SINFNM = ABS (A (ISAMAX (M * N, A, 1)))
      ELSE
          DO 10 I = 0, N-1
              S = ABS (A (ISAMAX (M, A(I*LDA+1), 1)))
              IF (S .GT. SINFNM) SINFNM = S
10        CONTINUE
      END IF
      RETURN
      END !FUNCTION SINFNM


      DOUBLE PRECISION FUNCTION DINFNM (M, N, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::DINFNM
CDEC$ ENDIF
      INTEGER M, N, LDA
      DOUBLE PRECISION A(LDA*N)
      INTEGER I
      DOUBLE PRECISION S
      INTEGER IDAMAX

      DINFNM = 0.D0
      IF (M .EQ. LDA) THEN
          DINFNM = DABS (A (IDAMAX (M * N, A, 1)))
      ELSE
          DO 20 I = 0, N-1
              S = DABS (A (IDAMAX (M, A(I*LDA+1), 1)))
              IF (S .GT. DINFNM) DINFNM = S
20        CONTINUE
      END IF
      RETURN
      END !FUNCTION DINFNM


      REAL FUNCTION CINFNM (M, N, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::CINFNM
CDEC$ ENDIF
      INTEGER M, N, LDA
      COMPLEX A(LDA*N)
      INTEGER I
      REAL S
      INTEGER ICAMAX

      CINFNM = (0., 0.)
      IF (M .EQ. LDA) THEN
          CINFNM = CABS (A (ICAMAX (M * N, A, 1)))
      ELSE
          DO 20 I = 0, N-1
              S = CABS (A (ICAMAX (M, A(I*LDA+1), 1)))
              IF (S .GT. CINFNM) CINFNM = S
20        CONTINUE
      END IF
      RETURN
      END !FUNCTION CINFNM


      DOUBLE PRECISION FUNCTION ZINFNM (M, N, A, LDA) 
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::ZINFNM
CDEC$ ENDIF
      INTEGER M, N, LDA
      DOUBLE COMPLEX A(LDA*N)
      INTEGER I
      DOUBLE PRECISION S
      INTEGER IZAMAX

      ZINFNM = (0.D0, 0.D0)
      IF (M .EQ. LDA) THEN
          ZINFNM = ZABS (A (IZAMAX (M * N, A, 1)))
      ELSE
          DO 20 I = 0, N-1
              S = ZABS (A (IZAMAX (M, A(I*LDA+1), 1)))
              IF (S .GT. ZINFNM) ZINFNM = S
20        CONTINUE
      END IF
      RETURN
      END !FUNCTION ZINFNM

