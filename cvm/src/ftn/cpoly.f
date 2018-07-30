C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2016
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Matrix polynom
C
C     Input/Output parameters:
C
C     M     - size of input square matrix (input)
C     A     - matrix (input)
C     LDA   - leading dimension of A (int)(input)
C     N     - size of coefficient array V (input)
C     V     - coefficient array (input)
C     P     - result square matrix (output) = V(1)*I + V(2)*A + V(3)*A^2 + ... + V(N)*A^(N-1)
C     LDP   - leading dimension of P (int)(input)
C     B     - working array of size NPOLY (M, N)

      SUBROUTINE CPOLY (M, A, LDA, N, V, P, LDP, B)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$     ATTRIBUTES DLLEXPORT::CPOLY
CDEC$ ENDIF
      INTEGER M, N, LDA, LDP
      COMPLEX A(LDA*M), V(N), P(LDP*M), B(1)
      INTEGER I, K, NS, NR, NQSR, MM, NRM, NWRK
      COMPLEX CONE /(1.,0.)/, CZERO /(0.,0.)/
      DOUBLE PRECISION Q
      CHARACTER TRANS /'N'/
      INTEGER NPOLY
      INTEGER FLOOR, CEILING

      IF (M .LE. 0) RETURN
      MM  = M * M

      CALL CSCAL (MM, CZERO, P, 1)
      IF (N .LE. 0) RETURN

      DO 10 I = 0, M-1
          P(I*(LDP+1)+1) = V(1)
10    CONTINUE

      IF (N .EQ. 1) RETURN

      Q    = DFLOAT  (N - 1)        ! b(0) is V(1)
      NS   = CEILING (DSQRT (Q))
      NR   = FLOOR   (Q / DFLOAT (NS))
      NQSR = N - 1 - NS * NR        ! q - sr
      NRM  = (NR + 1) * MM + 1      ! the 1st el. of the last matrix

      NWRK = NPOLY (M, N)
      DO 15 I = MM + 1, NWRK
          B(I) = CZERO
15    CONTINUE

      CALL CCOPYM (M, M, A, LDA, B, M)   ! the first of these matrices
                                         ! will contain powers of A
      DO 20 K = 1, NR
          DO 30 I = 1, MM, M + 1
              B(MM * K + I) = V(NS * K + 1)  ! b(i)*I
30        CONTINUE
20    CONTINUE

      DO 40 I = 1, NS - 1
          CALL CAXPYM (M, M, V(I + 1), B, M, P, LDP)

          DO 50 K = 1, NR - 1
              CALL CAXPY (MM, V(NS * K + I + 1), B, 1,
     1                                           B(K * MM + 1), 1)
50        CONTINUE

          IF (I .LE. NQSR) THEN
              CALL CAXPY (MM, V(NS * NR + I + 1), B, 1, 
     1                                           B(NR * MM + 1), 1)
          ENDIF

          CALL CCOPY (MM, B, 1, B(NRM), 1)
          CALL CGEMM (TRANS, TRANS, M, M, M, CONE, B(NRM), M, A, LDA,
     1                                           CZERO, B, M)
40    CONTINUE

      CALL CGEMM (TRANS, TRANS, M, M, M, CONE, B(MM + 1), M,
     1                                           B, M, CONE, P, LDP)


      IF (NR .GT. 1) CALL CCOPY (MM, B, 1, B(NRM), 1)

      DO 60 K = 2, NR
          CALL CCOPY (MM, B, 1, B(MM + 1), 1)
          CALL CGEMM (TRANS, TRANS, M, M, M, CONE, B(MM + 1), M, 
     1                                         B(NRM), M, CZERO, B, M)
          CALL CGEMM (TRANS, TRANS, M, M, M, CONE, B(K * MM + 1), M,
     1                                         B, M, CONE, P, LDP)
60    CONTINUE

      RETURN
      END !SUBROUTINE CPOLY

