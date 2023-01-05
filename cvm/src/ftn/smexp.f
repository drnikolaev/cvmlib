C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2023
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Square matrix exponent
C
C     Input/Output parameters:
C
C     M   - size of matrix (int)(input)
C     A   - matrix (real)(input)
C     LDA - leading dimesion of A (int)(input)
C     EA  - exponent of A (real)(output)
C     LDE - leading dimesion of EA (int)(input)
C     TOL - tolerance (real)(input)
C     R   - working array of size NR (real)(input). IT MUST BE FILLED WITH ZEROES
C     IR  - working array of size NI (int)(input)
C     NR  - working array R size (int)(input).  IT MUST BE CALCULATED BY 'DMEXPC'
C     NI  - working array IR size (int)(input). IT MUST BE CALCULATED BY 'DMEXPC'
C     NQ  - row length (int)(input).            IT MUST BE CALCULATED BY 'DMEXPC'
C     J   - A measure (int)(input).             IT MUST BE CALCULATED BY 'DMEXPC'
C     ISSYMM - whether matrix A is symmetric (int)(input). 1 - true, 0 - false
C     WORK   - working array of size LWORK (real)(input)
C     LWORK  - length of working array WORK (int)(input) - usually 64 * M

      SUBROUTINE SMEXP (M, A, LDA, EA, LDE, R, IR, NR, NI, NQ, J,
     1                  ISSYMM, WORK, LWORK)   ! referenced in 
                                               ! symmetric case only
      INTEGER M, LDA, LDE
      REAL A(LDA*M), EA(LDE*M)
      INTEGER NR, NI, NQ, J, LWORK
      LOGICAL ISSYMM
      REAL R(NR), WORK(LWORK)
      INTEGER IR(NI)

      INTEGER I, IQ, NQC, NB, INFO, NNR
      INTEGER MM, MM1, MM12, MM13, MM14, MNB, MW
      REAL ZERO /0./, ONE /1./, TWO /2./
      DOUBLE PRECISION DTWO /2.D0/
      REAL TJ, C
      CHARACTER TRANS /'N'/
      CHARACTER UPLO /'U'/
      LOGICAL EVEN
      INTEGER FLOOR, CEILING

      IF (M .LE. 0) RETURN
      IF (M .EQ. 1) THEN
         EA(1) = EXP (A(1))
         RETURN
      END IF

      TJ   = 1.
      C    = 0.5

      MM   = M * M
      MM1  = MM   + 1
      MM12 = MM1  + MM
      MM13 = MM12 + MM
      MM14 = MM13 + MM

      NQC = FLOOR (DFLOAT (NQ) / DTWO) + 1
      MNB = MM14 + 2 * NQC
      NNR  = FLOOR (DFLOAT (NQC - 1) /
     1             DFLOAT (CEILING (DSQRT (DFLOAT (NQC - 1)))))
      NB  = (2 * NNR + 2) * MM
      MW  = MNB + NB


      EVEN = .TRUE.
      I    = 1
      R(MM14)       = ONE
      R(MM14 + NQC) = C
      DO 40 IQ = 2, NQ
          C = C * FLOAT (NQ - IQ + 1) / FLOAT ((2 * NQ - IQ + 1) * IQ)

          IF (EVEN) THEN
              R(MM14 + I) = C
          ELSE
              R(MM14 + NQC + I) = C
              I = I + 1
          ENDIF

          EVEN = .NOT. EVEN
40    CONTINUE

      CALL SCOPYM (M, M, A, LDA, EA, LDE)
      IF (J .GT. 0) THEN
          TJ = TWO ** FLOAT (-J)
          CALL SSCALM (M, M, TJ, EA, LDE)
      ENDIF

      CALL SCOPYM (M, M, EA, LDE, R(MM1), M)
      CALL SGEMM (TRANS, TRANS, M, M, M, ONE, R(MM1), M,
     1                 EA, LDE, ZERO, R, M)
      CALL SSCAL (MM, ZERO, R(MM1), 1)

C     U and V matrices calculation...
      CALL SPOLY2 (M, R, NQC, R(MM14), R(MM14 + NQC), R(MM1), R(MM12),
     1                 R(MNB), R(MNB + (NNR + 2) * MM))
      CALL SCOPYM (M, M, R(MM1), M, EA, LDE)
      CALL SGEMM  (TRANS, TRANS, M, M, M, TJ,
     1                 A, LDA, R(MM12), M, ONE, EA, LDE)   ! N matrix
      CALL SGEMM  (TRANS, TRANS, M, M, M, -TJ,
     1                 A, LDA, R(MM12), M, ONE, R(MM1), M) ! D matrix
      CALL SCOPY  (MM, R(MM1), 1, R(MM13), 1)              ! copy of D
      CALL SCOPYM  (M, M, EA, LDE, R(MM12), M)             ! copy of N

      IF (ISSYMM) THEN
          CALL SSYTRF (UPLO, M, R(MM1), M, IR, WORK, LWORK, INFO)
          CALL SCOPY  (MM, R(MM1), 1, R, 1)              ! copy of LD/UD
          CALL SSYTRS (UPLO, M, M, R(MM1), M, IR, EA, LDE, INFO)
          CALL SSYRFS (UPLO, M, M, R(MM13), M, R, M, IR, 
     1                 R(MM12), M, EA, LDE, R(MW), R(MW + M),
     2                 R(MW + 2*M), IR(M + 1), INFO)
      ELSE
          CALL SGETRF (M, M, R(MM1), M, IR, INFO)       ! factorize
          CALL SCOPY  (MM, R(MM1), 1, R, 1)             ! copy of LD/UD
          ! F matrix (EA) is  the solution of the equation DF = N
          CALL SGETRS (TRANS, M, M, R(MM1), M, IR, EA, LDE, INFO)
          CALL SGERFS (TRANS, M, M, R(MM13), M, R, M, IR,
     1                 R(MM12), M, EA, LDE, R(MW), R(MW + M),
     2                 R(MW + 2*M), IR(M + 1), INFO)
      ENDIF

      DO 50 I = 1, J
          CALL SCOPYM (M, M, EA, LDE, R, M)
          CALL SCOPYM (M, M, EA, LDE, R(MM1), M)
          CALL SGEMM (TRANS, TRANS, M, M, M, ONE,
     1                 R, M, R(MM1), M, ZERO, EA, LDE)     ! F = F * F
50    CONTINUE

      RETURN
      END !SUBROUTINE SMEXP


C     Input/Output parameters:
C
C     M   - size of matrix (int)(input)
C     A   - matrix (real)(input)
C     LDA - leading dimesion of A (int)(input)
C     TOL - tolerance (real)(input)
C     NR  - working array R size (int)(output)
C     NI  - working array IR size (int)(output)
C     NQ  - row length (int)(output)
C     J   - A measure (int)(output)

      SUBROUTINE SMEXPC (M, A, LDA, TOL, NR, NI, NQ, J)
      INTEGER M, LDA
      REAL A(LDA*M), TOL
      INTEGER NR, NI, NQ, J

      REAL ANRM, TMP, EQ
      REAL ONE /1./
      DOUBLE PRECISION DTWO /2.D0/
      DOUBLE PRECISION DTWOL /0.69314718055994530941723212145818D0/
      INTEGER NB, MM, NQC
      REAL SINFNM
      INTEGER FLOOR, CEILING
      REAL TINY

      NI = M * 2
      MM = M * M

      ANRM = SINFNM (M, M, A, LDA) 
      NQ = 1
      EQ = 0.16666666666666666666666666666667     ! 1/6
      DO 10 WHILE (.TRUE.)
          TMP = EQ * ANRM
          IF (TMP * EXP (TMP) .LT. TOL .AND. NQ .GT. 1) GOTO 15
          NQ = NQ + 1
          EQ = EQ / FLOAT (16 * (4 * NQ * NQ - 1))
10    CONTINUE

15    NQ = NQ + 1
      
      J = 0
      IF (ANRM .GT. TINY (ONE)) THEN
          J = 1 + CEILING (DLOG (DBLE(ANRM)) / DTWOL)     ! G77 bug fix
      ENDIF

      NQC = FLOOR (DFLOAT (NQ) / DTWO) + 1
      NB  = FLOOR (DFLOAT (NQC - 1) /
     1             DFLOAT (CEILING (DSQRT (DFLOAT (NQC - 1)))))
      NB  = (2 * NB + 2) * MM

      NR  = 4 * MM + 2 * NQC + NB + 5 * M
      RETURN
      END !SUBROUTINE SMEXPC


      SUBROUTINE SPOLY2 (M, A, N, V1, V2, P1, P2, B, B2)
      INTEGER M, N
      REAL A(M*M), V1(N), V2(N), P1(M*M), P2(M*M)
      REAL B(1), B2(1)
      INTEGER I, K, NS, NR, NQSR, MM, NRM
      REAL Q, ONE /1./, ZERO /0./
      CHARACTER TRANS /'N'/
      INTEGER FLOOR, CEILING

      MM = M * M
      DO 10 I = 1, MM, M + 1
          P1(I) = V1(1)
          P2(I) = V2(1)
10    CONTINUE

      Q    = FLOAT   (N - 1)        ! b(0) is V(1)
      NS   = CEILING (DSQRT (DBLE(Q)))
      NR   = FLOOR   (DBLE(Q) / DFLOAT (NS))
      NQSR = N - 1 - NS * NR        ! q - sr
      NRM  = (NR + 1) * MM + 1      ! the 1st el. of the last matrix

      CALL SCOPY (MM, A, 1, B, 1)   ! the first of these matrices
                                    ! will contain powers of A
      DO 20 K = 1, NR
          DO 30 I = 1, MM, M + 1
              B(MM * K + I)        = V1(NS * K + 1)  ! b(i)*I
              B2(MM * (K - 1) + I) = V2(NS * K + 1)  ! b(i)*I
30        CONTINUE
20    CONTINUE

      DO 40 I = 1, NS - 1
          CALL SAXPY (MM, V1(I + 1), B, 1, P1, 1)
          CALL SAXPY (MM, V2(I + 1), B, 1, P2, 1)

          DO 50 K = 1, NR - 1
              CALL SAXPY (MM, V1(NS * K + I + 1), B, 1,
     1                                        B(K * MM + 1), 1)
              CALL SAXPY (MM, V2(NS * K + I + 1), B, 1,
     1                                        B2((K - 1) * MM + 1), 1)
50        CONTINUE

          IF (I .LE. NQSR) THEN
              CALL SAXPY (MM, V1(NS * NR + I + 1), B, 1,
     1                                        B(NR * MM + 1), 1)
              CALL SAXPY (MM, V2(NS * NR + I + 1), B, 1,
     1                                        B2((NR - 1) * MM + 1), 1)
          ENDIF

          CALL SCOPY (MM, B, 1, B(NRM), 1)
          CALL SGEMM (TRANS, TRANS, M, M, M, ONE, B(NRM), M, A, M,
     1                                           ZERO, B, M)
40    CONTINUE

      CALL SGEMM (TRANS, TRANS, M, M, M, ONE, B(MM + 1), M,
     1                                           B, M, ONE, P1, M)
      CALL SGEMM (TRANS, TRANS, M, M, M, ONE, B2, M,
     1                                           B, M, ONE, P2, M)

      IF (NR .GT. 1) CALL SCOPY (MM, B, 1, B(NRM), 1)

      DO 60 K = 2, NR
          CALL SCOPY (MM, B, 1, B(MM + 1), 1)
          CALL SGEMM (TRANS, TRANS, M, M, M, ONE, B(MM + 1), M,
     1                                           B(NRM), M, ZERO, B, M)
          CALL SGEMM (TRANS, TRANS, M, M, M, ONE,
     1                      B(K * MM + 1), M, B, M, ONE, P1, M)
          CALL SGEMM (TRANS, TRANS, M, M, M, ONE,
     1                      B2((K - 1) * MM + 1), M, B, M, ONE, P2, M)
60    CONTINUE

      RETURN
      END !SUBROUTINE SPOLY2

