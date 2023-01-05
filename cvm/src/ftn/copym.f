C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2023
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Matrix copy routines
C
C     Input/Output parameters:
C
C     M   - rows (int)(input)
C     N   - columns (int)(input)
C     A   - source matrix (real)(input)
C     B   - destination matrix (real)(output)
C     LDA - leading dimesion of A (int)(input)
C     LDB - leading dimesion of B (int)(input)

      SUBROUTINE SCOPYM (M, N, A, LDA, B, LDB)
      INTEGER M, N, LDA, LDB
      REAL A(LDA*N), B(LDB*N)
      INTEGER I

      IF (M .EQ. LDA .AND. M .EQ. LDB) THEN
          CALL SCOPY (M * N, A, 1, B, 1)
      ELSE
          DO 10 I = 0, N-1
              CALL SCOPY (M, A(I*LDA+1), 1, B(I*LDB+1), 1)
10        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE SCOPYM

      SUBROUTINE DCOPYM (M, N, A, LDA, B, LDB)
      INTEGER M, N, LDA, LDB
      DOUBLE PRECISION A(LDA*N), B(LDB*N)
      INTEGER I

      IF (M .EQ. LDA .AND. M .EQ. LDB) THEN
          CALL DCOPY (M * N, A, 1, B, 1)
      ELSE
          DO 20 I = 0, N-1
              CALL DCOPY (M, A(I*LDA+1), 1, B(I*LDB+1), 1)
20        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE DCOPYM

      SUBROUTINE CCOPYM (M, N, A, LDA, B, LDB)
      INTEGER M, N, LDA, LDB
      COMPLEX A(LDA*N), B(LDB*N)
      INTEGER I

      IF (M .EQ. LDA .AND. M .EQ. LDB) THEN
          CALL CCOPY (M * N, A, 1, B, 1)
      ELSE
          DO 30 I = 0, N-1
              CALL CCOPY (M, A(I*LDA+1), 1, B(I*LDB+1), 1)
30        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE CCOPYM

      SUBROUTINE ZCOPYM (M, N, A, LDA, B, LDB)
      INTEGER M, N, LDA, LDB
      DOUBLE COMPLEX A(LDA*N), B(LDB*N)
      INTEGER I

      IF (M .EQ. LDA .AND. M .EQ. LDB) THEN
          CALL ZCOPY (M * N, A, 1, B, 1)
      ELSE
          DO 40 I = 0, N-1
              CALL ZCOPY (M, A(I*LDA+1), 1, B(I*LDB+1), 1)
40        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE ZCOPYM

