C                  CVM Class Library
C                  http://cvmlib.com
C
C          Copyright Sergei Nikolaev 1992-2023
C Distributed under the Boost Software License, Version 1.0.
C    (See accompanying file LICENSE_1_0.txt or copy at
C          http://www.boost.org/LICENSE_1_0.txt)
C
C
C     Matrix axpy routines
C
C     The ?axpy routines perform a vector-vector operation defined as
C     y := a*x + y
C
C
C     Input/Output parameters:
C
C     M   - rows (int)(input)
C     N   - columns (int)(input)
C     A   - multiplier (real)(input)
C     X   - source matrix (real)(input)
C     LDX - leading dimesion of X (int)(input)
C     Y   - destination matrix (real)(output)
C     LDY - leading dimesion of Y (int)(input)

      SUBROUTINE SAXPYM (M, N, A, X, LDX, Y, LDY)
      INTEGER M, N, LDX, LDY
      REAL A, X(LDX*N), Y(LDY*N)
      INTEGER I

      IF (M .EQ. LDX .AND. M .EQ. LDY) THEN
          CALL SAXPY (M * N, A, X, 1, Y, 1)
      ELSE
          DO 10 I = 0, N-1
              CALL SAXPY (M, A, X(I*LDX+1), 1, Y(I*LDY+1), 1)
10        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE SAXPYM


      SUBROUTINE DAXPYM (M, N, A, X, LDX, Y, LDY)
      INTEGER M, N, LDX, LDY
      DOUBLE PRECISION A, X(LDX*N), Y(LDY*N)
      INTEGER I

      IF (M .EQ. LDX .AND. M .EQ. LDY) THEN
          CALL DAXPY (M * N, A, X, 1, Y, 1)
      ELSE
          DO 20 I = 0, N-1
              CALL DAXPY (M, A, X(I*LDX+1), 1, Y(I*LDY+1), 1)
20        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE DAXPYM


      SUBROUTINE CAXPYM (M, N, A, X, LDX, Y, LDY)
      INTEGER M, N, LDX, LDY
      COMPLEX A, X(LDX*N), Y(LDY*N)
      INTEGER I

      IF (M .EQ. LDX .AND. M .EQ. LDY) THEN
          CALL CAXPY (M * N, A, X, 1, Y, 1)
      ELSE
          DO 30 I = 0, N-1
              CALL CAXPY (M, A, X(I*LDX+1), 1, Y(I*LDY+1), 1)
30        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE CAXPYM


      SUBROUTINE ZAXPYM (M, N, A, X, LDX, Y, LDY)
      INTEGER M, N, LDX, LDY
      DOUBLE COMPLEX A, X(LDX*N), Y(LDY*N)
      INTEGER I

      IF (M .EQ. LDX .AND. M .EQ. LDY) THEN
          CALL ZAXPY (M * N, A, X, 1, Y, 1)
      ELSE
          DO 40 I = 0, N-1
              CALL ZAXPY (M, A, X(I*LDX+1), 1, Y(I*LDY+1), 1)
40        CONTINUE
      END IF
      RETURN
      END !SUBROUTINE ZAXPYM

