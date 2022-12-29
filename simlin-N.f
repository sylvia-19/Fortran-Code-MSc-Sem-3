      IMPLICIT REAL*8 (A-H, O-Z)
      PARAMETER (L=50)
      DIMENSION A(L,L), Y(L), X(L)
      PRINT *, 'ENTER THE DIMENSION OF MATRIX A'
      READ *, N
      PRINT *,'ENTER THE VALUES IN MATRIX A'
      READ *,((A(I,J),J=1,N),I=1,N)
      PRINT *,'ENTER THE VALUES IN PRODUCT MATRIX Y'
      READ *, (Y(I),I=1,N)
    ! TO FIND THE MATRIX AFTER GAUSSIAN ELIMINATION
      DO 10 K=1,N-1
      CALL INTERCHANGE(N,A,K,Y)
      PIVOT = A(K,K)
      DO 20 I=K+1,N
      FACTOR = A(I,K)/PIVOT
      DO 30 J=1,N+1
      A(I,J) = A(I,J) - FACTOR*(A(K,J))
  30  CONTINUE
      Y(I) = Y(I) - FACTOR*(Y(K))
  20  CONTINUE
  10  CONTINUE
      ! TO FIND OUT X1, X2, X3 USING BACK SUBSTITUTION
      X(N)=Y(N)/A(N,N)
      DO I=N-1,1,-1
      SUMM=0.0
      DO J=I+1,N
      SUMM = SUMM + A(I,J)*X(J)
      ENDDO
      X(I)=(Y(I)-SUMM)/A(I,I)
      ENDDO
      PRINT *,'MATRIX X IS:'
      WRITE(*,*) (X(J),J=1,N)
      STOP
      END

      ! SUBROUTINE TO FIND OUT THE PIVOT AND INTERCHANGING THE ROWS
      SUBROUTINE INTERCHANGE (N,A,K,Y)
      IMPLICIT REAL*8 (A-H, O-Z)
      PARAMETER (L=50)
      DIMENSION A(L,L), Y(L)
      N1 = K  ! N1 = UPDATED ROW INDEX
      BLARGE = A(K,K)
      DO 5 I=K+1,N
      IF ((A(I,K)) .GT. (A(K,K))) THEN
      BLARGE = A(I,K)
      N1 = I
      END IF
  5   CONTINUE
      ! INTERCHANGING THE ITH ROW WITH KTH ROW
      DO J=1,N
      TEMP = A(N1,J)
      A(N1,J) = A(K,J)
      A(K,J) = TEMP
      END DO
      TEMP = Y(K)
      Y(K) = Y(N1)
      Y(N1) = TEMP
      RETURN
      END