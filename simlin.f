      IMPLICIT REAL*8(A-H,O-Z)
      REAL XX
      PARAMETER (NUM=50)
      DIMENSION Y(NUM),X(NUM),A(NUM,NUM)
      PRINT *,'ENTER THE DIMENSION OF SQUARE MATRIX'
      READ *,N
      PRINT *,'ENTER THE VALUES IN MATRIX A'
      READ *,((A(I,J),J=1,N),I=1,N)
      PRINT *,'ENTER THE VALUES IN PRODUCT MATRIX Y'
      READ *, (Y(I),I=1,N)
      xx=A(1,1)
      ILOC=1
      yy=A(2,2)
      JLOC=2
      ! TO FIND THE PIVOT ELEMENT OF 1ST COLUMN
      DO I=2,N
      IF (xx.LT.A(I,1))THEN
      xx=A(I,1)
      ILOC=I
      ENDIF
      ENDDO
      BLARGE = XX
      PRINT *, 'PIVOT ELEMENT= ',BLARGE,' AND ITS LOCATION=',ILOC
      ! SWAPPING THE PIVOT ELEMENT'S ROW WITH 1ST ROW
      DO J=1,N
      TEMP=A(1,J)
      A(1,J)=A(ILOC,J)
      A(ILOC,J)=TEMP
      ENDDO
      TEMP=Y(1)
      Y(1)=Y(ILOC)
      Y(ILOC)=TEMP
      ! MATRIX AFTER 1ST OPERATION
      DO 10 I=2,N
      FACT=A(I,1)/A(1,1)
      DO 20 J=1,N
      A(I,J)=A(I,J)-FACT*A(1,J)
20    CONTINUE
      Y(I)=Y(I)-FACT*Y(1)
10    CONTINUE
      PRINT *, 'MATRIX AFTER 1ST OPERATION:'
      WRITE (*,11) ((A(I,J),J=1,N),I=1,N)
      ! TO FIND THE PIVOT ELEMENT OF 2ND COLUMN EXCLUDING ROW 1
      DO I=3,N
      IF (yy.LT.A(I,2))THEN
      yy=A(I,2)
      JLOC=I
      ENDIF
      ENDDO
      ALARGE = YY
      PRINT *, '2ND PIVOT ELEMENT= ',ALARGE, ' WITH LOCATION=',JLOC
      ! SWAPPING 2ND PIVOT'S ROW WITH THE OTHER ROW
      DO J=2,N
      TEMP=A(2,J)
      A(2,J)=A(JLOC,J)
      A(JLOC,J)=TEMP
      ENDDO
      TEMP=Y(2)
      Y(2)=Y(JLOC)
      Y(JLOC)=TEMP
      ! MATRIX AFTER 2ND OPERATION
      DO 30 I=3,N
      FACT=A(I,2)/A(2,2)
      DO 40 J=1,N
      A(I,J)=A(I,J)-FACT*A(2,J)
40    CONTINUE
      Y(I)=Y(I)-FACT*Y(2)
30    CONTINUE
      PRINT *, 'MATRIX AFTER 2ND OPERATION:'
      WRITE (*,11) ((A(I,J),J=1,N),I=1,N)
      PRINT *, 'NEW Y MATRIX IS:'
      WRITE (*,12) (Y(I),I=1,N)
      ! TO FIND OUT X1, X2, X3 USING BACK SUBSTITUTION
      X(N)=Y(N)/A(N,N)
      DO I=N-1,1,-1
      SUMM=0.0
      DO J=I+1,N
      TERM = A(I,J)*X(J)
      SUMM = SUMM + TERM
      ENDDO
      X(I)=(Y(I)-SUMM)/A(I,I)
      ENDDO

      PRINT *,'MATRIX X IS:'
      WRITE(*,12) (X(J),J=1,N)
  11  FORMAT(1x,3F13.6)
  12  FORMAT(1x,1F13.6)
      STOP
      END