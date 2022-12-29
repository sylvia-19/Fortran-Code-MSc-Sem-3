!       DIAGONALIZATION OF A SYMMETRIC MATRIX: JACOBI METHOD
        IMPLICIT NONE
!       A:ORIGINAL MATRIX; D:DIAGONAL MATRIX;
        INTEGER I,J,K,P,Q,N,NCOUNT,IT,MAX
        REAL A(100,100),HIGH,C,S,T(100,100),PHI,D(100,100)
        !REAL XPJ,XQJ
        REAL TEMP
        !EAL B(100)
!       N:NO. OF ROWS AND COLUMNS
	PRINT *, 'ENTER THE DIMENSION OF THE MATRIX'
        READ(*,*)N    
        MAX=((N**2)-N)
!       INITIALIZE THE TRANSFORMING MATRIX(T)
        DO I=1,N
        DO J=1,N
        IF(I.EQ.J)THEN
        T(I,J)=1.0
        ELSE
        T(I,J)=0.0
        ENDIF
        ENDDO
        ENDDO
        PRINT *, 'ENTER THE VALUES IN MATRIX A'
        DO I=1,N
        READ *, (A(I,J),J=1,N)
        ENDDO
! 	INITIALISING THE DIAGONAL MATRIX
        DO I=1,N
        DO J=1,N
        D(I,J)=0.0D0
        ENDDO
        ENDDO
        IT=0
        PRINT *, 'THE DIAGONALISED MATRIX IS:'
!       SEARCH FOR THE HIGHEST OFF-DIAGONAL ELEMENT
  12    HIGH=0.0D0
        P=0
        Q=0
        IT=IT+1
        DO I=1,N
        DO J=1,N
        IF((I.NE.J).AND.(ABS(A(I,J)).GT.HIGH))THEN
        HIGH=ABS(A(I,J))
        P=I
        Q=J
        ENDIF
        ENDDO
        ENDDO
!       FINDING THE ANGLE OF TRANSFORMATION
        PHI=(0.5D0)*ATAN((2.0D0*A(P,Q))/(A(Q,Q)-A(P,P)))
        C=COS(PHI)
        S=SIN(PHI)
!       GENERATION OF DIAGONAL MATRIX
        D(P,Q)=0.0D0
        D(Q,P)=0.0D0
        D(P,P)=(C*C*A(P,P))+(S*S*A(Q,Q))-(2.0D0*C*S*A(P,Q))
        D(Q,Q)=(S*S*A(P,P))+(C*C*A(Q,Q))+(2.0D0*C*S*A(P,Q))
        DO J=1,N
        IF((J.NE.P).AND.(J.NE.Q))THEN
        D(J,P)=(C*A(J,P))-(S*A(J,Q))
        D(P,J)=D(J,P)
        D(J,Q)=(C*A(J,Q))+(S*A(J,P))
        D(Q,J)=D(J,Q)
          DO I=1,N
          IF((I.NE.P).AND.(I.NE.Q))THEN
          D(J,I)=A(J,I)
          D(I,J)=A(I,J)
          ENDIF
          ENDDO
        ENDIF
        ENDDO
!       CHANGING THE TRANSFORMING MATRIX
        !DO J=1,N
        !XPJ=T(J,P)
        !XQJ=T(J,Q)
        !T(J,P) = C*XPJ - S*XQJ
        !T(J,Q) = S*XPJ + C*XQJ
        !ENDDO
!       CONDITION TO STOP CALCULATION
        NCOUNT=0
        DO I=1,N
        DO J=1,N
        IF(ABS(D(I,J)).LT.1E-30)THEN
        D(I,J)=0.0D0
        NCOUNT=NCOUNT+1
        ENDIF
        A(I,J)=D(I,J)
        ENDDO
        ENDDO
        IF(NCOUNT.GE.MAX)THEN
        DO I=1,N
        WRITE(*,*)(D(I,J),J=1,N)
        ENDDO
        !WRITE(*,*)'THE TRANSFORMATION MATRIX IS:'
        !DO I=1,N
        !WRITE(*,*)(T(I,J),J=1,N)
        !ENDDO
        GOTO 11
        ELSE
        GOTO 12
        ENDIF
!       ARRANGING THE DIAGONAL ELEMENTS IN ASCENDING ORDER(BUBBLE SORT)
   11   DO J=1,N-1
        DO K=J+1,N
        IF(D(K,K).LT.D(J,J))THEN
        TEMP=D(J,J)
        D(J,J)=D(K,K)
        D(K,K)=TEMP
          !DO I=1,N
          !B(I)=T(I,J)
          !T(I,J)=T(I,K)
          !T(I,K)=B(I)
          !ENDDO
        ENDIF
        ENDDO
        ENDDO
        !WRITE(*,*)'THE EIGEN VALUES ARRANGED IN ASCENDING ORDER ARE:'
        !DO I=1,N
        !WRITE(*,*)(D(I,J),J=1,N)
        !ENDDO
        !WRITE(*,*)'THE EIGEN VECTOR MATRIX IS:'
        !DO I=1,N
        !WRITE(*,*)(T(I,J),J=1,N)
        !ENDDO
        END