C SUBROUTINE THAT DIAGONALIZES SYMMETRIC MATRIX TO GIVE VALUES AS DIAGONAL 
C ELEMENTS AND VECTORS IN U-MATRIX COLUMN-WISE
C     SUBROUTINE DIAL(N,U,A)
      SUBROUTINE DIAL(N,A)
      IMPLICIT REAL*8(A-H,O-Z)
      parameter(kk=300)
      DIMENSION UTEST(kk),U(kk,kk),A(kk,kk)
      EPS=1.0D-20
      DO 8 J=1,N    
      DO 9 I=1,N  
    9 U(I,J)=0.0D0
    8 U(J,J)=1.0D0
   10 AMAX=0.0D0
      DO 11 I=2,N   
      JUP=I-1 
      DO 11 J=1,JUP  
      AII=A(I,I) 
      AJJ=A(J,J) 
      AOD=A(I,J)  
      ASQ=AOD*AOD 
   28 IF(ASQ-AMAX)23,23,27  
   27 AMAX=ASQ  
   23 IF(ASQ-EPS)11,11,12   
   12 DIFFR=AII-AJJ   
      IF(DIFFR)13,15,15   
   13 SAGN=-2.0D0
      DIFFR=-DIFFR   
      GO TO 16   
   15 SAGN=2.0D0 
   16 TDEN=DIFFR+DSQRT(DIFFR**2+4.0D0*ASQ)
      TAM=SAGN*AOD/TDEN  
      CZ=1.0D0/(DSQRT(1.0D0+TAM**2))
      SP=CZ*TAM   
      DO 24 L=1,N  
      XJ=CZ*U(L,J)-SP*U(L,I)  
      U(L,I)=SP*U(L,J)+CZ*U(L,I) 
      U(L,J)=XJ  
      IF(L-J)18,24,18  
   18 IF(L-I)21,24,21  
   21 XJ=CZ*A(L,J)-SP*A(L,I) 
      A(L,I)=SP*A(L,J)+CZ*A(L,I) 
      A(L,J)=XJ  
      A(I,L)=A(L,I)   
      A(J,L)=A(L,J)   
   24 CONTINUE  
      A(I,I)=CZ*CZ*AII+SP*SP*AJJ+2.0D0*SP*CZ*AOD 
      A(J,J)=CZ*CZ*AJJ+SP*SP*AII-2.0D0*SP*CZ*AOD 
      A(I,J)=0.0D0
      A(J,I)=0.0D0
   11 CONTINUE    
      IF(AMAX-EPS)20,20,10  
 20   CONTINUE   
C     IF(IORVEC.EQ.0)  GO  TO  30  
      DO  40  L=1,N   
      ATEST=+1000000.0D0
      DO 41 J=L,N   
      IF(A(J,J)-ATEST)42,41,41   
   42 ATEST=A(J,J)    
      JTEST=J   
   41 CONTINUE   
      A(JTEST,JTEST)=A(L,L)   
      A(L,L)=ATEST   
      DO 40 I=1,N  
      UTEST(I)=U(I,JTEST)   
      U(I,JTEST)=U(I,L) 
      U(I,L)=UTEST(I)  
   40 CONTINUE  
      RETURN   
      END