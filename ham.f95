       ! 1 D HARMONIC OSCILLATOR
       IMPLICIT REAL*8(A-H,O-Z)
       PARAMETER (NIN=300)
       DIMENSION V(NIN),G(NIN),SHI(NIN),X(NIN)
       WRITE(*,*) 'X0,XMAX,N'
       READ(*,*) X0,XMAX,N
       S=(XMAX-X0)/N
       SS=S*S 
       E=0.0
       SHI(1)=0.000
       SHI(2)=0.0001
       WRITE(*,*) 'THE INCREMENT IS',S
       
       DO I=1,N
       X(I)=X0+((I-1)*S)
      !WRITE (*,*) 'X(I) =', X(I)
       END DO
       
       !WRITE(*,*) 'SHI(1)',SHI(1),'SHI(2)',SHI(2)
       V(1)=0.5*(X0**2)
       V(2) = 0.5*((X0+S)**2)
       G(1)=2.0 * (V(1)-E)
       G(2)=2.0 * (V(2)-E)
     	WRITE(*,*) 'THE VALUE OF V(1)',V(1),'THE VALUE OF V(2)',V(2)
     	WRITE(*,*) 'THE VALUE OF G(1)',G(1),'THE VALUE OF G(2)',G(2)
        
  10  DO I=2,N
      V(I)=(0.5*(X0+(I-1)*S)**2)
      G(I)=2*(V(I)-E)
      !WRITE(*,*)'V(I)',V(I),'G(I)',G(I)
      END DO
      
      DO I=2,N
      SHI(I+1)=2*SHI(I)-SHI(I-1)+((5.0/6.0)*G(I)*SHI(I)*SS)
      SHI(I+1)=SHI(I+1)+(1.0/12.0)*G(I-1)*SHI(I-1)*SS
       SHI(I+1)=SHI(I+1)/(1-((SS/12.0)*G(I+1))
       !IF (I .EQ. N) THEN
       !IF (ABS(SHI(I)) .LT. 0.0000000001) THEN
	WRITE (2,*) X(I), SHI(I)
	!ELSE
	!E = E + 0.1
	!GO TO 10
	!END IF
	!END IF
       END DO
       STOP
       END