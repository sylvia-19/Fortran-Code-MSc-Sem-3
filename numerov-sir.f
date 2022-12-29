       PROGRAM NUMEROV_METHOD
       IMPLICIT NONE
       REAL*8 :: X(1000),G(1000),PSI(1000),E,EMAX,S,SS,EU,EL,TEMP
       INTEGER :: I,N,M,V,K
       
! V IS THE VIBRATIONAL QUANTUM NUMBER 
       WRITE(*,*)'GIVE V'
       READ(*,*)V

!EU = UPPER LIMIT OF ENEGRY
!EL = LOWER LIMIT OF ENERGY
       EU=5.0D0
       EL=0.0D0

!X = VALUES ALONG X-COORDINATE
       X(1)=-5.0D0

!S = GAP BETWEEN TWO SUCCESSIVE X VALUES
       S=0.10D0

!M = TOTAL  NUMBER OF POINT       
       M=100

!PSI WAVE FUNCTION
       PSI(1)=0.0D0
       PSI(2)=0.0001D0
       X(2)=X(1)+S

!E = ENERGY
       E=EL       

!TEMP IS DEFINED FOR CHECKING WHETHER PSI VANISHES AT THE FINAL POINT       
       TEMP=100000
       DO

!N = NODE        
         N=0
         G(1)=(X(1)*X(1))-(2.0D0*E)
         G(2)=(X(2)*X(2))-(2.0D0*E)
         SS=(S*S)/12.0D0
          DO I=2,M-1,1
            X(I+1)=X(I)+S
            G(I+1)=(X(I+1)*X(I+1))-(2.0D0*E)
            PSI(I+1)=(2.0D0*PSI(I))-(PSI(I-1))+((10.0D0*PSI(I)*G(I)*SS))
	    PSI(I+1)=PSI(I+1)+(PSI(I-1)*G(I-1)*SS)
            PSI(I+1)=PSI(I+1)/(1.0D0-(G(I+1)*SS))

!CHEK WHETHER NODE IS PRESENT OR NOT       
            IF((PSI(I+1)*PSI(I)).LT.0.0D0)THEN
            N=N+1
            ENDIF
           ENDDO

!CHECKING WHETHER PSI VANISHES AT THE FINAL POINT       
       IF(ABS(TEMP-PSI(M)).LT.1E-010)EXIT  
       TEMP=PSI(M)

        IF(N.LE.V)THEN
           EL=E
        ELSE
           EU=E
        ENDIF
       E=(EU+EL)/2.0D0
       ENDDO
        
       WRITE(*,*)'ENERGY EIGENVALUE IS', E

!LOOP FOR PLOT OF PSI VS X        
       DO I=2,M-1,1
        X(I+1)=X(I)+S
        G(I+1)=(X(I+1)*X(I+1))-(2.0D0*E)
        PSI(I+1)=(2.0D0*PSI(I))-(PSI(I-1))+((10.0D0*PSI(I)*G(I)*SS))
        PSI(I+1)=PSI(I+1)+(PSI(I-1)*G(I-1)*SS)
        PSI(I+1)=PSI(I+1)/(1.0D0-(G(I+1)*SS))
        WRITE(12,*)X(I),(PSI(I))
       ENDDO
       END PROGRAM NUMEROV_METHOD
