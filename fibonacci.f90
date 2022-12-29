PROGRAM FIBONACCI
    IMPLICIT NONE
    INTEGER :: N,A,B,C,I 
    A=0; B=1
    WRITE (*,*) 'ENTER THE NUMBER OF TERMS THAT YOU WANT TO BE DISPLAYED'
    READ *, N
    WRITE (*,*) 'THE FIBONACCI NUMBERS UP TO',N,'ARE:' 
    DO I = 1,N 
        WRITE (*,*) A 
        C = A + B 
        A = B 
        B = C 
    END DO
END PROGRAM FIBONACCI