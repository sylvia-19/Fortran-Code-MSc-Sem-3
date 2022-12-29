! program to display all odd fibonacci numbers below 70
PROGRAM ODDFIBOLESSTHAN70
  IMPLICIT NONE
INTEGER :: A,B,C
    A=0
    B=1
    DO WHILE (A<70)
      IF (MOD(A,2) .NE. 0) THEN
        PRINT *, A
      END IF
      C=A+B
      A=B
      B=C
    END DO
END PROGRAM ODDFIBOLESSTHAN70