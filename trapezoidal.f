!	PROGRAM TO DEMONSTRATE TRAPEZOIDAL METHOD WITH F(X) = X*X
      implicit real*8 (a-h, o-z)
      pi = 4.0 * atan(1.0)
      print *, 'enter the number of divisions N'
      read *, n
      print *, 'enter the lower limit(a) and upper limit(b)'
      read *, a, b
      r=n
      h = (b-a)/r
      print *, 'h = ',h
      s1 = (a**2)
      s2 = (b**2)
      summ = s1 + s2
      do i = 1, n-1
      f = (a+i*h)**2
      summ = summ + (2*f)
      end do
      summ = summ*h/2
      print *, 'the normalisation value is ',summ
      end
