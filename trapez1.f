      integer n, i
      real h, summ, f, s1, s2, a, b, x, pi
      pi = 4.0 * atan(1.0)
      print *, 'enter the number of divisions N'
      read *, n
      print *, 'enter the lower limit(a) and upper limit(b)'
      read *, a, b
      h = (b-a)/(n-1)
      print *, 'h = ',h
      s1 = exp((-1)*(a**2))
      s2 = exp((-1)*(b**2))
      summ = s1 + s2
      do i = 1, n-1
      f = exp((-1)*(a+i*h)**2)
      summ = summ + (2*f)
      x = a + i*h
      write(20,*) x, f
      end do
      summ = summ*h/2
      summ = summ/sqrt(pi)
      print *, 'the normalisation value is ',summ
      end
