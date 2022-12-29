      real a, b, h, l, delx, delpx, up, pi, summ, summ1, summ2
      real s1, s2, f, x1, x2, xavg2, x2avg
      integer n, i
      summ1 = 0
      summ2 = 0
      a = 0
      pi = 4.0 * atan(1.0)
      print *, 'enter the number of divisions N'
      read *, n
      print *, 'enter the final limit(b)'
      read *, b
      print *, 'enter the length of the box (l)'
      read *, l
      h = (b-a)/(n-1)
      
      ! for calculating x average
      s1 = a * (sin(pi*a/l)**2)
      s2 = b * (sin(pi*b/l)**2)
      summ = s1 + s2
      do i = 1, n-1
      f = (a+i*h) * ((sin(pi*(a+i*h)/l))**2)
      summ = summ + (2*f)
      end do
      xavg2 = (summ) * (h/2) * (2/l)  ! sqrt(2/l) * sqrt(2/l) = 2/l
      xavg2 = xavg2 * xavg2
      
      ! for calculating x^2 average
      s1 = (a*a) * (sin(pi*a/l)**2)
      s2 = (b*b) * (sin(pi*b/l)**2)
      do i = 1, n-1
      f = ((a+i*h)**2) * ((sin(pi*(a+i*h)/l))**2)
      summ1 = summ1 + (2*f)
      end do
      x2avg = (summ1*h)/l ! short form of writing h/2 * 2/l
      
      ! to find delx
      delx = sqrt(x2avg - xavg2)
      print *, 'delx= ',delx
      
      ! we do not have to assign s1 and s2 in case of derivative
      ! because the 1st and last limit is =0 wrt derivative
      
      ! for calculating px^2 average
      do i = 1, n-1
      x1 = a + (i+1)*h
      x2 = a + (i-1)*h
      f = sin(pi*x1/l) - sin(pi*x2/l)
      f = f/(2*h)
      summ2 = summ2 + (2*f*f)
      end do
      delpx = (summ2*h)/l ! h/2 * 2/l
      delpx = sqrt(delpx)
      print *, 'delpx= ',delpx
      
      !to find out the uncertainty product
      up = delx * delpx
      print *, 'the uncertainty product is',up
      end