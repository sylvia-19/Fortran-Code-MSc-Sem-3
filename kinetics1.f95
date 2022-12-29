      program kinetics1
      real at, t
      integer n
      print *, 'enter the number of points N'
      read *, n
      print *, 'enter the final value of time'
      read *, tf
      a0 = 1.0
      k1 = 1.0
      ti = 0.0
      h = (tf-ti)/(n-1)
      print *, 'h = ',h
      t = 0.0
      open (40, file = 'kin_out.dat', status = 'unknown')
      do i = 1, n
      at = a0 + h*(-1)*k1*a0
      a0 = at
      t = t + h
      write (40,*) t, at
      end do
      call system ('gnuplot -p kin1.plt')
      end program kinetics1