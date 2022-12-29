      program kinetics2
      real at, t, bt, ct, k1, k2
      integer n
      a0 = 1.0
      b0 = 0.0
      ti = 0.0
      t = 0.0
      az = a0 ! to store the value of a0
      print *, 'enter the number of points N'
      read *, n
      print *, 'enter the final value of time'
      read *, tf
      print *, 'enter the values of rate constants k1 and k2'
      read *, k1, k2
      h = (tf-ti)/(n-1)
      open (10, file = 'kin2_out.dat', status = 'unknown')
      do i = 1, n
      at = a0 - (h*k1*a0)
      bt = (h*k1*a0) + (b0-(h*k2*b0))
      ct = az - (at+bt)
      t = t + h
      a0 = at
      b0 = bt
      write (10,*) t, at, bt, ct
      end do
      call system ('gnuplot -p kin2.plt')
      end program kinetics2