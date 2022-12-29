      parameter (n=50)
      dimension a(n,n), b(n,n), c(n,n)
      print *, 'enter the dimension of the square matrix'
      read *, l
      print *, 'enter the values in matrix A'
      read *, ((a(i,j), j=1,l), i=1,l)
      print *, 'enter the values in matrix B'
      read *, ((b(i,j), j=1,l), i=1,l)
      ! print *, 'matrix A is:'
      ! write (*,11) ((a(i,j), j = 1,l), i=1,l)
      ! print *, 'matrix B is:'
      ! write (*,11) ((b(i,j), j = 1,l), i=1,l)
      do i = 1, l
      do j = 1, l
      c(i,j) = 0.0
      do k = 1, l
      c(i,j) = c(i,j) + a(i,k)*b(k,j)
      end do
      end do
      end do
      print *, 'the product of AxB is:'
      write (*,11) ((c(i,j), j = 1,l), i=1,l)
  11  format (1x, 3f13.6)
      end
