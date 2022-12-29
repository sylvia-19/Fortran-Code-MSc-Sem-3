      implicit real*8 (a-h,o-z)
      parameter (n=50)
      dimension a(n,n), b(n,n), c(n,n)
      open (1, file = 'input.f', status = 'old')
      read (1,*) ((a(i,j), j=1,3), i=1,3)
      read (1,*) ((b(i,j), j=1,3), i=1,3)
      do i = 1,3
      do j = 1,3
      c(i,j) = 0.0
      do k = 1,3
      c(i,j) = c(i,j) + a(i,k)*b(k,j)
      end do
      end do
      end do
      print *, 'the product of the matrices is:'
      write (*,11) ((c(i,j), j=1,3), i=1,3)
  11  format(1x,3f13.6)
      end
