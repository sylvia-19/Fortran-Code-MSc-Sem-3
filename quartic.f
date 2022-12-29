      implicit real*8 (a-h, o-z)
      parameter (num=300)
      dimension h(num,num)
      
      do n=6,30,2
      ! initialisation
      do i = 1, n
      do j = 1, n
      h(i,j) = 0.0
      end do
      end do
      
      do i = 1, n
      h(i,i) = (3*i*i) + (5*i) + 2.5
      do j = 1,i-1
      if (j .eq. (i-2)) then
      r = i
      h(i,j) = 2 * (r-1) * sqrt(r*(r-1))
      end if
      if (j .eq. (i-4))  then
      r = i
      h(i,j) = 0.5 * sqrt(r*(r-1)*(r-2)*(r-3))
      end if
      end do
      end do
      ! to store the lower triangle's value in upper triangle
      do i = 1,n
      do j = i+1,n
      h(i,j) = h(j,i)
      end do
      end do

      call dial(n, h)
      ! displaying the eigenvalues
      print *, 'the eigenvalue matrix for n=',n,' is:'
      write (*,11) (h(i,i),i=1,4)
  11  format (1x, 4f13.6)
      end do
      end