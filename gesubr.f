      implicit real *8 (a-h, o-z)
      real xx
      parameter (num=50)
      dimension a(num,num), x(num), y(num)
      n = 3
      print *, 'enter the values in matrix A'
      read *, ((a(i,j), j=1,n), i=1,n)
      print *, 'enter the values in product matrix Y'
      read *, (y(i), i=1,n)
      
      print *, 'the original matrix A is:'
      write(*,11) ((a(i,j), j=1,n), i=1,n)
      print *, 'the original matrix Y is:'
      write(*,12) (y(i), i=1,n)
      
      xx = a(1,1)
      iloc = 1
      yy = a(2,2)
      jloc = 2
      ! to find out the pivot element of 1st column
      do i = 2, n
      if (xx .lt. a(i,1)) then
      xx = a(i,1)
      iloc = i
      end if
      end do
      blarge = xx
      print *,'the pivot element is= ',blarge,' with location=',iloc
      ! swapping the pivot element's row with the 1st row
      do j = 1, n
      temp = a(1,j)
      a(1,j) = a(iloc,j)
      a(iloc,j) = temp
      end do
      temp = y(1)
      y(1) = y(iloc)
      y(iloc) = temp
      print *, 'the swapped matrix is:'
      write(*,11) ((a(i,j), j=1,n), i=1,n)
      ! matrix after 1st operation
      do 10 i=2, n
      fact = a(i,1)/a(1,1)
      do 20 j=1, n
      a(i,j) = a(i,j) - fact*a(1,j)
  20  continue
      y(i) = y(i) - fact*y(1)
  10  continue
      print *, 'matrix after 1st operation:'
      write (*,11) ((a(i,j), j=1,n), i=1,n)
      ! to find the pivot element in the 2nd column excluding 1st row
      do i = 2, n
      if (yy .lt. a(i,2)) then
      yy = a(i,2)
      jloc = i
      end if
      end do
      alarge = yy
      print *,'pivot element of 2nd column= ',yy,' with location=',jloc
      ! swapping the 2nd pivot element's row with the other row
      do j = 2, n
      temp = a(2,j)
      a(2,j) = a(jloc,j)
      a(jloc,j) = temp
      end do
      temp = y(2)
      y(2) = y(jloc)
      y(jloc) = temp 
      print *, 'the 2nd swapped matrix is:'
      write(*,11) ((a(i,j), j=1,n), i=1,n)
      ! matrix after 2nd operation
      do 30 i = 3, n
      fact = a(i,2)/a(2,2)
      do 40 j = 1, n
      a(i,j) = a(i,j) - fact*a(2,j)
  40  continue
      y(i) = y(i) - fact*y(2)
  30  continue
      print *, 'matrix after 2nd operation:'
      write (*,11) ((a(i,j), j=1,n), i=1,n)
      print *, 'new Y matrix is:'
      write (*,12) (y(i), i=1, n)
      ! finding x1,x2,x3 using subroutine
      call back (a, y, x, n)
      print *, 'the resultant X matrix is:'
      write (*,12) (x(i), i=1, n)
  11  format (1x, 3f13.6)
  12  format (1x, 1f13.6)
      stop
      end
      
      subroutine back (a, y, x, n)
      implicit real*8 (a-h, o-z)
      parameter (num=50)
      dimension a(num,num), y(num), x(num)
      x(n) = y(n)/a(n,n)
      do i = n-1, 1, -1
      summ = 0.0
      do j = i+1, n
      term = a(i,j)*x(j)
      summ = summ + term
      end do
      x(i) = (y(i)-summ)/a(i,i)
      end do
      return
      end