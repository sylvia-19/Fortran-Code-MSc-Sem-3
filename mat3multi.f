      parameter (n=50)
      dimension a(n,n), b(n,n), c(n,n), d(n,n), e(n,n)
      integer row, colum, c1, c2
      ! column of A = row of B. So, colum = r1
      print *, 'enter the number of rows for matrix A'
      read *, row
      print *, 'no. of columns for A/no. of rows for B'
      read *, colum
      print *, 'enter the number of columns for matrix B'
      read *, c1
      print *, 'enter the values in matrix A'
      read *, ((a(i,j), j=1,colum), i=1,row)
      print *, 'enter the values in matrix B'
      read *, ((b(i,j), j=1,c1), i=1,colum)
      do i = 1, row
      do j = 1, c1
      c(i,j) = 0.0
      do k = 1, colum
      c(i,j) = c(i,j) + a(i,k)*b(k,j)
      end do
      end do
      end do
      print *, 'the product of the matrices(c) is'
      write(*,*) ((c(i,j), j=1,c1), i=1,row)
      ! two matrices have been multiplied so far
      ! now we will multiply their product with the 3rd matrix
      
      ! column of C = row of D. So, c1 = r2
      print *, 'enter the number of columns for matrix D'
      read *, c2
      print *, 'enter the values in matrix D'
      read *, ((d(i,j), j=1,c2), i=1,c1)
      do i = 1, row
      do j = 1, c2
      e(i,j) = 0.0
      do k = 1, c1
      e(i,j) = e(i,j) + c(i,k)*d(k,j)
      end do
      end do
      end do
      print *, 'the final product of all the matrices is:'
      write (*,*) ((e(i,j), j=1,c2), i=1,row)
      end
