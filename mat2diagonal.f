	implicit real*8 (a-h, o-z)
	parameter (l=50)
	dimension a(l,l), u(l,l), ut(l,l), u1(l,l), u2(l,l)
	n=2
	print *, 'enter the values in 2x2 matrix: A'
	read *, ((a(i,j), j=1,n), i=1,n)
	theta = 0.5 * atan((2*a(1,2))/(a(2,2)-a(1,1)))
	!********** setting up the U matrix **********
	u(1,1) = cos(theta)
	u(1,2) = sin(theta)
	u(2,1) = -sin(theta)
	u(2,2) = cos(theta)
	!********** transposing U and storing it in UT **********
	do i=1,n
	do j=1,n
	ut(j,i) = u(i,j)
	end do
	end do
	print *, 'the transposed matrix UT is:'
	write (*,11) ((ut(i,j), j=1,n), i=1,n)
	!********** multiply:AxU=U1 **********
	do i = 1, n
	do j = 1, n
	u1(i,j) = 0.0
	do k = 1, n
	u1(i,j) = u1(i,j) + a(i,k)*u(k,j)
	end do
      	end do
      	end do
      	print *, 'the product of AxU is:'
      	write (*,11) ((u1(i,j), j = 1,n), i=1,n)
      	!********** multiply:UTxU1=U2 **********
      	do i = 1, n
	do j = 1, n
	u2(i,j) = 0.0
	do k = 1, n
	u2(i,j) = u2(i,j) + ut(i,k)*u1(k,j)
	end do
      	end do
      	end do
      	print *, 'the diagonalised matrix is:'
      	write (*,11) ((u2(i,j), j = 1,n), i=1,n)
   11   format (1x,2f13.6)
	end