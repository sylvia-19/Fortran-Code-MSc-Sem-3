      ! program to diagonalise a 3x3 matrix
      implicit real*4 (a-h,o-z)
      parameter (l=50)
      dimension a(l,l), u(l,l), ut(l,l), u1(l,l), u2(l,l)
      print *, 'enter the dimension of matrix A'
      read *, n
      print *, 'enter the values in matrix A'
      read *, ((a(i,j), j=1,n), i=1,n)
      
      print *, 'matrix A is:'
      do i=1,n
      write (*,*) (a(i,j), j=1,n)
      end do
      !****************************************************************
      do m=1,n
      ! ********** to find the highest off-diagonal element **********
      temp = a(1,2)
      iloc = 1
      jloc = 2
      do i=1,n
      do j=1,n
      if (j .ne. i) then
      if (a(i,j) .gt. temp) then
      alarge = a(i,j)
      iloc = i
      jloc = j
      end if
      end if
      end do
      end do
      ! print *, 'the highest off-diagonal element is:',alarge
      ! print *, 'its location is:',iloc,',',jloc


	! ******************** to make alarge zero ********************
      
	!********** setting up the U matrix **********
	theta = 0.5*atan((2*a(iloc,jloc))/(a(iloc,iloc)-a(jloc,jloc)))
	!**************************************************************
	do i=1,n
	do j=1,n
	u(iloc,iloc) = cos(theta)
	u(jloc,iloc) = sin(theta)
	u(iloc,jloc) = -sin(theta)
	u(jloc,jloc) = cos(theta)
	if ((i .eq. iloc) .or. (j .eq. jloc)) then
	continue
	else
	if (i .eq. j) then
	u(i,i) = 1
	else
	u(i,j) = 0
	end if
	end if
	end do
	end do
	
	!print *, 'U matrix is:'
	!do i=1,n
	!write (*,*) (u(i,j), j=1,n)
	!end do
	!**************************************************************
	!********** transposing U and storing it in UT **********
	do i=1,n
	do j=1,n
	ut(j,i) = u(i,j)
	end do
	end do
	
	!print *, 'the transposed matrix UT is:'
	!do i=1,n
	!write (*,*) (ut(i,j), j=1,n)
	!end do
	
	!********** multiply:AxU=U1 **********
	do i = 1, n
	do j = 1, n
	u1(i,j) = 0.0
	do k = 1, n
	u1(i,j) = u1(i,j) + a(i,k)*u(k,j)
	end do
      	end do
      	end do
      	
      	!print *, 'the product of AxU is:'
      	!do i=1,n
	!write (*,*) (u1(i,j), j=1,n)
	!end do
      	!********** multiply:UTxU1=U2 **********
      	do i = 1, n
	do j = 1, n
	u2(i,j) = 0.0
	do k = 1, n
	u2(i,j) = u2(i,j) + ut(i,k)*u1(k,j)
	end do
      	end do
      	end do
      	
      	print *, 'the diagonalised matrix(after performing: U"AU) is:'
      	do i=1,n
	write (*,*) (u2(i,j), j=1,n)
	end do
	! **************** storing U2 in A ****************
	do i=1,n
	do j=1,n
	a(i,j) = u2(i,j)
	end do
	end do
	
	end do  ! end of outer 'm' loop
	end