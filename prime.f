	implicit none
	integer n, i, r, s
	r=1
	print *, 'enter a positive integer (0 is not acceptable)'
	read *, n
	s = sqrt(real (n))
	if ((n .eq. 0) .or. (n .eq. 1)) then
	print *,n,'is neither a prime nor a composite number'
	else
	do i=2, s
	r = mod(n,i)
	if (r .eq. 0) then
	exit
	end if
	end do
	if (r .eq. 0) then
	print *,n,'is not a prime number'
	else
	print *,n,'is a prime number'
	end if
	end if
	end