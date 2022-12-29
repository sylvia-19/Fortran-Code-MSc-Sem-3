! 	program to find out the wavefunction for a harmonic oscillator
!	using Numerov's method

!	we have considered h(Planck's const)=1, k=1, m=1
	implicit none
	integer l
	parameter (l=400)
	real v(l), g(l), psi(l), s, e, xmin, xmax, h, x(l), ss
	integer i, n
	print *, 'enter the lower limit(xmin) and upper limit(xmax)'
	read *, xmin, xmax
	print *, 'enter the number of divisions(n)'
	read *, n
	h=n
	s = (xmax-xmin)/h
	ss = s*s
	print *, 'the increment(s) is=',s
	print *, 'enter the value of energy(E)'
	read *, e
	psi(2) = 0.0001
	psi(1) = 0.0d0
!	saving the values of X in an array
	do i=1,n
	x(i) = xmin + (i-1)*s
	end do
!	assigning the first 2 values to V, G and PSI
	v(1) = 0.5 * xmin*xmin
	v(2) = 0.5 * ((xmin+s)**2)
	g(1) = 2.0 * (v(1)-e)
	g(2) = 2.0 * (v(2)-e)
	print *, 'v(1)=',v(1),'v(2)=',v(2),'g(1)=',g(1),'g(2)=',g(2)
	print *, 'psi(1)=',psi(1),'psi(2)=',psi(2)
!	storing the other values of V and G in the array
	do i=2,n
	v(i) = 0.5 * ((xmin+(i-1)*s)**2)
	g(i) = 2.0 * (v(i)-e)
	end do
!	storing the first 2 values in a file
	write (35,*) x(1), psi(1), v(1)
	write (35,*) x(2), psi(2), v(2)
!	calculation to find out PSI
	do i=2,n
	psi(i+1)=(2.0*psi(i)) - psi(i-1) + ((5.0/6.0)*g(i)*psi(i)*ss)
	psi(i+1) = psi(i+1) + (ss/12.0)*g(i-1)*psi(i-1)
	psi(i+1) = psi(i+1) / (1-((ss/12.0)*g(i+1)))
	write (35,*) x(i), psi(i), v(i)
	end do
	end
