!WAP that accepts a, b and c from the keyboard and finds the roots of the quadratic equation: ax2 + bx + c = 0
program quadratic
implicit none
	real :: a,b,c,x1,x2,d
	print *, "Enter the values "
	read (*,*) a,b,c
    if (a==0) then
      print *, "SORRY. 'a' CANNOT BE ZERO"
      else
	d = (b*b)-(4*a*c)
	if (d>=0) then
    	x1 = (-b + sqrt(d))/(2*a)
        x2 = (-b - sqrt(d))/(2*a)
        print *, "The roots of the equation are ",x1, " and ",x2
        else
          print *, "COMPLEX ROOT FOUND. SORRY."
        end if
        end if
end program quadratic