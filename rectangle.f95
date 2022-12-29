!WAP to calculate the perimeter, area and diagonal of a rectangle whose two adjacent sides are a and b
program rectangle
	implicit none
    	real :: a,b,perimeter,area,diagonal
        print *, "Enter the two sides of a rectangle "
        read (*,*) a,b
        perimeter = 2*(a+b)
        area = a*b
        diagonal = sqrt((a*a)+(b*b))
        print *, "The perimeter of the rectangle is ",perimeter
        print *, "The area of the rectangle is ",area
        print *, "The diagonal of the rectangle is ",diagonal
end program rectangle