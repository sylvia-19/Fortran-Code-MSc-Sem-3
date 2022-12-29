!implement Heron's formula
program triangle
implicit none
real :: a,b,c,s,area
write (*,*) "Enter the three sides of a triangle"
read (*,*) a,b,c
s = (a+b+c)/2
write (*,*) "The semi-perimeter is ", s
if (((a+b)>c) .and. ((b+c)>a) .and. ((c+a)>b) .and. (a>0) .and. (b>0) .and. (c>0)) then 
area = sqrt(s*(s-a)*(s-b)*(s-c))
write (*,*) "The area of the triangle is ", area
else
write (*,*) "It is not a triangle!"
end if
end program triangle