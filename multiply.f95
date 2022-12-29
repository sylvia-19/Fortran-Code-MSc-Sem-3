! WAP to input 2 numbers and display their product
program multiply
implicit none
real :: a,b,p
write (*,*) "Enter two numbers of real type "
read (*,*) a,b
p = a*b
write (*,*) "The product is ", p
end program multiply