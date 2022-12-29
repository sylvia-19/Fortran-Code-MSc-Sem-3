program factorial_sem3
integer n, i, fac
fac = 1
write (*,*) "enter a number to find its factorial"
read (*,*) n
do i=1, n
  fac = fac * i
end do
print *, "the factorial of",n," is: ",fac
end program factorial_sem3