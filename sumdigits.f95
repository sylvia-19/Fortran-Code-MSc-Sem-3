!WAP to calculate the sum of the digits of a 5-digit number
integer n,i,q,r,s
s=0
print *, 'enter a 5-digit number'
read *, n
do i=1,5
  q = n/10
  r = mod(n,10)
  s = s + r 
  n = q
end do
print *,'the sum of the digits is: ',s
end  