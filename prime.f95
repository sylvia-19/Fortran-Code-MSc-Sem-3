!WAP to enter a number and check if it is prime or not
	integer n,i,r
    r = 1
    print *, "Enter a positive integer (0 is not included)"
    read (*,*) n
    if (n .eq. 1) then
      print *, n,' is neither a prime nor a composite number'
      else
    do i=2,n/2
      r = mod(n,i)
      if(r .eq. 0) then
        exit
        end if
    end do
    if (r .eq. 0) then
      print *, n,' is not a prime number'
      else
        print *, n, ' is a prime number'
     end if
   end if 
end