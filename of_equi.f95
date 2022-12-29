program of_equi
implicit none
	integer ::a
    write (*,*) "Enter any number of integer type"
    read (*,*) a
    write (*,*) "A=",a
    if (a>40) then
      write (*,*) "A is greater than 40"
      else
        write (*,*) "A is lesser than 40"
        end if
	write (*,*) "The value of A is",a
end program of_equi