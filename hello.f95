program hello
implicit none
  !integer :: a
  character (len = 20) :: s
  print *, "Enter your name"
  read (*,*) s
  print *, "Hello ", s
  !do a=1,5
  !write(*,'(1x,i0)',advance='no') a
  !the above line is for displaying the output in 1 line
  !end do
end program hello