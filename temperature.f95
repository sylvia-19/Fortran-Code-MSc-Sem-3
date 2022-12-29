!WAP to input a temperature in Celsius and convert it to Fahrenheit
program temperature
implicit none
real :: c,f
write (*,*) "Enter the temperature in Celsius "
read (*,*) c
f=(9*c/5) + 32
write (*,*) "The temperature in Fahrenheit is ", f
end program temperature