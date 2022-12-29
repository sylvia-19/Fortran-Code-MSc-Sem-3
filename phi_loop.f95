real x, phi
do x=-3.0, 3.0, 0.5
  phi = (1.0/sqrt(2*3.1416))*(exp(-x*x/2.0))
  write (*,*) 'phi= ',phi,' for x= ',x
end do
end