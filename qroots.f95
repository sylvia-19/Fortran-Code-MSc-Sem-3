real a,b,c,term,a2,r1,r2 
print *, "enter the coefficients: A, B and C"
read *, a,b,c
term = b*b - 4*a*c
a2 = a*2
if (term .lt. 0.0) then
  print *, 'roots are complex'
  else if (term .gt. 0.0) then
    term = term**0.5
    r1 = (-b+term)/a2
    r2 = (-b-term)/a2
    print *, 'roots are ', r1,' and ',r2
    else
      r1 = -b/a2
      print *, 'the roots are equal at ',r1
end if
end