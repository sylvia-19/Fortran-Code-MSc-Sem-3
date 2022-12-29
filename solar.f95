program solar
implicit none
real :: ltyr, ltmin, dist, elapse, second
integer :: m ,s, minute
ltyr = 9.46 * (10.0**12)
ltmin = ltyr/(365.25*24.0*60.0)
dist = 150.0 * (10.0**6)
elapse = dist/ltmin
minute = elapse
second = (elapse - minute)*60.0
m=minute
s=second
print *, 'Light takes',m,'minutes',s,'seconds to reach the Earth from the Sun.'
end program solar