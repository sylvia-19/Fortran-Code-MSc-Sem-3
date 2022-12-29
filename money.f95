!a program to take input of any amount from the user and display it in different denominations
program money
implicit none
integer n, m2000, m500, m200, m100, m50, m20, m10, m5, remain
read (*,*) n

m2000 = n/2000
write (*,*) m2000
remain = n - (m2000 * 2000)

m500 = remain/500
write (*,*) m500
remain = remain - (m500 * 500)

m200 = remain/200
write (*,*) m200
remain = remain - (m200 * 200)

m100 = remain/100
write (*,*) m100
remain = remain - (m100 * 100)

m50 = remain/50
write (*,*) m50
remain = remain - (m50 * 50)

m20 = remain/20
write (*,*) m20
remain = remain - (m20 * 20)

m10 = remain/10
write (*,*) m10
remain = remain - (m10 * 10)

m5 = remain/5
write (*,*) m5

end program money