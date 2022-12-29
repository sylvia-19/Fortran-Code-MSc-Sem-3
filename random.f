      integer i
      seed = 7**5
      a = 2
      b = (2**20) - 1
      do i = 1, 50
      x = mod(seed*a,b)
      random = x/b
      print *, random
      seed = x
      end do
      end
