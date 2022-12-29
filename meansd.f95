      real mean, ssq, x, w, sd, r
      integer i
      mean = 0.0
      ssq = 0.0
      print *, 'enter the number of readings'
      read *, n
      print *, 'enter the',n,'values, one per line'
      do 1 i = 1, n
        read *, x
        w = x - mean
        r = i - 1
        mean = (r * mean + x)/i
        ssq = ssq + w*w*r/i
  1   continue
      sd = (ssq/r) ** 0.5
      print *, 'mean is ',mean
      print *, 'standard deviation is ',sd
      end