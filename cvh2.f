      implicit real*8 (a-h, o-z)
      parameter (ka = 500)
      dimension t(ka), qp(ka), qo(ka), fnp(ka), fno(ka)
      dimension cvp(ka), cvo(ka), cv(ka)
      print *, 'enter the following values:'
      print *, 't-initial, t-final, theta (rotation)'
      print *, 'ntemp (no. of divisions), n-even, n-odd'
      print *, 'and universal gas constant R'
      read *, tin, tf, throt, ntemp, neven, nodd, r
      th = (tf-tin)/float(ntemp-1)

      do i = 1, ntemp
      t(i) = tin + ((i-1)*h)
      end do
      
      do i = 1, ntemp
      sump = 0.0
      sumo = 0.0
      do j = 0, neven, 2
      term1 = ((2*j) + 1)
      term2 = ((-j*(j+1)*throt)/t(i))
      term2 = exp(term2)
      term = term1 * term2
      sump = sump + term
      end do
      do j = 1, nodd, 2
      term1 = ((2*j) + 1)
      term2 = ((-j*(j+1)*throt)/t(i))
      term2 = exp(term2)
      term = term1 * term2
      sumo = sumo + term
      end do
      qp(i) = sump
      qo(i) = sumo
      fnp(i) = t(i) * log(qp(i))
      fno(i) = t(i) * log(qo(i))
      end do
      
      do i = 2, ntemp-1
      cvp(i) = fnp(i-1) + fnp(i+1) - (2*fnp(i))
      cvp(i) = cvp(i)/(th*th)
      cvp(i) = r * t(i) * cvp(i)
      cvo(i) = fno(i-1) + fno(i+1) - (2*fno(i))
      cvo(i) = cvo(i)/(th*th)
      cvo(i) = r * t(i) * cvo(i)
      cv(i) = (0.25*cvp(i)) + (0.75*cvo(i))
      write (32,*) t(i), cvp(i), cvo(i), cv(i)
      end do
      stop
      end