
! A simple scalar function to be called from Pure (the factorial).

function fact(n) result(p)
  integer n, p
  p = 1
  do i = 1, n
     p = p*i
  end do
end function fact
