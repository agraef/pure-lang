
! Matrix example (Gaussian elimination). This brings the given mxn matrix A
! into (column) echelon form. The algorithm uses partial pivoting. The
! permutation of the columns is returned in the index array.

! Note that this algorithm is prepared to work on transposed matrices, as Pure
! matrices are stored in row-major order. Hence it computes a column echelon
! form. In Pure land this becomes a row echelon form which is what we want.

subroutine gauss(m, n, A, index)
  implicit none
  integer i, j, k, p, q, n, m
  double precision A(m,n), pivot, x, y
  integer index(n)
  do i = 1, n
     index(i) = i
  end do
  do i = 1, n
     ! partial pivoting
     k = 0; pivot = 0.0
     do j = i, n
        x = A(i, index(j))
        if (abs(x) > abs(pivot)) then
           k = j; pivot = x
        end if
     end do
     x = pivot
     if (abs(x) == 0.0) exit ! zero pivot, bail out
     ! the pivot column
     p = index(k)
     if (i /= k) then
        index(k) = index(i); index(i) = p
     end if
     ! normalize the pivot column
     A(:, p) = A(:, p) / x
     ! subtract multiples of the pivot column from the remaining columns
     do k = i+1, n
        q = index(k); y = A(i, q)
        A(:, q) = A(:, q) - y*A(:, p)
     end do
  end do
end subroutine gauss
