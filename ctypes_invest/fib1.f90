module fib1
  use iso_c_binding
  implicit none
  contains
    subroutine fib(a,n) bind(c,name='c_fib')
      integer(c_int), intent(in), value :: n
      integer(c_int) :: i
      real(c_double) :: a(n)
      do i=1, n
         if (i==1) then
            a(i) = 0.0d0
         else if (i==2) then
            a(i) = 1.0d0
        else
            a(i) = a(i-1) + a(i-2)
         end if
      end do
      end subroutine
end module fib1