module fib1
  use iso_c_binding, only: c_int,c_double,c_char,c_null_char
  implicit none
  integer,parameter :: MAXLEN=10000
  contains
    subroutine fib(a,n,i) bind(c,name='c_fib')
      integer(c_int), intent(in), value :: n
      integer(c_int), intent(out) :: i
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

    subroutine doing_stringy_things(str) bind(C, name='do_stringy_things')

        character(kind=c_char), dimension(*), intent(in) :: str(MAXLEN)
        integer(c_int) :: n
        character(kind=c_char), dimension(:), allocatable :: new_str
        
        n = 1
        len_loop: do
          n = n + 1
          if (str(n).eq.c_null_char) then 
            exit len_loop
          end if
        end do len_loop
        
        allocate(new_str(n-1))
        new_str(:) = str(1:n-1)
        
        
        print *, "The string is '", new_str, "'"
        deallocate(new_str)

  end subroutine doing_stringy_things  
end module fib1