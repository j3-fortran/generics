program main

   call driver_1()
   call driver_2()

contains

   ! driver_1() uses the 1-parameter variant.
   subroutine driver_1()
      use swap_m, only: swap_t

      instantiate swap_t(integer), only: swap
      instantiate swap_t(real), only: swap

      integer :: i, j
      real :: x(4), y(4)

      i = 3
      j = 4
      print *, "i = ", i, ", j = ", j
      call swap(i, j)
      print *, "i = ", i, ", j = ", j

      x = 3.1415 * [(i, i = 1,4)]
      y = 2.7183 * [(i, i = 1,4)]

      print *, "x = ", x, ", y = ", y
      call swap(x, y)
      print *, "x = ", x, ", y = ", y

   end subroutine driver_1

   ! driver_2() uses the 2-parameter variant.
   subroutine driver_2()
      use alt_swap_m, only: swap_t

      instantiate swap_t(integer, 0), only: swap
      instantiate swap_t(real, 1), only: swap

      integer :: i, j
      real :: x(4), y(4)

      i = 3
      j = 4
      print *, "i = ", i, ", j = ", j
      call swap(i, j)
      print *, "i = ", i, ", j = ", j

      x = 3.1415 * [(i, i = 1,4)]
      y = 2.7183 * [(i, i = 1,4)]

      print *, "x = ", x, ", y = ", y
      call swap(x, y)
      print *, "x = ", x, ", y = ", y

   end subroutine driver_2

end program main
