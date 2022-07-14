program main

   call driver_A()
   call driver_B()

contains

   ! driver_A() uses the 1-parameter variant that is limited to
   ! scalars.
   subroutine driver_A()
      use swap_m, only: swap_t

      instantiate swap_t(integer), only: swap
      instantiate swap_t(real), only: swap

      integer :: i, j
      real :: x, y

      i = 3
      j = 4
      print *, "i = ", i, ", j = ", j
      call swap(i, j)
      print *, "i = ", i, ", j = ", j

      x = 3.1415
      y = 2.7183

      print *, "x = ", x, ", y = ", y
      call swap(x, y)
      print *, "x = ", x, ", y = ", y

   end subroutine driver_A

   ! driver_B() uses the 2-parameter variant that supports arrays.
   subroutine driver_B()
      use array_swap_m, only: swap_t

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

   end subroutine driver_B

end program main
