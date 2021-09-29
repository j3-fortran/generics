module foo_mod
   use, intrinsic :: iso_fortran_env
   implicit none
   
   type :: foo<k>
      integer :: k ! not a component!
      
      real(kind=k), allocatable :: data(:)
   contains
      procedure :: s
   end type foo

contains


   function s<k>(this)
      integer :: k
      class(foo<k>), intent(in) :: this
      real(kind=k) :: s
      s = sum(this%data)
   end function s

end module foo_mod

program main
   use foo_mod
   use, intrinsic :: iso_fortran_env
   implicit none

   type(foo<real(kind=REAL32)>) :: x32
   type(foo<real(kind=REAL64>)) :: x64

   x32%data = [1,2,3]
   x64%data = sqrt(x32%data)

   print*,x32%s()
   print*,x64%s()

end program main
