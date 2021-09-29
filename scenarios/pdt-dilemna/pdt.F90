module foo_mod
   use, intrinsic :: iso_fortran_env
   implicit none
   
   type :: foo(k)
      integer, kind :: k
      real(kind=k), allocatable :: data(:)
   contains
      procedure :: s32
      procedure :: s64
   end type foo

contains


   function S32(this)
      class(foo(k=REAL32)), intent(in) :: this
      real(kind=this%k) :: s32
      s32 = sum(this%data)
   end function S32

   function S64(this)
      class(foo(k=REAL64)), intent(in) :: this
      real(kind=this%k) :: s64
      s64 = sum(this%data)
   end function S64

end module foo_mod

program main
   use foo_mod
   use, intrinsic :: iso_fortran_env
   implicit none

   type(foo(REAL32)) :: x32
   type(foo(REAL64)) :: x64

   x32%data = [1,2,3]
   x64%data = sqrt(x32%data)

   print*,x32%s32()
   print*,x64%s64()

   print*,x32%s64() ! illegal
end program main
