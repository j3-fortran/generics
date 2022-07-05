!-----------------
! The following module defines a template swap_T that is
! parameterized by a type parameter T and an integer rank N.
!
! The template in turn defines 3 procedures
!   - swap for rank N, non-pointer, non-allocatable entities
!   - swap for rank N, pointer entities
!   - swap for rank N, allocatable entities
!-----------------

module alt_swap_m
   implicit none
   private
   public :: swap_t
   
   template swap_t(T, N)
      private

      public :: swap
      public :: swap_dyn
   
      type :: T
      end type T
      integer, parameter :: N
   
      interface swap
         module procedure swap_
      end interface swap

      interface swap_dyn
         module procedure swap_ptr
         module procedure swap_alloc
      end interface swap_dyn

   contains

      ! Note: The following procedure fails if x and y are of
      ! different shape.
      subroutine swap_(x, y)
         type(T), rank(N), intent(inout) :: x, y
         type(T), rank(N), allocatable :: tmp
         
         tmp = x
         x = y
         y = tmp

      end subroutine swap_

      subroutine swap_ptr(x, y)
         type(T), rank(N), pointer, intent(inout) :: x, y
         type(T), rank(N), pointer :: tmp
         
         tmp => x
         x => y
         y => tmp

      end subroutine swap_ptr

      subroutine swap_alloc(x, y)
         type(T), rank(N), allocatable, intent(inout) :: x, y
         type(T), rank(N), allocatable :: tmp

         call move_alloc(from=x, to=tmp)
         call move_alloc(from=y, to= x)
         call move_alloc(from=tmp, to=y)

      end subroutine swap_alloc

   end template swap_t

end module alt_swap_m
