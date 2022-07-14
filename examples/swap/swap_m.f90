!-----------------
! The following module defines a template swap_T that is
! parameterized by a single type parameter T.
! The template in turn defines 3 procedures
!   - swap for scalar, non-pointer, non-allocatable entities
!   - swap for scalar rank, pointer entities
!   - swap for scalar, allocatable entities
!-----------------

module swap_m
   implicit none
   private
   public :: swap_t
   
   template swap_t(T)
      private

      public :: swap
      public :: swap_dyn
   
      type :: T
      end type T
   
      interface swap
         module procedure swap_
      end interface swap

      interface swap_dyn
         module procedure swap_ptr
         module procedure swap_alloc
      end interface swap_dyn

   contains

      ! Note: The following procedure fails if x and y are of
      ! different rank and/or shape.
      subroutine swap_(x, y)
         type(T), intent(inout) :: x
         type(T) :: tmp

         tmp = x
         x = y
         y = tmp

      end subroutine swap_

      subroutine swap_ptr(x, y)
         type(T), pointer, intent(inout) :: x, y
         type(T), pointer :: tmp
         
         tmp => x
         x => y
         y => tmp

      end subroutine swap_ptr

      subroutine swap_alloc(x, y)
         type(T), allocatable, intent(inout) :: x, y
         type(T), allocatable :: tmp

         call move_alloc(from=x, to=tmp)
         call move_alloc(from=y, to= x)
         call move_alloc(from=tmp, to=y)

      end subroutine swap_alloc

   end template swap_t

end module swap_m
