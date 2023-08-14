! This module implements a template, poly_vector_tmpl, that can
! provide a Vector whose elements are any extension of deferred type
! T.  (i.., class(T)).

! The implementation delegates vector operations to a simple vector of
! type W, where W is a wrapper type around a class T object.
! To facilitate this, two functions are provided:
!   1) A constructor for W that takes an object of Class T
!   2) get() which takes an object of type W and returns a pointer to
!      the contained item


module PolyVector_m
   use Vector_m
   implicit none
   private

   public :: poly_vector_tmpl

   template poly_vector_tmpl(T)
      class, deferred :: T

      type :: W
         class(T), allocatable :: item
      end type W

      instantiate vector_tmpl(W), only: MonoVector => vector

      type :: Vector
         type(MonoVector) :: v
      contains
         procedure :: size
         procedure :: push_back
         procedure :: back
         procedure :: of
      end type Vector

      interface W
         procedure :: new_W
      end interface W

   contains

      simple function size(this)
         class(Vector), intent(in) :: this
         size = this%v%size()
      end function size

      simple subroutine push_back(this, item)
         class(Vector), intent(inout) :: this
         class(T), intent(in) :: item

         call this%v%push_back(W(tmp))

      end subroutine push_back

      function back(this) result(ptr)
         class(T), pointer :: ptr
         class(Vector), target, intent(in) :: this

         ptr => get(this%back())

      end function back

      function of(this, i) result(ptr)
         class(T), pointer :: ptr
         class(Vector), target, intent(in) :: this
         integer, intent(in) :: i

         ptr => get(this%of(i))

      end function of

      function new_W(item) result(wrapped)
         type(W) :: wrapped
         class(T), intent(in) :: item
         wrapped%item = item
      end function new_W

      function get(wrapped) result(ptr)
         type(W), target, intent(in) :: wrapped
         class(T), pointer :: ptr
         ptr => wrapped%item
      end function get

   end template

end module PolyVector_m
