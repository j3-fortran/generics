template foo(T, SUB)
   ! Attempt to provide a templated derived type
   type :: C
      TYPE(T), allocatable :: elements(:)
   CONTAINS
      ! Who's type bound procedures implementation
      ! is provided as a template argument
      procedure :: bar
   end type

   interface
      subroutine sub(x,y)
        type(C), INTENT(INOUT) :: x
        TYPE(T), INTENT(IN) :: y
      end subroutine
   end interface

contains

   subroutine bar(...)
      call sub(...)
   end subroutine

end template

! This statement proves problematic for checking
! the interface of insert, as the precise definition
! of type(C) is not known until instantiation has been completed
instantiate foo(real, insert)

subroutine insert(x, y)
   type(C), intent(inout) :: x
   real, intent(in) :: y

   call c%bar(...)

   ...
end subroutine

! Note that the template itself is valid, but it cannot
! be instantiated, just like the following subroutine is
! valid, but there's no way you'd be able to call it

SUBROUTINE S(x)
   TYPE :: T
   END TYPE
   TYPE(T), INTENT(IN) :: x
END SUBROUTINE