module unlimited_polymorphic_swap
  implicit none

  type, requirements :: T
  contains
    procedure(assign_interface) ::  assign
    generic :: assignment(=) => assign
  end type

  abstract interface
    subroutine assign(lhs,rhs)
      type(T), intent(inout) :: lhs, rhs
    end subroutine
  end interface

contains

  subroutine swap(a, b)
    type(T), intent(inout) :: a, b
    type(T) tmp
    tmp = a
    a = b
    b = tmp
  end subroutine

end module
