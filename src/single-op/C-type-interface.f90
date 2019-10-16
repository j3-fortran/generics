module C_type_interface
  use R_type_requirements, only : R
  implicit none

  type C
    class(R), allocatable :: Re, Im
  contains
    procedure :: add
  end type

contains

  module function add(lhs,rhs) result(total)
    class(C), intent(in) :: lhs
    type(C), intent(in) :: rhs
    type(C) :: total
    total%Re = lhs%Re + rhs%Re
    total%Im = lhs%Im + rhs%Im
  end function

end module

