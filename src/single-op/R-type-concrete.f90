module R_type_concrete
  use R_type_requirements, only : R
  implicit none

  private
  public :: R_concrete

  type, extends(R) :: R_concrete
  contains
    procedure :: add
  end type

contains

    function add(lhs,rhs) result(lhs_op_rhs)
      class(R_concrete), intent(in) :: lhs
      class(R), intent(in) :: rhs
      class(R), allocatable :: lhs_op_rhs
    end function

end module
