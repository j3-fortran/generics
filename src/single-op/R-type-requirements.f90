module R_type_requirements
  implicit none

  type, abstract :: R
  contains
    procedure(binary_op_interface), deferred :: add
    generic :: operator(+)=>add
  end type

  abstract interface

    module function binary_op_interface(lhs,rhs) result(lhs_op_rhs)
      class(R), intent(in) :: lhs, rhs
      class(R), allocatable :: lhs_op_rhs
    end function

  end interface

end module
