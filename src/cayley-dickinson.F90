module R_type_requirements
  implicit none

#define requirements abstract

  type, requirements :: R !! requirements for the generic type
  contains
    procedure(binary_op_interface), deferred :: add, multiply
    procedure(unary_op_interface), deferred :: negate
    generic :: operator(+)=>add
    generic :: operator(*)=>multiply
    generic :: operator(-)=>negate
  end type

  type, extends(R), requirements :: R_conjugable
  contains
    procedure(unary_op_interface_conjugable), deferred :: conjg
    generic :: operator(.conjg.)=>conjg
  end type

  abstract interface

    module function binary_op_interface(lhs,rhs) result(lhs_op_rhs)
      class(R), intent(in) :: lhs, rhs
      class(R), allocatable :: lhs_op_rhs
    end function

    module function unary_op_interface(rhs) result(op_rhs)
      class(R), intent(in) :: rhs
      class(R), allocatable :: op_rhs
    end function

    module function unary_op_interface_conjugable(rhs) result(op_rhs)
      class(R_conjugable), intent(in) :: rhs
      class(R_conjugable), allocatable :: op_rhs
    end function

  end interface

  type C
    class(R_conjugable), allocatable :: Re, Im
  contains
    procedure :: add, multiply, conjg
    generic :: operator(.conjg.)=>conjg
  end type

contains

  module function add(lhs,rhs) result(total)
    class(C), intent(in) :: lhs
    type(C), intent(in) :: rhs
    type(C) :: total
    total%Re = lhs%Re + rhs%Re
    total%Im = lhs%Im + rhs%Im
  end function

  module function multiply(lhs,rhs) result(product_)
    class(C), intent(in) :: lhs
    type(C), intent(in) :: rhs
    type(C) :: product_
    product_%Re = lhs%Re * rhs%Re + (- .conjg.(rhs%Im) * lhs%Im)
    product_%Im = rhs%Re * lhs%Im + lhs%Re * (.conjg. rhs%Im)
  end function

  module function negate(rhs) result(negative)
    class(C), intent(in) :: rhs
    type(C) :: negative
    negative%Re = -rhs%Re
    negative%Im = -rhs%Im
  end function

  module function conjg(rhs) result(conjg_rhs)
    class(C), intent(in) :: rhs
    type(C) :: conjg_rhs
    conjg_rhs%Re = .conjg.(rhs%Re)
   !conjg_rhs%Im = .conjg.(rhs%Re+rhs%Re) ! fails
    conjg_rhs%Im = -rhs%Im
  end function

end module
