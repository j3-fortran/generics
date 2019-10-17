module R_type_requirements
  implicit none

#define requirements abstract

  type, requirements :: R !! requirements for the generic type
  contains
    procedure(binary_op_interface), deferred :: add, multiply
    procedure(unary_op_interface), deferred :: negate
    procedure(constant_interface), deferred, nopass :: zero, one
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

    module function constant_interface() result(const)
      class(R), allocatable :: const
    end function

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
    procedure :: add, multiply_CC, conjg, norm
    procedure, pass(rhs) ::  multiply_BC
    generic :: operator(.conjg.)=>conjg
    generic :: operator(.norm.)=>norm
    generic :: operator(*)=>multiply_CC, multiply_BC
  end type

contains


  module function one() result(I)
    type(C) I
    I%Re = I%Re%one()
    I%Im = I%Re%zero()
  end function

  module function zero() result(O)
    type(C) O
    O%Re = O%Re%zero()
    O%Im = O%Im%zero()
  end function

  module function im() result(i)
    type(C) i
    i%Re = i%Re%zero()
    i%Im = i%Im%one()
  end function

  module function injection( operand ) result(injected)
    type(R), intent(in) :: operand
    type(C) injected
    injected%Re = operand
    injected%Im = operand%zero()
  end function

  module function multiply_BC(lhs,rhs) result(product_)
    type(B), intent(in) :: lhs
    class(C), intent(in) :: rhs
    type(C) product_
    product_ = injection_deep_left(lhs)*rhs
  end function

  module function norm(rhs) result(norm_rhs)
    class(C), intent(in) :: rhs
    type(B) norm_rhs
    norm_rhs = projection_deep_left( rhs*(.conjg.rhs) )
  end function

  module function add(lhs,rhs) result(total)
    class(C), intent(in) :: lhs
    type(C), intent(in) :: rhs
    type(C) total
    total%Re = lhs%Re + rhs%Re
    total%Im = lhs%Im + rhs%Im
  end function

  module function multiply_CC(lhs,rhs) result(product_)
    class(C), intent(in) :: lhs
    type(C), intent(in) :: rhs
    type(C) product_
    product_%Re = lhs%Re * rhs%Re + (- .conjg.(rhs%Im) * lhs%Im)
    product_%Im = rhs%Re * lhs%Im + lhs%Re * (.conjg. rhs%Im)
  end function

  module function negate(rhs) result(negative)
    class(C), intent(in) :: rhs
    type(C) negative
    negative%Re = -rhs%Re
    negative%Im = -rhs%Im
  end function

  module function conjg(rhs) result(conjg_rhs)
    class(C), intent(in) :: rhs
    type(C) conjg_rhs
    conjg_rhs%Re = .conjg.(rhs%Re)
   !conjg_rhs%Im = .conjg.(rhs%Re+rhs%Re) ! fails
    conjg_rhs%Im = -rhs%Im
  end function

end module
