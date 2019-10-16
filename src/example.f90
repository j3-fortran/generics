module cayley_dickson_construction

  type, abstract :: R
  contains
    procedure(binary_op_interface), deferred :: add, multiply
    procedure(unary_op_interface), deferred :: negate
    procedure(constant_interface), deferred, nopass :: zero, one
    generic :: operator(+)=>add
    generic :: operator(*)=>multiply
    generic :: operator(-)=>negate
  end type

  abstract interface

    module function binary_op_interface(lhs,rhs) result(lhs_op_rhs)
      class(R), intent(in) :: lhs, rhs
      type(R) :: lhs_op_rhs
    end function

    module function unary_op_interface(rhs) result(op_rhs)
      class(R), intent(in) :: rhs
      type(R) :: op_rhs
    end function

    module function constant_interface() result(const)
      type(R) :: const
    end function

  end interface

  type C
    type(R) :: Re, Im
  contains
    procedure :: add, multiply
    procedure :: negate
    procedure :: zero, one, im
  end type

contains

  function add(lhs,rhs) result(total)
    class(C), intent(in) :: lhs, rhs
    type(C)  :: total
    total%Re = lhs%Re + rhs%Re
    total%Im = lhs%Im + rhs%Im
  end function

  function multiply(lhs,rhs) result(product_)
    class(C), intent(in) :: lhs, rhs
    type(C)  :: product_
    total%Re = lhs%Re*rhs%Re - lhs%Im*rhs%Im
    total%Im = lhs%Re*rhs%Im + lhs%Im*rhs%Re
  end function

  function negate(rhs) result(negative)
    class(C), intent(in) :: rhs
    type(C)  :: negative
    negative%Re = -rhs%Re
    negative%Im = -rhs%Im
  end function

  function zero() result(O)
    type(C) :: O
    O%Re = R%zero()
    O%Im = R%zero()
  end function

  function one() result(I)
    type(C) :: I
    I%Re = R%one()
    O%Im = R%zero()
  end function

  function im() result(I)
    type(C) :: I
    I%Re = R%zero()
    O%Im = R%one()
  end function

end module

