! A generic function defined using template syntax that has a function fun
! as an type-bound procedure to the type.
! It might be instantiated as
! sum = reduce<atype>(x)
! where x is a rank 1 array of type real.
!
module template_function_reduce
  use, intrinsic:: iso_fortran_env, only: int64
  implicit none

contains

  pure template <T> function reduce(x)

    constraints
      type :: T
      contains
        procedure(func) :: fun ! Type bound procedure
      end type
      abstract interface
        function func(y,z)
          match(T), intent(in) :: y ! matches both type(T) and class(T)
          type(T), intent(in)  :: z
          type(T) :: func
        end function func
      end interface
    end constraints

    type(T), intent(in) :: x(:)
    type(T) :: reduce
    integer(int64) :: i

    if ( size(x, kind=int64) == 0 ) &
      error stop "In REDUCE, X must have at least one element."
    reduce = x(1)
    do i = 2, size(x,kind=int64)
       reduce = reduce % fun(x(i))
    end do

  end function reduce

end module template_function_reduce
