! A generic function defined using type inference that has a function fun
! as an argument to the function reduce.
! It might be instantiated as
! sum = reduce(x, operator(+))
! where x is a rank 1 array of type real.
!
module inferred_type_reduce
  use, intrinsic:: iso_fortran_env, only: int64
  implicit none

contains

  pure generic function reduce(x, fun)

    requires
      type, assumed :: T
      end type
    end requires

    type(T), intent(in) :: x(:)
    procedure(func) :: fun
    abstract interface
      function func(y,z)
        nature(T), intent(in) :: y ! matches both type(T) and class(T)
        type(T), intent(in)   :: z
        type(T) :: func
      end function func
    end interface
    type(T) :: reduce
    integer(int64) :: i

    if ( size(x, kind=int64) == 0 ) &
      error stop "In REDUCE, X must have at least one element."
    reduce = x(1)
    do i = 2, size(x,kind=int64)
       reduce = fun(reduce, x(i))
    end do

  end function reduce

end module inferred_type_reduce
