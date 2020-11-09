! A generic function defined using function syntax that has a function fun
! as an parameter to the generic reduce.
! It might be instantiated as
! sum = reduce(real,operator(+))(x)
! where x is a rank 1 array of type real.
!
module generic_function_reduce
  use, intrinsic:: iso_fortran_env, only: int64
  implicit none

contains

  pure generic function reduce(T, fun)(x)

    constraints
      type :: T
      end type
      procedure(func) :: fun
      abstract interface
        function func(y,z)
          match(T), intent(in) :: y ! matches both type(T) and class(T)
          type(T), intent(in) :: z
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
       reduce = fun(reduce, x(i))
    end do

  end function reduce

end module generic_function_reduce
