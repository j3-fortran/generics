! A generic module defined using "function" syntax that has a function fun
! as an argument to the function reduce.
! It might be instantiated as
! use real_module_reduce => generic_module_reduce(real): real_reduce => reduce
! or
! new real_module_reduce => generic_module_reduce(real): real_reduce => reduce
! and the function real_reduce invoked as
! sum = real_reduce(x, operator(+))
!
module generic_module_reduce(T)
  use, intrinsic:: iso_fortran_env, only: int64
  implicit none

  requirement
    type :: T ! May need syntax to indicate assignment is defined
    end type
  end requirement

contains

  pure function reduce(x, fun)
    type(T) :: reduce
    type(T), intent(in) :: x(:)
    procedure(func) :: fun
    abstract interface
      function func(y,z)
        nature(T), intent(in) :: y ! matches both type(T) and class(T)
        type(T), intent(in)   :: z
        type(T) :: func
      end function func
    end interface
    integer(int64) :: i

    if ( size(x, kind=int64) == 0 ) &
      error stop "In REDUCE, X must have at least one element."
    reduce = x(1)
    do i = 2, size(x,kind=int64)
       reduce = fun(reduce, x(i))
    end do

  end function reduce

end module generic_module_reduce
