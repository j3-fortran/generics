! A generic module defined using "template" syntax that has a function fun
! as an parameter to the template in the hope that that will result in inlining.
! It might be instantiated as
! use real_module_reduce => generic_module_reduce<real, "+">: &
!     real_reduce => reduce
! or
! new real_module_reduce => generic_module_reduce<real, operator(+)>: &
!     real_reduce => reduce
! and the function real_reduce invoked as
! sum = real_reduce(x)

template <T, fun> module template_module_reduce
  use, intrinsic:: iso_fortran_env, only: int64
  implicit none

  requires
    type :: T ! May need syntax to indicate assignment is defined
    end type
    procedure(func) :: fun
    abstract interface
      function func(y,z)
        match(T), intent(in) :: y ! matches both type(T) and class(T)
        type(T), intent(in) :: z
        type(T) :: func
      end function func
    end interface
  end requires

contains

  pure function reduce(x)
    type(T) :: reduce
    type(T), intent(in) :: x(:)
    integer(int64) :: i

    if ( size(x, kind=int64) == 0 ) &
      error stop "In REDUCE, X must have at least one element."

    reduce = x(1)
    do i = 2, size(x,kind=int64)
       reduce = fun(reduce, x(i))
    end do

  end function reduce

end module template_module_reduce
