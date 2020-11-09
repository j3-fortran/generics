! A generic module defined using "function" syntax that has T with an operator +
! with the signature (T,T)=>T implied by a type attribute, group. Defining
! the presence of an operator with an attribute really only works for the
! relational and numeric operators.
! It might be instantiated as
! use real_module_reduce => generic_module_reduce(real): real_reduce => reduce
! or
! new real_module_reduce => generic_module_reduce(real): real_reduce => reduce
! and the function real_reduce invoked as
! sum = real_reduce(x)
!
! Note possible type operator attributes are:
! minimal: type declarations and argument passing are defined
! limited: minimal with assignment also defined
! equality: limited with == and /= also defined for the type
! comparable: equality with <, >, <=, >= also defined for the type
! group: limited with + and - also defined for the type
! ring: group with * also defined for the type
! field: ring with / also defined for the type
! numeric: field with ** also defined for the type
!
module attribute_interface_reduce(T)
  use, intrinsic:: iso_fortran_env, only: int64
  implicit none

  constraints
    type, group :: T ! Group => T has + and - ((T,T)=>T) operators defined
    end type
  end constraints

contains

  pure function reduce(x)
    type(T) :: reduce
    type(T), intent(in) :: x(:)
    integer(int64) :: i

    if ( size(x, kind=int64) == 0 ) &
      error stop "In REDUCE, X must have at least one element."
    reduce = x(1)
    do i = 2, size(x,kind=int64)
       reduce = reduce + x(i)
    end do

  end function reduce

end module attribute_interface_reduce
