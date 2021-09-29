module Vector_mod

   type :: vector<T>
      type(T), allocatable :: elements(:)
   contains
      procedure :: push_back
      procedure :: back
   end type vector<T>

contains

   subroutine push_back(this, x)
      class(vector<T>), intent(inout) :: this
      type(T), intent(in) :: x

      this%elements = [this%elements, x)
   end subroutine push_back

   function back(this) result(x)
      class(vector<T>), target, intent(in) :: this
      type(T), pointer :: x
      associate (n => size(this%elements))
        if (n > 0) then
           x => this%elements(n)
        else
           x => null()
        end if
   end function back

end module Vector_mod


module ArrayTemplate_mod

   type :: Array<T, n>
      type(T), allocatable, rank(n) :: elements ! variant 1
      ! type(T), pointer, rank(n) :: elements ! variant 2
   end type Array

contains

   function wrap(x)
      type (Array<T,n>, pointer :: wrap
      type(T), rank(n), target, intent(in) :: x(:)
      wrap%elements = x
   end function wrap

   function unwrap(wrap) result(x)
      type(T), rank(n), pointer :: x
      type(Array<T,n>), pointer, intent(in) :: wrap
      if (associated(wrap)) then
         x => wrap%elements
      else
         x => null()
      end if
   end function unwrap

end module ArrayTemplate_mod


program main
   use Vector_mod
   use ArrayTemplate_mod

   type(vector<Array<real,1>>)  :: v ! vector of 1D arrays of variable size

   real, allocatable :: x(:)
   real, pointer :: y(:)
   x = [5,7,8]

   ! Would like to do:
   call v%push_back(x)  ! x  is not type(Array<real,1>)
   y => v%back()
   
   ! would instead need to do
   call v%push_back(wrap(x)
   y => unwrap(v%back())

end program main
   !-------------

   ! Problems:
   !
   !  (1) With wrapper containining ALLOCATABLE, dummy arguments
   !      cannot be INTENT(INOUT) or INTENT(OUT).
   !  (2) With wrapper containing POINTER, arguments must have TARGET attribute.     
   !  (3) Laborious for commun use cases either way.
   !
   ! What we want is something that has semantics similar to argument association.

   
module Array_Templatemod

   ! New type attribute "ARGUMENT_ASSOCIATED"
   ! Can only have one data component.
   ! Dummy arguments of such types can be associated with actual arguments of the 
   ! wrapped component
   ! Function results of ARGUMENT_ASSOCIATED types can be automatically converted
   ! to RHS.  (More problematic case.)

   ! Example
   type, ARGUMENT_ASSOCIATED :: Array<T, n>
      type(T), rank(n) :: elements ! neither pointer nor allocatable
   end type Array

end module Array_Templatemod


! What about "inducing" other operators?  E.g., what if we have a
! template that requires a type parameter has the "+" operator.

module something_mod
   concept :: addable(T,U,V)
      operator:  V = T + U
   end concept

contains
   subroutine needs_plus<T,U,V>(a,b,c)
      requirements :: addable(T,U,V)

      c = a + b
   end subroutine needs_plus<T

end module something_mod

program main
   use something_mod
   use ArrayTemplate_mod


   integer, allocatable :: a(:,:)
   real :: b
   real, allocatable :: c(:,:)

   allocate(a(5,3))
   a = 7
   b = 3

   ! Because Array is a wrap type, it supports the necessary operation.
   call needs_plus<Array<integer,2>,real,Array<real,2>>(a,b,c)

end program main

