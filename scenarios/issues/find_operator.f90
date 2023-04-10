program templates_and_operator_overloading

template find_tmpl(T, equals)
   type, deferred :: T
   interface
      pure logical function equals(a,b)
         type(T), intent(in) :: a, b
      end function equals
   end interface

   interface operator (==)
      procedure, deferred :: equals
   end interface

   public :: find, operator(==) ! Can one export == from here?

contains

   function find(v, val) result(iter)
      type(iterator) :: iter
      type(vector), intent(in) :: v
      type(T), intent(in) :: val


      iter = v%begin()
      do while (iter /= v%end())
         if(iter%of()==val) return
         call iter%next()
      end do

   end function find

end template find_tmpl

! We want to use the intrinsic == for vectors of integers
! We should be able to instantiate with operator(==) as argument
! But if find_tmpl exports operator(==) as public, what does
! operator(==) refer to at this point?
instantiate find_tmpl(integer, operator(==))

end program

