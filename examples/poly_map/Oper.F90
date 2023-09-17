module Oper_m
   implicit none

   requirement Oper(T, op)
      type, deferred :: T
      interface
         logical function op(x,y)
            type(T), intent(in) :: x
            type(T), intent(in) :: y
         end function Op
      end interface
   end requirement

   requirement PolyOper(T, op)
      class, deferred :: T
      interface
         logical function op(x,y)
            class(T), intent(in) :: x
            class(T), intent(in) :: y
         end function op
      end interface
   end requirement

end module Oper_m
