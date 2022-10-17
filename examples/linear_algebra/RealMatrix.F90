module usage_mod
   use Matrix_mod
   implicit none

   public :: RealMatrix
   public :: zero
   public :: one
   public :: minus
   public :: min, max
   public :: div
   

   instantiate Matrix_tmpl(real,+,*,3): RealMatrix => Matrix
   instantiate MatrixZero_tmpl(zero_real)
   instantiate MatrixOne_tmpl(one_real)
   instantiate MatrixMinus_tmpl(-)
   instantiate MatrixOrder_tmpl(min, max)
   instantiate MatrixDiv_tmpl(/)


contains


   pure function zero_real()
      real :: zero_real
      zero_real = 0
   end function zero_real

   pure function one_real()
      real :: one_real
      one_real = 1
   end function one_real

end module usage_mod
