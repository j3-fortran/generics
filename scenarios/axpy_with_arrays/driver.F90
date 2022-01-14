program driver
   use Array_m, only: Array_t
   use AXPY_m, only: AXPY_t


   INSTANTIATE Array_t(real, 0), only: RealScalar => Array   ! scalar
   INSTANTIATE Array_t(real, 2), only: RealArray_2D => Array ! 2d array

   ! 1. instatiate function for a*x
   INSTANTIATE map_t(real, 0, real, 2, real, 2, real, operator(*)), only: times => map
   ! 2. instatiate operator for (ax) + y
   INSTANTIATE map_t(real, 2, real, 2, real, 2, real, operator(+)), only: plus => map
   ! 3. instantiate AXPY procedure
   INSTANTIATE AXPY_T(RealScalar, RealArray_2D, RealArray_2D, RealArray_2D, RealArray_2D, times, plus)

   ! and the rest is easy
   type(RealScalar) :: a ! coefficient
   type(RealArray_2d) :: x, y

   a = 2.
   x = reshape([1,2,3,4,5,6],[2,3]) 
   
   ! ta da
   call AXPY(a, x, y)
   
end program driver
   
