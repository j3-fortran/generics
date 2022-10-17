module RealInstance_mod
   use Matrix_mod
   implicit none

   integer, parameter :: n = 10

   ! Provide Matrix type and aux templates
   instantiate Matrix_tmpl(real, operator(+), operator(*), n), only: &
        Matrix, MatrixZero_tmpl,  MatrixOne_tmpl, &
        MatrixSubtraction_tmpl, MatrixPartialOrder_tmpl, GaussianSolver_tmpl
   instantiate MatrixZero_tmpl(zero_real)
   instantiate MatrixOne_tmpl(zero_real, zero_one)
   instantiate MatrixSubtraction_tmpl(operator(-))
   instantiate MatrixPartialOrder_tmpl(min,max)
   instantiate GaussianSolver_tmpl(zero_real, operator(-), operator(/))

contains

   elemental real function zero_real()
      zero_real = 0
   end function zero_real

   elemental real function one_real()
      one_real = 1
   end function one_real

end module RealInstance_mod

   
   
module ComplexInstance_mod
   use Matrix_mod
   implicit none

   integer, parameter :: n = 10

   ! Provide Matrix type and aux templates
   instantiate Matrix_tmpl(complex, operator(+), operator(*), n), only: &
        Matrix, MatrixZero_tmpl,  MatrixOne_tmpl, &
        MatrixSubtraction_tmpl, MatrixPartialOrder_tmpl, GaussianSolver_tmpl

   instantiate MatrixZero_tmpl(zero_complex)
   instantiate MatrixOne_tmpl(zero_complex, one_complex)
   instantiate MatrixSubtraction_tmpl(operator(-))
   instantiate MatrixPartialOrder_tmpl(min,max)
   instantiate GaussianSolver_tmpl(operator(/))

contains

   elemental complex function zero_complex()
      zero_complex = 0
   end function zero_complex

   elemental complex function one_complex()
      one_complex = 1
   end function one_complex

end module ComplexInstance_mod

   
module IntegerInstance_mod
   use Matrix_mod
   implicit none

   integer, parameter :: n = 10

   ! Provide Matrix type and aux templates
   instantiate Matrix_tmpl(integer, operator(+), operator(*), n), only: &
        Matrix, MatrixZero_tmpl,  MatrixOne_tmpl, &
        MatrixSubtraction_tmpl, MatrixPartialOrder_tmpl

   instantiate MatrixZero_tmpl(zero_integer)
   instantiate MatrixOne_tmpl(zero_integer, one_integer)
   instantiate MatrixSubtraction_tmpl(operator(-))
   instantiate MatrixPartialOrder_tmpl(min,max)

contains

   elemental integer function zero_integer()
      zero_integer = 0
   end function zero_integer

   elemental integer function one_integer()
      one_integer = 1
   end function one_integer

end module IntegerInstance_mod

   
module TropicalSemiRingInstance_mod
   use Matrix_mod
   implicit none

   integer, parameter :: n = 10

   ! Provide Matrix type and aux templates
   instantiate Matrix_tmpl(real, min, operator(+), n), only: &
        Matrix, MatrixZero_tmpl,  MatrixOne_tmpl

   instantiate MatrixZero_tmpl(inf_real)
   instantiate MatrixOne_tmpl(inf_real, zero_real)

contains

   elemental real function inf_real()
      use ieee_arithmetic
      inf_real = ieee_value(inf_real, ieee_positive_inf)
   end function inf_real

   elemental real function zero_real()
      zero_real = 0
   end function zero_real

end module TropicalSemiRingInstance_mod
   
