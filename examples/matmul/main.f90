program main
   use LinearAlgebra_m, only: MatrixMultiply_t, MatrixBlock_t


   instantiate MatrixBlock_t(real), only: RealBlock => MatrixBlock, reduce_RealBlock_t => reduce_t
   instantiate reduce_RealBlock_t(sum), only: sum => reduce

   instantiate MatrixBlock_t(complex), only: ComplexBlock => MatrixBlock, reduce_ComplexBlock_t => reduce_t
   instantiate reduce_ComplexBlock_t(sum), only: sum => reduce

   instantiate MatrixBlock_t(RealBlock), only: NestedRealBlock => MatrixBlock
   instantiate MatrixBlock_t(ComplexBlock), only: NestedComplexBlock => MatrixBlock

   call simple_driver()
   call block_matmul_driver()
   call nested_driver()
   
contains


   ! The following procedure implements and uses matrix multiplication
   ! of an ordinary real array and an ordinary integer array to produce
   ! an ordinary real array.
   subroutine simple_driver()

      INSTANTIATE MatrixMultiply_t(real, integer, real, times, sum), only: matmul

      real :: x(3, 2), y(2, 3), z(3, 3)

      x = reshape([(real(i), i = 1, 6)], [3, 2])
      y = reshape([(real(i), i = 6, 1, -1)], [2, 3])

      z = matmul(x, y)

      do i = 1, 3
         print *, z(i,:)
      end do

   contains

      elemental function times(x,y) result(z)
         real :: z
         real, intent(in) :: x
         integer, intent(in) :: i

         z = x*i
      end function times
      
   end subroutine simple_driver


   subroutine block_matmul_driver()

      INSTANTIATE MatrixMultiply_t(RealBlock, ComplexBlock, ComplexBlock, matmul, sum), only: block_matmul => matmul

      type(RealBlock)    :: A(7,2)
      type(ComplexBlock) :: B(2,3)
      type(ComplexBlock) :: C(7,3)

      call initialize_A(A)
      call initialize_B(B)

      C = block_matmul(A, B)

      call  print_results(C)

   end subroutine block_matmul_driver


   subroutine nested_driver()

      INSTANTIATE MatrixMultiply_t(RealBlock, ComplexBlock, ComplexBlock, times, sum), only: block_matmul => matmul
      INSTANTIATE MatrixMultiply_t(NestedRealBlock, NestedComplexBlock, NestedComplexBlock, block_matmul, sum), only: nested_matmul => matmul

      type(NestedRealBlock)    :: A(7,2)
      type(NestedComplexBlock) :: B(2,3)
      type(NestedComplexBlock) :: C(7,3)

      call initialize_A(A)
      call initialize_B(B)

      C = nested_matmul(A, B)

      call  print_results(C)

   end subroutine nested_driver



end program
