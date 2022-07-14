program main
   use LinearAlgebra_m, only: MatrixMultiply_t, MatrixBlock_t

   call simple_driver()
   call block_matmul_driver()
   call nested_driver()
   
contains


   ! The following procedure implements and uses matrix multiplication
   ! of an ordinary real array and an ordinary integer array to produce
   ! an ordinary real array.
   subroutine simple_driver()

      INSTANTIATE MatrixMultiply_t(real, integer, real, operator(*), sum), only: matmul

      real :: A(3, 2), C(3, 3)
      integer :: B(2,3)

      A = reshape([(real(i), i = 1, 6)], [3, 2])
      B = reshape([(i, i = 6, 1, -1)], [2, 3])

      ! C = A x B
      ! Note: We could have used intrinsic matmul instead.
      C = matmul(A, B)

      do i = 1, 3
         print *, C(i,:)
      end do
      
   end subroutine simple_driver


   
   ! The following procedure instantiates a matmul() procedure that
   ! acts on arrays whose elements are blocks (encapsulated arrays).

   subroutine block_matmul_driver()

      INSTANTIATE MatrixBlock_t(real), only: RealBlock => MatrixBlock
      INSTANTIATE MatrixBlock_t(complex), only: ComplexBlock => MatrixBlock, reduce_T
      INSTANTIATE reduce_T(sum), only: sum => reduce
      ! Note: matmul template parameter is the intrinsic matmul procedure.
      INSTANTIATE MatrixMultiply_t(RealBlock, ComplexBlock, ComplexBlock, matmul, sum), only: matmul

      type(RealBlock)    :: A(7,2)
      type(ComplexBlock) :: B(2,3)
      type(ComplexBlock) :: C(7,3)

      call initialize_A(A)
      call initialize_B(B)

      ! C = A x B
      C = block_matmul(A, B)

      call  print_results(C)

   end subroutine block_matmul_driver


   ! Now we go one step further and have arrays whole elements are themselves arays of blocks.
   subroutine nested_driver()

      INSTANTIATE MatrixBlock_t(real), only: RealBlock => MatrixBlock
      INSTANTIATE MatrixBlock_t(complex), only: ComplexBlock => MatrixBlock, ComplexBlock_reduce_T =E reduce_T
      INSTANTIATE ComplexBlock_reduce_T(sum), only: sum => reduce

      INSTANTIATE MatrixBlock_t(RealBlock), only: NestedRealBlock => MatrixBlock
      INSTANTIATE MatrixBlock_t(ComplexBlock), only: NestedComplexBlock => MatrixBlock, NestedComplexBlock_reduce_T => reduce_T
      INSTANTIATE NestedComplexBlock_reduce_T(sum), only: sum => reduce

      ! First we instantiate the matmul() to be used on the inner block arrays:
      INSTANTIATE MatrixMultiply_t(RealBlock, ComplexBlock, ComplexBlock, matmul, sum), only: block_matmul => matmul
      ! And now we can instantiate matmul() on the outer arrays:
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
