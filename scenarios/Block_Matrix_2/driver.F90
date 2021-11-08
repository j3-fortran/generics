program driver

   call vanilla()
   call block_multiply()

contains

   ! Implements regular matrix multiplication of two real arrays.
   subroutine vanilla()
      use MatrixBlock_m, only: MatrixBlock_tmpl ! template
      INSTANTIATE :: MB => MatrixBlock_tmpl(real, operator(*), operator(+))
      
      use MatrixBlock_m, only: MatrixMultiply_tmpl ! template
      INSTANTIATE :: local => MatrixMultiply_tmpl(MB, MB, MB, operator(*), operator(+))
      use local, only: operator(.matmul.)

      type(MB) :: A, B, C

      
      A = MB(real(reshape([(i, i=1,6)],[3,2])))
      B = MB(real(reshape([(i, i=1,6)],[2,3])))

      C = A .matmul. B

   end subroutine vanilla


   ! implements matrix matrix multiply A .matmul. B = C, where
   !   - A is a block of integeres
   !   - B is a matrix of blocks of reals
   !   - C is a matrix of blocks of reals

   subroutine block_multiply()
      use MatrixBlock_m, only: MatrixBlock_tmpl ! template

      INSTANTIATE :: MB_int_m => MatrixBlock_tmpl(integer, operator(*), operator(+))
      use MB_int_m, only: MB_int => MatrixBlock, operator(+)
      INSTANTIATE :: MB1_real_m => MatrixBlock_tmpl(real, operator(*), operator(+))
      use MB1_real_m, only: MB1_real => MatrixBlock, operator(+), operator(*) 
      INSTANTIATE :: MB2_real_m => MatrixBlock_tmpl(MB1, operator(*), operator(+)) ! higher order blocks
      use MB2_real_m, only: MB2_real => MatrixBlock, operator(+)
      
      use MatrixBlock_m, only: MatrixMultiply_tmpl ! template
      INSTANTIATE :: MM1_m => MatrixMultiply_tmpl(integer, MB1_real, MB1_real, operator(*), operator(+))
      use MM1_m, only: operator(.matmul.)

      INSTANTIATE :: MM2_m => MatrixMultiply_tmpl(MB_int, MB2_real, MB2_real, operator(.matmul.), operator(+))
      use MM2_m, only: operator(.matmul.)

      type(MB_int) :: A
      type(MB2_Real) :: B
      type(MB2_Real) :: C

      ! initial values
      A = MB_int(reshape([(i, i=1,6)],[3,2]))

      B = MB2_real(reshape(...))
      allocate(B(2,2))
      c = a .matmul. b
   end subroutine block_multiply



end program driver
