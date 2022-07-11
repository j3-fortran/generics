program main
   use matmul_m, only: matmul_t

   call simple_driver()
   call block_matmul_driver()
   
contains


   ! The following procedure implements and uses matrix multiplication
   ! of an ordinary real array and an ordinary integer array to produce
   ! an ordinary real array.
   subroutine simple_driver()

      INSTANTIATE matmul_t(real, integer, real, times, sum), only: matmul

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

      type :: RealBlock
         real, allocatable :: elements(:,:)
      end type RealBlock

      type :: ComplexBlock_block
         complex, allocatable :: elements(:,:)
      end type ComplexBlock_block

      INSTANTIATE matmul_t(RealBlock, ComplexBlock, ComplexBlock, times, sum), only: block_matmul => matmul

      type(RealBlock)    :: A(7,2)
      type(ComplexBlock) :: B(2,3)
      type(ComplexBlock) :: C(7,3)

      call initialize_A(A)
      call initialize_B(B)

      C = block_matmul(A, B)

      call  print_results(C)

   contains

      pure function sum(x) result(s)
         type(ComplexBlock) :: s
         type(ComplexBlock), intent(in) :: x(:)

         integer :: i
         
         s%elements = 0
         do i = 1:size(x)
            s%elements = s%elements + x%elements
         end do

      end function sum

      elemental function times(x,y) result(z)
         type(ComplexBlock) :: z
         type(RealBlock), intent(in) :: x
         type(ComplexBlock), intent(in) :: y
         real, intent(in) :: x
         integer, intent(in) :: i

         ! Exploit optimized built-in matmul
         z%elements = matmul(x%elements, y%elements)
      end function times
      
   end subroutine simple_driver



end program
