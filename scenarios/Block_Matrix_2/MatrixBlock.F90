module MatrixBlock_m
   implicit none
   private

   public :: MatrixBLock_tmpl
   public :: MatMul_tmpl
   

   RESTRICTION :: IS_SUMMABLE(T)
      pure function sum(x) result(y)
         type(T), intent(in) :: x(:)
         type(T) :: y
      end function sum
   END RESTRICTION

   RESTRICTION :: SUPPORTS_OPERATOR(T, U, V, operator(+))
      elemental operator(+)(x, y) result(y)
         type(T), intent(in) :: x
         type(U), intent(in) :: y
         type(V) :: y
      end function operator(+)
   END RESTRICTION

   ! Operators here are _pointwise_
   template :: MatrixBlock_tmpl(T, operator(*), operator(+))
      type :: T
      end type T
      REQUIRES :: SUPPORTS_OPERATOR(T,T,T, operator(*))
      REQUIRES :: SUPPORTS_OPERATOR(T,T,T, operator(+))

      type MatrixBlock
         type(T), rank(2), allocatable :: elements
      end type MatrixBlock

      interface MatrixBlock
         module procedure new_MatrixBlock
      end interface assignment(=)

      interface operator(+)
         module procedure plus_block_matrix
      end interface operator(+)

      interface operator(*)
         module procedure times_block_matrix
      end interface operator(*)

   contains

      pure function MatrixBlock(x) result(bm)
         type(MatrixBlock) :: bm
         type(T), intent(in) :: x(:,:)
         bm%elements = x
      end function MatrixBlock

      elemental subroutine plus_block_matrix(x, y) result(z)
         type(MatrixBlock), intent(in) :: x
         type(MatrixBlock), intent(in) :: y
         type(MatrixBlock) :: z
         z%elements = x%elements + y%elements
      end subroutine sum_block_matrix

      elemental subroutine times_block_matrix(x, y) result(z)
         type(MatrixBlock), intent(in) :: x
         type(MatrixBlock), intent(in) :: y
         type(MatrixBlock) :: z
         z%elements = x%elements * y%elements
      end subroutine times_block_matrix

   end module MatrixBlock_m

   TEMPLATE :: Sum_tmpl(T, operator(+))
      type :: T
      end type T
      REQUIRES :: SUPPORTS_OPERATOR(T, T, T, operator(+))

      interface :: sum
         module procedure sum_T
      end interface

   contains

      ! Assumes x is not an empty array - no
      ! default "zero" for MatrixBlock of unknown shape.
      pure function sum_T(x) result(y)
         type(T), intent(in) :: x(:)
         type(T) :: y

         integer :: i

         y = x(1)
         do i = 2, size(x)
            y = y + x
         end do
      end function sum_T

   END TEMPLATE
      

   TEMPLATE :: MatrixMultiply_tmpl(T, U, V, operator(.matmul.), operator(+))
      type :: T
      end type T
      type :: U
      end type U
      type :: V
      end type V
      procedure :: times ! element multiplication
      procedure :: sum   ! element product summation

      REQUIRES :: SUPPORTS_OPERATOR(T, U, V, operator(.matmul.))
      REQUIRES :: SUPPORTS_OPERATOR(V, V, V, operator(+))

      interface operator(.matmul.)
         module procedure mb_mb_multiply
         module procedure scalar_mb_multiply
         module procedure mb_scalar_multiply
      end interface operator(.matmul.)


   contains

      pure function mb_mb_multiply(x, y) result(z)
         use MatrixBlock_tmpl
         use Sum_tmpl
         INSTANTIATE MB_T_m => MatrixBlock_tmpl(T)
         INSTANTIATE MB_U_m => MatrixBlock_tmpl(U)
         INSTANTIATE MB_V_m => MatrixBlock_tmpl(V)
         INSTANTIATE local_sum => Sum_tmpl(V,operator(+))

         use MB_T_m, only: MB_T
         use MB_U_m, only: MB_U
         use MB_V_m, only: MB_V

         type(MB_T), intent(in) :: x
         type(MB_U), intent(in) :: y
         type(MB_V) :: z

         integer :: i, j

         associate (ni => size(x%elements,1), nj => size(y%elements,2))
           allocate(z%elements(ni,nj))

           do concurrent (i=1:ni,j=1:nj)
              z%elements(i,j) = sum(x%elements(i,:) .matmul. y%elements(:,j))
           end do

         end associate

      end function mb_mb_multiply


      pure function scalar_mb_multiply(x,y) result(z)
         INSTANTIATE MB_U_m => MatrixBlock_t(U)
         INSTANTIATE MB_V_m => MatrixBlock_t(V)

         use MB_U_m, only: MB_U
         use MB_V_m, only: MB_V

         type(T), intent(in) :: x
         type(MB_U), intent(in) :: y
         type(MB_V) :: z

         z%elements = x .matmul. y%elements ! using ELEMENTAL

      end function scalar_mb_multiply

      pure function mb_scalar_multiply(x,y) result(z)
         INSTANTIATE MB_T_m => MatrixBlock_t(T)
         INSTANTIATE MB_V_m => MatrixBlock_t(V)

         use MB_T_m, only: MB_T
         use MB_V_m, only: MB_V

         type(MB_T), intent(in) :: x
         type(U), intent(in) :: y
         type(MB_V) :: z

         z = x%elements .matmul. y ! ELEMENTAL

      end function scalar_mb_multiply


   END TEMPLATE



end module MatrixBlock_m
