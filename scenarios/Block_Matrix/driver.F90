module ConcreteBlockMatrix_mod
  use GenericBlockMatrixMultiply_mod
  implicit none
  private

  public :: ConcreteA, ConcreteB, ConcreteC, ConcreteT

  type, extends(BlockA) :: ConcreteA
     real :: element
   contains
     procedure :: matrix_multiply
  end type ConcreteA

  type, extends(BlockB) :: ConcreteB
     real :: element
  end type ConcreteB

  type, extends(BlockC) :: ConcreteC
     real :: element
   contains
     procedure :: plus
     procedure :: zero
     procedure :: assign  ! c = c
  end type ConcreteC

  type, extends(BlockT) :: ConcreteT
     real :: element
  end type ConcreteT


contains

  function matrix_multiply(a, b) result(t)
    class(ConcreteA), intent(in) :: a
    class(BlockT), allocatable :: t
    class(BlockB), intent(in) :: b

    allocate(ConcreteT :: t)
    select type (t)
    type is (ConcreteT)

       select type (b)
       type is (ConcreteB)
          t%element = a%element * b%element
       class default
          error stop 'operation not supported for given subclass of BlockB'
       end select

    end select

  end function matrix_multiply


  function plus(c1, t) result(c2)
    class(BlockC), allocatable :: c2
    class(ConcreteC), intent(in) :: c1
    class(BlockT), intent(in) :: t

    allocate(ConcreteC :: c2)

    select type (c2)
    type is (ConcreteC)
       select type (c1)
       type is (ConcreteC)
          select type (t)
          class is (ConcreteT)
             c2%element = c1%element + t%element
          class default
             error stop 'operation not supported for given subclass of BlockT'
          end select
       end select
    end select

  end function plus


  function zero(c) result(z)
    class(BlockC), allocatable :: z
    class(ConcreteC), intent(in) :: c

    allocate(ConcreteC :: z)

    select type (z)
    type is (ConcreteC)
       z%element = 0
    end select
    
  end function zero


  subroutine assign(c_out, c_in)
    class(ConcreteC), intent(out) :: c_out
    class(BlockC), intent(in) :: c_in

    select type (c_in)
    class is (ConcreteC)
       c_out%element = c_in%element
    class default
       error stop 'operation not supported for given subclass of BlockC'
    end select

  end subroutine assign

end module ConcreteBlockMatrix_mod


  

program driver
  use GenericBlockMatrixMultiply_mod
  use ConcreteBlockMatrix_mod
  implicit none

  integer :: n_blocks_i, n_blocks_j, n_blocks_k
  
  type(ConcreteA), allocatable :: t_a(:,:)
  type(ConcreteB), allocatable :: t_b(:,:)
  type(ConcreteC), allocatable :: t_c(:,:)
  type(ConcreteT) :: t_t

  type(BlockMatrix) :: a, b, c
  integer :: i, j, k

  n_blocks_i = 3
  n_blocks_j = 2
  n_blocks_k = 4

  
  allocate(t_a(n_blocks_i, n_blocks_k))
  allocate(t_b(n_blocks_k, n_blocks_j))
  allocate(t_c(n_blocks_i, n_blocks_j))

  do k = 1, n_blocks_k
     do i = 1, n_blocks_i
        t_a(i,k)%element = 1
     end do
  end do

  do j = 1, n_blocks_j
     do k = 1, n_blocks_k
        t_b(k,j)%element = 1
     end do
  end do
  
  a = BlockMatrix(t_a, t_b, t_c, t_t)
  b = BlockMatrix(t_a, t_b, t_c, t_t)
  c = BlockMatrix(t_a, t_b, t_c, t_t)

  c = a .matmul. b

  ! Because seed values are all "1.", we expect result values to be
  ! n_blocks_k

  do j = 1, size(t_c, 2)
     do i = 1, size(t_c, 1)
        select type (q => c%c(i,j))
        type is (ConcreteC)
           print*, i, j, q%element == n_blocks_k
        end select
     end do
  end do

end program driver
