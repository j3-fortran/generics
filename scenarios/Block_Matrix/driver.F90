module ConcreteBlockMatrix_mod
  use GenericBlockMatrixMultiply_mod
  implicit none
  private

  public :: ConcreteA, ConcreteB, ConcreteC, ConcreteT

  type, extends(BlockA) :: ConcreteA
     real, allocatable :: matrix(:,:)
   contains
     procedure :: matrix_multiply
  end type ConcreteA

  type, extends(BlockB) :: ConcreteB
     real, allocatable :: matrix(:,:)
  end type ConcreteB

  type, extends(BlockC) :: ConcreteC
     integer :: ni, nj
     real, allocatable :: matrix(:,:)
   contains
     procedure :: plus
     procedure :: zero
     procedure :: assign  ! c = c
  end type ConcreteC

  type, extends(BlockT) :: ConcreteT
     real, allocatable :: matrix(:,:)
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
          t%matrix = matmul(a%matrix, b%matrix)
       class default
          error stop 'operation not supported for given subclass of BlockB'
       end select

    end select

  end function matrix_multiply


  function plus(c1, t) result(c2)
    class(BlockC), allocatable :: c2
    class(ConcreteC), intent(in) :: c1
    class(BlockT), intent(in) :: t

    integer :: ni, nj

    ni = c1%ni
    nj = c1%nj

    allocate(ConcreteC :: c2)

    select type (c2)
    type is (ConcreteC)
       allocate(c2%matrix(ni,nj))
       select type (c1)
       type is (ConcreteC)
          select type (t)
          class is (ConcreteT)
             c2%matrix = c1%matrix + t%matrix
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
       z%ni = c%ni
       z%nj = c%nj
       allocate(z%matrix(z%ni, z%nj))
       z%matrix = 0
    end select

    
  end function zero


  subroutine assign(c_out, c_in)
    class(ConcreteC), intent(out) :: c_out
    class(BlockC), intent(in) :: c_in

    select type (c_in)
    class is (ConcreteC)
       c_out%matrix = c_in%matrix
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
  integer, parameter :: ni = 1, nj = 1, nk = 2
  
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
        allocate(t_a(i,k)%matrix(ni,nk))
        t_a(i,k)%matrix = 1
     end do
  end do

  do j = 1, n_blocks_j
     do k = 1, n_blocks_k
        allocate(t_b(k,j)%matrix(nk,nj))
        t_b(k,j)%matrix = 1
     end do
  end do
  
  do j = 1, n_blocks_j
     do i = 1, n_blocks_i
        t_c(i,j)%ni = 1
        t_c(i,j)%nj = 1
     end do
  end do
  
  
  a = BlockMatrix(t_a, t_b, t_c, t_t)
  b = BlockMatrix(t_a, t_b, t_c, t_t)
  c = BlockMatrix(t_a, t_b, t_c, t_t)

  c = a .matmul. b

  ! Because seed values are all "1.", we expect result values to be
  ! n_blocks_k * nk

  do j = 1, size(t_c, 2)
     do i = 1, size(t_c, 1)
        select type (q => c%c(i,j))
        type is (ConcreteC)
           print*, i, j, all(q%matrix == n_blocks_k*nk)
        end select
     end do
  end do

end program driver
