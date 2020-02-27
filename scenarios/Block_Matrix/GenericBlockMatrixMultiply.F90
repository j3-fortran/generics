#define REQUIREMENT type, abstract
module GenericBlockMatrixMultiply_mod
  implicit none
  private

  ! Type parameters
  public ::  BlockA
  public ::  BlockB
  public ::  BlockC
  public ::  BlockT

  ! Generic
  public :: BlockMatrix


  ! Requirements on type parameters
  type, abstract :: BlockA
   contains
     procedure(matrix_multiply_t), deferred :: matrix_multiply   ! BlockA .matmul. BlockB
     generic :: operator(.matmul.) => matrix_multiply
  end type BlockA

  ! BlockB objects are things that BlockA objects can multiply on the left to return BlockT objects
  type, abstract :: BlockB
   contains
  end type BlockB

  ! BlockT: return type for (BlockA .matmul. BlockB).
  ! This is not necessarily type BlockC, so long as BlockT supports
  ! BlockC = BlockC + BlockT
  type, abstract :: BlockT
  end type BlockT

  ! BlockC objects are things that can add to BlockT objects (on the left)
  ! Also need to be settable to "zero" to begin accumulation.
  type, abstract :: BlockC
   contains
     procedure(plus_c_t), deferred :: plus_c_t !   c = a + t
     procedure(plus_c_c), deferred :: plus_c_c !   c = a + c
     generic :: operator(+) => plus_c_t, plus_c_c
     procedure(zero_c), deferred :: zero
     procedure(assign_c), deferred :: assign  ! c = c
     generic :: assignment(=) => assign
  end type BlockC

  abstract interface


     function matrix_multiply_t(a,b) result(t)
       import BlockA, BlockB, BlockT
       class(BlockT), allocatable :: t
       class(BlockA), intent(in) :: a
       class(BlockB), intent(in) :: b
     end function matrix_multiply_t

     function plus_c_t(c1,t) RESULT(c2)
       import BlockC, BlockT
       class(BlockC), allocatable :: c2
       class(BlockC), intent(in) :: c1
       class(BlockT), intent(in) :: t
     end function plus_c_t

     function plus_c_c(c1,c2) RESULT(c3)
       import BlockC
       class(BlockC), allocatable :: c3
       class(BlockC), intent(in) :: c1
       class(BlockC), intent(in) :: c2
     end function plus_c_c

     subroutine assign_c(c_out, c_in)
       import BlockC
       class(BlockC), intent(out) :: c_out
       class(BlockC), intent(in) :: c_in
     end subroutine assign_c

     function zero_c(c)
       import BlockC
       class(BlockC), intent(in) :: c ! needed for bounds
       class(BlockC), allocatable :: zero_c
     end function zero_c

  end interface

  ! Now we can implement a generic algorithm

  type :: BlockMatrix ! <BlockA,BlockB,BlockC>
     class(BlockA), allocatable :: a(:,:)
     class(BlockB), allocatable :: b(:,:)
     class(BlockC), allocatable :: c(:,:)
     class(BlockT), allocatable :: t(:,:)
   contains
     procedure :: a_x_b_to_c
     generic :: operator(.matmul.) => a_x_b_to_c
!!$     procedure :: plus_c_t
!!$     generic :: operator(+) => plus_c_t
!!$     procedure :: zero => zero_matrix
  end type BlockMatrix

  interface BlockMatrix
     module procedure new_BlockMatrix
     module procedure new_BlockMatrix_a
     module procedure new_BlockMatrix_b
     module procedure new_BlockMatrix_c
     module procedure new_BlockMatrix_t
  end interface BlockMatrix

contains


  function new_BlockMatrix(a, b, c, t) result(m)
    type(BlockMatrix) :: m
    class(BlockA), intent(in) :: a(:,:)
    class(BlockB), intent(in) :: b(:,:)
    class(BlockC), intent(in) :: c(:,:)
    class(BlockT), intent(in) :: t(:,:)

    m%a = a
    m%b = b
    m%c = c
    m%t = t

  end function new_BlockMatrix

  function new_BlockMatrix_a(a) result(m)
    type(BlockMatrix) :: m
    class(BlockA), intent(in) :: a(:,:)

    m%a = a

  end function new_BlockMatrix_a

  function new_BlockMatrix_b(b) result(m)
    type(BlockMatrix) :: m
    class(BlockB), intent(in) :: b(:,:)

    m%b = b

  end function new_BlockMatrix_b

  function new_BlockMatrix_c(c) result(m)
    type(BlockMatrix) :: m
    class(BlockC), intent(in) :: c(:,:)

    m%c = c

  end function new_BlockMatrix_c


  function new_BlockMatrix_t(t) result(m)
    type(BlockMatrix) :: m
    class(BlockT), intent(in) :: t(:,:)

    m%t = t

  end function new_BlockMatrix_t


  recursive function a_x_b_to_c(a, b) RESULT(c)
    type(BlockMatrix) :: c
    class(BlockMatrix), intent(in) :: a
    type(BlockMatrix), intent(in) :: b
    
    integer :: i, j, k
    integer :: ni, nj, nk


    ni = size(a%a,1)
    nj = size(b%b,2)
    nk = size(a%a,2)

    if (size(b%b,1) /= nk) error stop 'incompatible shapes'

!!$    allocate(c%c(ni,nj), mold=a%c)
    allocate(c%c(ni,nj), source=a%c)

    do j = 1, nj
       do i = 1, ni
          c%c(i,j) = a%c(i,j)%zero()
          do k = 1, nk
             c%c(i,j) = c%c(i,j) + (a%a(i,k) .matmul. b%b(k,j))
          end do
       end do
    end do

  end function a_x_b_to_c
    

end module GenericBlockMatrixMultiply_mod
