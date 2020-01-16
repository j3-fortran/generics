module NestedBlock_mod
  use GenericBlockMatrixMultiply_mod
  use SimpleBlock_mod
  implicit none
  private

  public :: NestedA, NestedB, NestedC, NestedT

  type, extends(BlockA) :: NestedA
     type(BlockMatrix) :: matrix
   contains
     procedure :: matrix_multiply
  end type NestedA

  type, extends(BlockB) :: NestedB
     type(BlockMatrix) :: matrix
  end type NestedB

  type, extends(BlockC) :: NestedC
     type(BlockMatrix) :: matrix
   contains
     procedure :: plus_c_t
     procedure :: plus_c_c
     procedure :: zero
     procedure :: assign => assign_c  ! c = c
  end type NestedC

  type, extends(BlockT) :: NestedT
     type(BlockMatrix) :: matrix
  end type NestedT


contains

  recursive subroutine assign_c(c_out, c_in)
    class(NestedC), intent(out) :: c_out
    class(BlockC), intent(in) :: c_in

    type(BlockMatrix) :: t
    select type (q =>c_in)
    class is (NestedC)
       ! next line suddenly started breaking on NAG 6247.
       ! Renaming "assign" => "assign_c" did not help.
       ! Introducing temporary does.
!!$       c_out%matrix = q%matrix
       t = q%matrix
       c_out%matrix = t
    class default
       error stop 'operation not supported for given subclass of BlockC'
    end select

  end subroutine assign_c

  ! Concern: the contained BlockMatrix multiply returns something of
  ! class BlockC, but here we need to produce something of class
  ! BlockT.
  !
  ! Then ... there is the issue that the underlying t component is not
  ! allocated, but is instead the c component.
  recursive function matrix_multiply(a, b) result(t)
    class(BlockT), allocatable :: t

    class(NestedA), intent(in) :: a
    class(BlockB), intent(in) :: b

    allocate(NestedT :: t)
    select type (t)
    type is (NestedT)

       select type (b)
       type is (NestedB)
          t%matrix = (a%matrix .matmul. b%matrix)
       class default
          error stop 'operation not supported for given subclass of BlockB'
       end select

    end select

  end function matrix_multiply



  recursive function plus_c_t(c1, t) result(c2)
    class(BlockC), allocatable :: c2
    class(NestedC), intent(in) :: c1
    class(BlockT), intent(in) :: t

    integer :: i, j

!!$    allocate(NestedC :: c2)
    allocate(c2, source=c1)
    select type (c2)
    type is (NestedC)
       select type (t)
       type is (NestedT)
          do j = 1, size(c1%matrix%c,2)
             do i = 1, size(c1%matrix%c,1)
                c2%matrix%c(i,j) = c1%matrix%c(i,j) + t%matrix%c(i,j)
             end do
          end do
       class default
          error stop 'operation not supported for given subclass of BlockT'
       end select
    end select

  end function plus_c_t

  recursive function plus_c_c(c1, c2) result(c3)
    class(BlockC), allocatable :: c3
    class(NestedC), intent(in) :: c1
    class(BlockC), intent(in) :: c2

    integer :: i, j

    allocate(NestedC :: c3)

    select type (c3)
    type is (NestedC)
       select type (c2)
       type is (NestedC)
          do j = 1, size(c1%matrix%c,2)
             do i = 1, size(c1%matrix%c,1)
                c3%matrix%c(i,j) = c1%matrix%c(i,j) + c2%matrix%c(i,j)
             end do
          end do
       class default
          error stop 'operation not supported for given subclass of BlockT'
       end select
    end select

  end function plus_c_c

  recursive function zero(c) result(z)
    class(BlockC), allocatable :: z
    class(NestedC), intent(in) :: c

    integer :: ni, nj
    integer :: i, j


!!$    allocate(NestedC :: z)
    allocate(z, source=c)
    select type (z)
    type is (NestedC)
       ni = size(c%matrix%c,1)
       nj = size(c%matrix%c,2)
!!$       allocate(z%matrix%c(ni,nj), mold=c%matrix%c(1,1))
       z%matrix%c = c%matrix%c
       do j = 1, nj
          do i = 1, ni
             z%matrix%c(i,j) = c%matrix%c(i,j)%zero()
          end do
       end do
    end select
    
  end function zero

end module NestedBlock_mod

