module SimpleBlock_mod
  use GenericBlockMatrixMultiply_mod
  implicit none
  private

  public :: SimpleA, SimpleB, SimpleC, SimpleT

  type, extends(BlockA) :: SimpleA
     real :: element
   contains
     procedure :: matrix_multiply
  end type SimpleA

  type, extends(BlockB) :: SimpleB
     real :: element
  end type SimpleB

  type, extends(BlockC) :: SimpleC
     real :: element
   contains
     procedure :: plus_c_t
     procedure :: plus_c_c
     procedure :: zero
     procedure :: assign  ! c = c
  end type SimpleC

  type, extends(BlockT) :: SimpleT
     real :: element
  end type SimpleT


contains

  function matrix_multiply(a, b) result(t)
    class(BlockT), allocatable :: t
    class(SimpleA), intent(in) :: a
    class(BlockB), intent(in) :: b

    allocate(SimpleT :: T)

    select type (t)
    type is (SimpleT)

       select type (b)
       type is (SimpleB)
          t%element = a%element * b%element
       class default
          error stop 'operation not supported for given subclass of BlockB'
       end select

    end select

  end function matrix_multiply


  function plus_c_t(c1, t) result(c2)
    class(BlockC), allocatable :: c2
    class(SimpleC), intent(in) :: c1
    class(BlockT), intent(in) :: t

    allocate(SimpleC :: c2)
    
    select type (c2)
    type is (SimpleC)
       select type (t)
       type is (SimpleT)
          c2%element = c1%element + t%element
       class default
          error stop 'operation not supported for given subclass of BlockT'
       end select
    end select

  end function plus_c_t


  function plus_c_c(c1, c2) result(c3)
    class(BlockC), allocatable :: c3
    class(SimpleC), intent(in) :: c1
    class(BlockC), intent(in) :: c2

    allocate(SimpleC :: c3)
    
    select type (c3)
    type is (SimpleC)
       select type (c2)
       type is (SimpleC)
          c3%element = c1%element + c2%element
       class default
          error stop 'operation not supported for given subclass of BlockT'
       end select
    end select

  end function plus_c_c


  function zero(c) result(z)
    class(BlockC), allocatable :: z
    class(SimpleC), intent(in) :: c

    allocate(SimpleC :: z)

    select type (z)
    type is (SimpleC)
       z%element = 0
    end select
    
  end function zero

  subroutine assign(c_out, c_in)
    class(SimpleC), intent(out) :: c_out
    class(BlockC), intent(in) :: c_in

    select type (c_in)
    class is (SimpleC)
       c_out%element = c_in%element
    class default
       error stop 'operation not supported for given subclass of BlockC'
    end select

  end subroutine assign

end module SimpleBlock_mod

