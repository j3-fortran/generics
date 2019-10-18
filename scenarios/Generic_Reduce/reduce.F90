module generic_reduce
  implicit none


  requirement :: R
     type (T), parameter :: zero
   contains
     procedure :: operator_sum
     procedure :: operator_multiply
     procedure :: assign
     procedure :: identity_sum
     procedure :: identity_multiply
     generic :: .operator. => operator
     generic :: assignment(=) => assign
     
   end requirement

   elemental function reduce(x)
    type(T) :: reduce
    type(T), intent(in) :: x
    
    integer :: i

    sum = T%unit
    do i = 1, size(x)
       product = sum .oper. x(i)
    end do

  end function reduce

  function reduce_square(x)
    type(T) :: reduce
    type(T), intent(in) :: x(:)
    
    integer :: i

    sum = T%unit
    do i = 1, size(x)
       product = sum .oper. (x(i)*x(i))
    end do

  end function reduce

  function reduce_i_square(x)
    type(T) :: reduce
    type(T), intent(in) :: x
    
    integer :: i

    sum = T%unit
    do i = 1, size(x)
       product = sum .oper. (i*(x(i)*x(i)))
    end do

  end function reduce_i_square

  function sum(x)
    type (T) :: sum
    type (T), intent(in) :: x(:)

    integer :: i
    sum = T%zero
    do i = 1, size(x)
       sum = sum + x(i) ! uses "+"
    end do
    
  end function sum




    
  
