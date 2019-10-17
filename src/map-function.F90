module map_function
  implicit none

#define requirements

  requirements type :: T
  end type

  requirements type :: U
  end type

  requirements type :: V
  end type

  requirements interface

    module function f(u_, v_) result(t_)
      type(U), intent(in) :: u_
      type(V), intent(in) :: v_
      type(T) t_
    end function

  end interface

contains

  module function map_f( u_array, v_array) result(t_array)
    type(U), intent(in) :: u_array(:)
    type(V), intent(in) :: v_array(:)
    type(T) t_array(size(u_array))
    integer i

    do i=1,size(t_array)
      t_array(i) = f(u_array(i), v_array(i))
    end do

   !t_array(5) = 5                            ! compiler detected error
   !t_array(6) = f( v_array(6), u_array(6) )  ! compiler detected error
   !t_array(7) = f( v_array(7), v_array(7) )  ! compiler detected error
   !t_array(8) = u_array(8) + v_array(8)      ! compiler detected error
    t_array(9) = f( u_array(6), v_array(8) )

  end function

end module
