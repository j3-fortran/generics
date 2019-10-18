module map_function_specialization  !  <V>
  use map_function ! (<REAL,V,REAL>)
  implicit none

#define requirement

  requirement type :: V
  end type

  interface map_f
     module procedure map_f_real_v_real
  end interface map_f
  
  requirement interface 

    module function f(u_, v_) result(t_)
      real, intent(in) :: u_
      type(V), intent(in) :: v_
      real t_
    end function

  end interface

contains

  function map_f_real_v_real( u_array, v_array) result(t_array)
    real, intent(in) :: u_array(:)
    type(V), intent(in) :: v_array(:)
    real t_array(size(u_array))
    integer i

    do i=1,size(t_array)
      t_array(i) = f(u_array(i), v_array(i))
    end do

   t_array(5) = 5                            ! compiler detected error
   !t_array(6) = f( v_array(6), u_array(6) )  ! compiler detected error
   !t_array(7) = f( v_array(7), v_array(7) )  ! compiler detected error
   !t_array(8) = u_array(8) + v_array(8)      ! compiler detected error
    t_array(9) = f( u_array(6), v_array(8) )
    t_array(1) = u_array(6)

  end function map_f_real_v_real

end module
