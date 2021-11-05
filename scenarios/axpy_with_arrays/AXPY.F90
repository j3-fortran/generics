module AXPY_m

   public :: axpy_t ! template
   public :: scalar_op ! restriction
   public :: assignable ! restriction
   
   RESTRICTION elemental_op(T, U, V, F)
     elemental function F(x,y) result(z)
       type(T), intent(in) :: x
       type(U), intent(in) :: y
       type(V) :: z
     end function F
   END RESTRICTION

   RESTRICTION assignable(T, U)
      elemental assignment(=)(x,y)
      type(T), intent(out) :: x 
      type(U), intent(in) :: y
      end assignment
   END RESTRICTION

   template axpy_t(T_a, T_x, T_y, T_ax, T_axpy, times, plus)

      type :: T_a
      end type T_a
      type :: T_x
      end type T_x
      type :: T_y
      end type T_y
      type :: T_ax
      end type T_ax
      type :: T_axpy
      end type T_axpy

      procedure :: times
      procedure :: plus
      
      REQUIRES elemental_op(T_a, T_x, T_ax, times)
      REQUIRES elemental_op(T_ax, T_y, T_axpy, plus)
      REQUIRES assignable(T_y, T_axpy)

   contains

      subroutine AXPY(a, x, y)
         type(T), intent(in) :: a
         type(U), intent(in) :: x
         type(V), intent(inout) :: y

         y = plus(times(a,x), y)
      end subroutine AXPY

   END TEMPLATE

end module AXPY_m
