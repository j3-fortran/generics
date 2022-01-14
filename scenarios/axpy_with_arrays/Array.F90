module Array_m

   public :: Array_t ! template

   TEMPLATE Array_t(T, rank)
      type :: t
      end type t
      integer :: rank

      type :: Array
         type(T), rank(rank), allocatable :: elements
      end type Array
      
      interface assignment(=)
         module procedure assign_from_raw
         module procedure assign_to_raw
      end interface assignment(=)

   contains

      subroutine assign_from_raw(x,y)
         type(Array), intent(out) :: x
         type(T), rank(rank), intent(in) :: y
         x%elements = y
      end subroutine assign_from_raw

      subroutine assign_to_raw(x,y)
         type(T), rank(rank), allocatable, intent(out) :: x
         type(Array), intent(in) :: x
         x = y%elements
      end subroutine assign_to_raw

   END TEMPLATE

   ! Q: How do we specify that an proc must be elemental?
   template map_t(T, rank_T, U, rank_U, V, rank_V, W, F)
      type :: T
      end type T
      type :: U
      end type U
      type :: V
      end type V
      type :: W
      end type W
      integer :: rank_T, rank_U, rank_V
      procedure :: F
 
      REQUIRES :: scalar_op(T, U, V, F)
      REQUIRES :: assignable(V, W)

   contains

      elemental subroutine map(a, b) result(c)
         INSTANTIATE Array_t(T, rank_T), only: Array_of_T
         INSTANTIATE Array_t(U, rank_U), only: Array_of_U
         INSTANTIATE Array_t(W, rank_V), only: Array_of_W
         
         type(Array_of_T), intent(in) :: a
         type(Array_of_U), intent(in) :: b
         type(Array_of_W) :: c

         ! exploit that F must be elemental
         c%elements = F(a%elements, b%elements)
      end subroutine map

   END TEMPLATE

end module Array_m


