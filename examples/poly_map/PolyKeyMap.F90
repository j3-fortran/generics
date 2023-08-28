module PolyKeyMap_m
   use oper_m
   use Pair_w
   use Map_m
   implicit none

   template PolyKeyMap_tmpl(K, T, less)
      class, deferred :: K ! key
      type, deferred :: T ! value
      requires PolyOper(K, less)

      type :: KeyWrap
         class(K), allocatable :: item
      end type KeyWrap

      instantiate Pair_tmpl(KeyWrap, T)
      instantiate Map_tmpl(KeyWrap, T, wrap_less), only: MapInner => Map

      type :: Map
         type(MapInner) :: inner
      contains
         procedure :: insert
         procedure :: get
      end type Map
         

   end template

contains


   subroutine insert(this, key, value)
      class(Map), intent(inout) :: this
      class(K), intent(in) :: key
      type(T), intent(in) :: value

      type(KeyWrap) :: kw

      kw = KeyWrap(key)
      call this%inner%insert(kw, value)
   end subroutine insert


   function get(this, key) result(value)
      type(T), pointer :: value
      class(Map), target, intent(inout) :: this
      class(K), intent(in) :: key

      type(KeyWrap) :: kw

      kw = KeyWrap(key)
      value => this%inner%get(kw)

   end function get


   logical function wrap_less(wx, wy)
      type(KeyWrap), intent(in) :: wx, wy
      pair_less = less(wx%key, wy%key)
   end function wrap_less

end module Map_m
