module PolyPolyMap_m
   use oper_m
   use Map_m
   use Pair_m
   implicit none

   template PolyPoly_tmpl(K, T, less)
      type, deferred :: K ! key
      class, deferred :: T ! value
      requires PolyOper(K, less)

      type :: ValueWrap
         class(T), allocatable :: item
      end type ValueWrap

      type :: KeyWrap
         class(K), allocatable :: item
      end type KeyWrap

      instantiate Pair_tmpl(KeyWrap, ValueWrap)
      instantiate Map_tmpl(KeyWrap, ValueWrap, wrap_less), only: MapInner => Map

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
      class(T), intent(in) :: value

      type(ValueWrap) :: tw
      type(KeyWrap) :: kw

      tw = ValueWrap(value)
      kw = KeyWrap(key)
      call this%inner%insert(kw, tw)

   end subroutine insert


   function get(this, key) result(value)
      class(T), pointer :: value
      class(Map), target, intent(inout) :: this
      class(K), intent(in) :: key

      type(ValueWrap), pointer :: tw
      type(KeyWrap) :: kw

      kw = KeyWrap(kw)
      tw  => this%inner%get(kw))
      value => tw%item

   end function get


   logical function wrap_less(wx, wy)
      type(KeyWrap), intent(in) :: wx, wy
      pair_less = less(wx%key, wy%key)
   end function wrap_less


end module Map_m
