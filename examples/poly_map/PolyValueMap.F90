module PolyValueMap_m
   use oper_m
   use Pair_tmpl
   use Map_m
   implicit none

   template PolyValueap_tmpl(K, T, less)
      type, deferred :: K ! key
      class, deferred :: T ! value
      REQUIRES Oper(K, less)

      type :: ValueWrap
         class(T), allocatable :: item
      end type ValueWrap

      instantiate Pair_tmpl(K, ValueWrap)
      instantiate Map_tmpl(K, ValueWrap, less), only: MapInner => Map

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
      type(K), intent(in) :: key
      class(T), intent(in) :: value

      type(ValueWrap) :: tw

      tw = ValueWrap(value)
      call this%inner%insert(key, tw)

   end subroutine insert


   function get(this, key) result(value)
      type(T), pointer :: value
      class(Map), target, intent(inout) :: this
      class(K), intent(in) :: key

      type(ValueWrap), pointer :: tw

      tw  => this%inner%get(key)
      value => tw%item
   end function get


end module Map_m
