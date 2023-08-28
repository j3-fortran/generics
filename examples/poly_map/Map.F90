module Map_m
   use oper_m
   use Pair_m
   use BinaryTree_m
   implicit none

   template map_tmpl(K, T, less)
      type, deferred :: K ! key
      type, deferred :: T ! value
      requires oper(K, less)

      instantiate Pair_tmpl(K, T)
      instantiate BinaryTree_tmpl(Pair, pair_less)

      type :: Map
         type(BinaryTree) :: tree
      contains
         procedure :: insert
         procedure :: get
      end type Map

   end template

contains


   subroutine insert(this, key, value)
      class(Map), intent(inout) :: this
      type(K), intent(in) :: key
      type(T), intent(in) :: value

      type(Pair) :: p

      p = Pair(key, value)

      call this%tree%insert(p)
   end subroutine insert


   function get(this, key) result(value)
      type(T), pointer :: value
      class(Map), target, intent(inout) :: this
      type(K), intent(in) :: key

      type(Pair) :: p1
      type(Pair), pointer :: p2

      p1%key = key ! do not care about value

      p2 => this%tree%find(p1)
      value => p2%value

   end subroutine insert


   logical function pair_less(px, py)
      type(Pair), intent(in) :: px, py
      pair_less = less(px%key, py%key)
   end function pair_less

end module Map_m
