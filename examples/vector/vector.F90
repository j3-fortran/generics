module vector_m
   implicit none
   private

   public :: vector_tmpl

   template vector_tmpl(T)
      type :: T; end 

      public :: vector ! container
      public :: iterator
      public :: find_tmpl  ! template for finding elements
      

      type :: vector
         private
         integer :: vsize ! num valid elements
         type(T), rank(1), allocatable :: elements
      contains
         procedure :: size
         procedure :: capacity
         procedure :: set_capacity
         procedure :: push_back
         procedure :: back
         procedure :: front
         procedure :: of
         procedure, private :: grow_to
         procedure, private :: downsize

         procedure :: begin
         procedure :: end
      end type vector

      type :: iterator
         private
         integer :: current_index
         type(vector), pointer :: reference
      contains
         procedure :: equals
         procedure :: not_equals
         generic :: operator(==) => equals
         generic :: operator(!=) => not_equals
         procedure :: next
         procedure :: prev
         procedure :: of_iter
         generic :: of => of_iter
      end type iterator

      interface vector
         module procedure new_vector
      end interface vector


      ! Inner template
      template find_tmpl(equals)
         interface
            pure logical function equals(a,b)
               type(T), intent(in) :: a, b
            end function equals
         end interface
         
         public :: find

      contains

         function find(v, val) result(iter)
            type(iterator) :: iter
            type(vector), intent(in) :: v
            type(T), intent(in) :: val

            associate ( b => v%begin() )
              associate ( e => v%end() )
                iter = b
                do while (iter /= e)
                   if (equals(iter%of(), val)) return
                   call iter%next()
                end do
              end associate
            end associate
         end function find
      end template find_tmpl
      


   contains

      pure function new_vector() result(v)
         type(vector) :: v

         allocate(v%elements(0))
         v%vsize = 0
         
      end function new_vector

      pure integer function size(this)
         class(vector), intent(in) :: this
         size = this%vsize
      end function size

      pure integer function capacity(this)
         class(vector), intent(in) :: this
         capacity = size(this%elements)
      end function capacity

      pure subroutine push_back(this, val)
         class(vector), intent(inout) :: this
         type(T), intent(in) :: val

         type(T), rank(1), allocatable :: tmp

         call this%grow_to(this%vsize + 1)
         this%vsize = this%vsize + 1
         this%elements(this%vsize) = val

      end subroutine push_back
         
      function back(this) result(res)
         class(vector), target, intent(in) :: this
         type(T), pointer :: res

         res => this%elements(this%vsize)

      end function back

      function front(this) result(res)
         class(vector), target, intent(in) :: this
         type(T), pointer :: res

         res => this%elements(1)

      end function front

      subroutine pop_back(this)
         class(vector), intent(inout) :: this

         call this%downsize(this%vsize - 1)

      end subroutine pop_back


      subroutine grow_to(this, capacity)
         class(vector), intent(inout) :: this
         integer, intent(in) :: capacity

         if (capacity > MAX_CAPACITY) then
            call this%set_capacity(max(2*this%vsize, capacity)) ! gives O(n) algorithm for growing vector with push.
         endif

      end subroutine grow_to

      pure subroutine set_capacity(this, capacity)
         class(vector), intent(inout) :: this
         integer, intent(in) :: capacity

         if (capacity > 0) then
            if (.not. allocated(this%elements)) then
               allocate(this%elements(capacity))
            else
               allocate(tmp(capacity))
               tmp(1:this%vsize) = this%elements
               deallocate(this%elements)
               call move_alloc(from=tmp, to=this%elements)
            end if
         elseif (allocated(this%elements)) then ! capacity == 0
            deallocate(this%elements)
         end if

      end subroutine set_capacity

      
      function of(this, i) result(res)
         class(vector), target, intent(in) :: this
         integer, intent(in) :: i
         type(T), pointer :: res

         res => this%elements(i)%item

      end function of

      subroutine downsize(this, newsize)
         class(vector), intent(inout) :: this
         integer, intent(in) :: newsize  ! assumes newsize<=size()
         integer :: i

         this%vsize=newsize
         return
      end subroutine downsize


      function begin(this) result(iter)
         type (iterator) :: iter
         class (vector), target, intent(in) :: this
         
         iter%current_index = 1
         
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function begin
      
      
      function end(this) result(iter)
         type (iterator) :: iter
         class (vector), target, intent(in) :: this
         
         iter%current_index = this%size() + 1 ! past the end
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
      end function end

!!!! Iterator methods

      pure logical function equals(this, other)
         class(iteartor), intent(in) :: this
         type(iterator), intent(in) :: other
         equals = this%current_index == other%current_index
      end function equals

      pure logical function not_equals(this, other)
         class(iterator), intent(in) :: this
         type(iterator), intent(in) :: other
         not_equals = .not. (this == other)

      end function not_equals

      subroutine next(this)
         class(iterator), intent(inout) :: this
         this%current_index = this%current_index + 1
      end subroutine next

      subroutine prev(this)
         class(iterator), intent(inout) :: this
         this%current_index = this%current_index - 1
      end subroutine prev


      function of_iter(this) result(res)
         class(iterator), intent(in) :: this
         type(T), pointer :: res
         res => this%reference%of(this%current_index)
      end function of_iter
      
   end template vector_tmpl


   
end module vector_m
