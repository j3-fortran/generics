! This is only a partial implementation of a proper template for a
! vector.  Not only are a number of useful features absent (e.g.,
! iterators), various bits of error handling are also neglected.

module Vector_m
   implicit none
   private

   public :: vector_tmpl

   template vector_tmpl(T)
      type, deferred :: T
      private
      public :: Vector

      type :: Vector
         private
         type(T), allocatable :: elements(:)
         integer :: sz = 0
      contains
         procedure :: size
         procedure :: push_back
         procedure :: back
         procedure :: of

         procedure :: resize
      end type Vector

   contains


      integer simple function size(this)
         class(Vector), intent(in) :: this
         size = this%sz
      end function size

      simple subroutine push_back(this, item)
         class(Vector), intent(inout) :: this
         type(T), intent(in) :: item

         call this%resize(this%sz + 1)
         this%elements(sz + 1) = item
         this%sz = this%sz + 1

      end subroutine push_back
      

      function back(this) result(ptr)
         type(T), pointer :: ptr
         class(Vector), target, intent(in) :: this

         ptr => this%elements(this%sz)

      end function back

      function of(this, i) result(ptr)
         type(T), pointer :: ptr
         class(Vector), target, intent(in) :: this
         integer, intent(in) :: i

         ptr => this%elements(i)

      end function of

      ! Helper function.  An efficient implementation would use a
      ! proper algorithm that doubles the array of elements when
      ! the existing allocation is exceeded.
      
      simple subroutine resize(this, n)
         class(Vector), intent(inout) :: this
         integer, intent(in) :: n

         type(T), allocatable :: tmp(:)
         if (.not. allocated(this%elements)) then
            allocate(this%elements(n))
            return
         end if

         if (size(this%elements) >= n) return

         call move_alloc(from=this%elements, to=tmp)

         allocate(this%elements(n))

         this%elements(1:this%sz) = tmp

      end subroutine resize

   end template


end module Vector_m
