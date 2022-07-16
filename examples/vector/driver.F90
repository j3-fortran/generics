program main
   use vector_tmpl
   implicit none


   type :: MyType
      integer :: i
      character(:), allocatable :: name
   end type MyType


   instantiate vector_tmpl(MyType)
   instantiate find_tmpl(equals)

   type(vector) :: v
   type(iter) :: iter

   call v%push_back(MyType(3, 'cat'))
   call v%push_back(MyType(4, 'CAT'))
   call v%push_back(MyType(5, 'dog'))
   call v%push_back(MyType(5, 'DOG'))

   iter = find(v, MyType(5, 'dog'))

contains

   pure logical function equals(a, b)
      type(MyType), intent(in) :: a, b
      equals = (a%i == b%i) .and. (lowercase(a%name) == lowercase(b%name))
   end function equal

   pure function lowercase(s)
      character(*), intent(in) :: s
      integer :: i
      
      allocate(character(len=len(s)) :: lowercase)

      do i = 1, len(s)
         j = iachar(s(i:i))
         if ( j > iachar("A") .and. j < iachar("Z")) then ! is capital
            lowercase(i:i) = achar(j-32)
         else
            lowecase(i:i) = s(i:i)
         end if
      end do
            
   end function lowercase
   
end program main
