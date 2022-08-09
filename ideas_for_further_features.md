1. Rank agnosticism (stake a claim that this is generics territory)

   - rank agnostic DO CONCURRENT
     integer :: idx(n)
        DO CONCURRENT (integer :: idx=@shape(arr))
           ...
        END DO
   - Mathematics of Arrays
     - separate working group

   - relax constraints for assumed rank
     - intrinsic assignment
     - pass to other procedures 
     - @notation
   - Deferred rank?
     ```
     type :: wrapper
        class(*), allocatable, rank(..) :: item
     end type
     type :: container
        type(wrapper), allocatable :: items(:)
     end type
     
     subroutine foo(arr)
       real, dimension(..), intent(in) :: arr
        
       real, dimension(..), allocatable :: tmp
     end subroutine
     ```
     - SWAP
     ```
     subroutine swap(a, b)
     type(T), intent(inout), dimension(..) :: a, b
     type(T) :: tmp(@shape(a))
     
        tmp = a
        a = b
        b = tmp
        
     end subroutine
     ```
2. Allow other types as constants
   - currently only allow logical, integer, character
    
3. "Extensible" types
   - enable containers with polymorphic items
   
4. Ecosystem of wrappers
   - e.g. enable a container of allocatables or a container of pointers
     ```
     type :: wrapper
        class(T), allocatable :: item
     end type
     type :: container 
        type(wrapper), allocatable :: items(:)
     end 
     ```
     instantiate  VEC_TMPL(wrapper)
     
 5. Iterators for containers (ranges)
    ```
    type(Container) :: c

    type(Iterator) :: iter
    do (iter:c)
       <do something with iter>
    end do
    type :: container
    contains
       procedure :: begin
       generic :: iterator(begin) => begin
       procedure :: end
       generic :: iterator(end) => end
       generic :: iterator(elemental) => elemental
    end 
    ```
    
    
6. Elemental for containers ...
   ```
   type(Container_of_int) :: c1, c2
   integer :: i, i_arr(:)
   call elemental(c1, c1)
   call elemental(c1, c2)
   call elemental(c1, i)
   call elemental(c1, i_arr)
   
   call c1%elemental(sub, c2)
   ```
