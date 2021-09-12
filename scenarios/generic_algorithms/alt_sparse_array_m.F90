! This attempts to do the same thing as sparse_array_m.F90 but just using
! parameterized modules.  I.e., without templated types/procedures.

! First we have a parameterized module that is only about the
! implementation of the sparse array itself.  This isolates the
! container from the additional type parameters neeed by various
! high-level operations (map, filter, ...)

module sparse_array_m(T)
   type :: T
   end type T
   
   concept :: copyable(T)
     assignment: T = T
   end concept

   type :: sparse_array_t
      requirements :: copyable(T)
      private
      type(T), allocatable :: elements(:)
      integer, allocatable :: indices(:)
   contains
      procedure :: insert_at
      procedure :: get
   end type sparse_array_t

contains

    subroutine insert_at(self, index, element)
        requirements :: copyable
        class(sparse_array_t), intent(inout) :: self
        integer, intent(in) :: index
        type(T), intent(in) :: element

        ...
    end subroutine

    function get(self, index) result(element)
        requirements :: copyable
        class(sparse_array_t), intent(in) :: self
        integer, intent(in) :: index
        type(T) :: element

        ...
    end function
 end module sparse_array_m

 ! Next we place each high-level procedure in a separate module to
 ! provide the greatest possible control over instantiations.  A
 ! variant would be to put all in one module, but then make extensive
 ! use of "ONLY".+

module sparse_array_map_m(T, U)
   implicit none
   private
   public :: map

    type :: T
    end type T
    type :: U
    end type U

contains

   function map(transformation, xs) result(ys)
      use sparse_array_m(T), only: sparse_T_array_t => sparse_array_t
      use sparse_array_m(U), only: sparse_U_array_t => sparse_array_t
      requirements :: copyable(T), copyable(U)
      interface
         pure function transformation_i(x) result(y)
            type(T), intent(in) :: x
            type(U) :: y
         end function transformation_i
      end interface
      procedure(transformation_i) :: transformation
      type(sparse_T_array_t), intent(in) :: xs
      type(sparse_U_array_t) :: ys

      ! PRIVACY violation ....
      ys%indices = xs%indices
      ys%elements = etransformation(xs%elements)
    contains
       elemental function etransformation(x) result(y)
          type(T), intent(in) :: x
          type(U) :: y
          
          y = transformation(x)
       end function etransformation
    end function
 end module sparse_array_map_m

 module sparse_array_filter_m(T)
    type :: T
    end type T
    use sparse_array_m(T)

contains

    function filter<T>(array, predicate) result(filtered)
        requirements :: copyable(T)
        interface
           pure function predicate_i(element)
              type(T), intent(in) :: element
              logical :: predicate_i
           end function predicate_i
        end interface
        type(sparse_array_t), intent(in) :: array
        procedure(predicate_i) :: predicate
        type(sparse_array_t) :: filtered

        ! PRIVACY violation
        associate(matches => epredicate(array%elements))
          filtered%indices = pack(array%indices, matches)
          filtered%elements = pack(array%elements, matches)
        end associate
     contains
        elemental function epredicate(element)
            type(T), intent(in) :: element
            logical :: epredicate

            epredicate = predicate(element)
        end function
    end function
 end module sparse_array_filter_m


 module sparse_array_reduce_m(T,U)
    
   implicit none
   private
   public :: map

    type :: T
    end type T
    type :: U
    end type U

 contains
    function reduce(array, accumulator, initial) result(combined)
        requirements :: copyable(T), copyable(U)
        interface
           function accumulator_i(x, y) result(z)
              type(U), intent(in) :: y
              type(T), intent(in) :: x
              type(U) :: z
           end function accumulator_i
        end interface
        type(sparse_array_t), intent(in) :: array
        procedure(accumulator_i) :: accumulator
        type(U), intent(in) :: initial
        type(U) :: combined
        
        integer :: i
        
        combined = accumulator(intial, self%elements(1))
        do i = 2, size(self%elements)
           combined = accumulator(combined, self%elements(i))
        end do
    end function

end module


program example_usage
    use iso_varying_string, only: varying_string, assignment(=), operator(//), len
    use sparse_array_m(integer), only: int_sparse_array => sparse_array_t
    use sparse_array_m(varying_string), only: string_sparse_array => sparse_array_t

    use sparse_arary_map_m(integer, varying_string), only: map
    use sparse_arary_filter_m(varying_string), only: filter
    use sparse_arary_reduce_m(varying_string, varying_string), only: reduce

    implicit none

    type(int_sparse_array_t) :: at_squares
    type(string_sparse_array_t) :: words_at_squares
    type(string_sparse_array_t) :: squares_with_small_root_words

    call at_squares%insert_at(1, 1)
    call at_squares%insert_at(4, 2)
    call at_squares%insert_at(9, 3)
    call at_squares%insert_at(16, 4)
    call at_squares%insert_at(25, 5)
    call at_squares%insert_at(36, 6)

    words_at_squares = map(at_squares, to_word)
    squares_with_small_root_words = filter(words_at_squares, shorter_than_four_characters)

    print *, reduce(squares_with_small_root_words, count_letters, 0)) ! should be 9
    ! could also write as "one-liner" like
    ! print *, reduce( &
    !                 filter( &
    !                       map(at_squares, to_word), &
    !                       shorter_than_four_characters), &
    !                 count_letters, &
    !                 0)
contains
    pure function to_word(number) result(word)
        integer, intent(in) :: number
        type(varying_string) :: word

        select case (number)
        case (1)
            word = "one"
        case (2)
            word = "two"
        case (3)
            word = "three"
        case (4)
            word = "four"
        case (5)
            word = "five"
        case (6)
            word = "six"
        case default
            word = "many"
        end select
    end function

    pure function shorter_than_four_characters(string)
        type(varying_string), intent(in) :: string
        logical :: shorter_than_four_characters

        shorter_than_four_characters = len(string) < 4
    end function

    pure function count_letters(x, y) result(z)
        integer, intent(in) :: x
        type(varying_string), intent(in) :: y
        integer :: z

        z = x + len(y)
    end function
end program
