module sparse_array_m
    implicit none
    private
    public :: sparse_array_tmpl, map_tmpl, filter_tmpl, reduce_tmpl

    template sparse_array_tmpl(T)
        private
        public :: sparse_array

        type :: T; end type

        type :: sparse_array
            private
            type(T), allocatable :: elements(:)
            integer, allocatable :: indices(:)
        contains
            procedure :: insert_at
            procedure :: get
        end type
    contains
        subroutine insert_at(self, index, element)
            class(sparse_array), intent(inout) :: self
            integer, intent(in) :: index
            type(T), intent(in) :: element

            ...
        end subroutine

        function get(self, index) result(element)
            class(sparse_array), intent(in) :: self
            integer, intent(in) :: index
            type(T) :: element

            ...
        end function
    end template

    template map_tmpl(T, U)
        private
        public :: map
        interface map
            module procedure map_
        end interface
        type :: T; end type
        type :: U; end type
        instantiate sparse_array_tmpl(T), only: sparse_array_t => sparse_array
        instantiate sparse_array_tmpl(U), only: sparse_array_u => sparse_array
    contains
        function map_(transformation, xs) result(ys)
            interface
                pure function transformation_i(x) result(y)
                    type(T), intent(in) :: x
                    type(U) :: y
                end function
            end interface
            procedure(transformation_i) :: transformation
            type(sparse_array_t), intent(in) :: xs
            type(sparse_array_u) :: ys

            integer :: i

            ys%indeces = xs%indeces
            ys%elements = [(transformation(xs%elements(i)), i = 1, size(xs%elements))]
        end function
    end template

    template filter_tmpl(T)
        private
        public :: filter
        interface filter
            module procedure filter_
        end interface
        type :: T; end type
        instantiate sparse_array_tmpl(T)
    contains
        function filter_(array, predicate) result(filtered)
            interface
                pure function predicate_i(element)
                    type(T), intent(in) :: element
                    logical :: predicate_i
                end function
            end interface
            type(sparse_array), intent(in) :: array
            procedure(predicate_i) :: predicate
            type(sparse_array) :: filtered

            integer :: i

            associate(matches => [(predicate(array%elements(i)), i = 1, size(array%elements))])
                filtered%indices = pack(array%indices, matches)
                filtered%elements = pack(array%elements, matches)
            end associate
        end function
    end template

    template reduce_tmpl(T, U)
        private
        public :: reduce
        interface reduce
            module procedure reduce_
        end interface
        type :: T; end type
        type :: U; end type
        instantiate sparse_array_tmpl(T)
    contains
        function reduce_(array, accumulator, initial) result(combined)
            interface
                function accumulator_i(x, y) result(z)
                    type(U), intent(in) :: x
                    type(T), intent(in) :: y
                    type(U) :: z
                end function
            end interface
            type(sparse_array), intent(in) :: array
            procedure(accumulator_i) :: accumulator
            type(U), intent(in) :: initial
            type(U) :: combined

            integer :: i

            combined = accumulator(initial, self%elements(1))
            do i = 2, size(self%elements)
                combined = accumulator(combined, self%elements(i))
            end do
        end function
    end template
end module

program example_usage
    use iso_varying_string, only: varying_string, assignment(=), len
    use sparse_array_m, only: sparse_array_tmpl, map_tmpl, filter_tmpl, reduce_tmpl

    implicit none

    instantiate sparse_array_tmpl(integer), only: sparse_array_int => sparse_array
    instantiate sparse_array_tmpl(varying_string), only: sparse_array_string => sparse_array
    instantiate map_tmpl(integer, varying_string)
    instantiate filter_tmpl(varying_string)
    instantiate reduce_tmpl(varying_string, integer)

    type(sparse_array_int) :: at_squares
    type(sparse_array_string) :: words_at_squares
    type(sparse_array_string) :: squares_with_small_root_words

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
    !         filter( &
    !                 map(at_squares, to_word), &
    !                 shorter_than_four_characters), &
    !         count_letters, &
    !         0)
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
