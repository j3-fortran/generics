module sparse_array_m
    concept :: copyable(T)
      assignment: T = T
    end concept

    type :: sparse_array_t<T>
        requirements :: copyable(T)
        private
        type(T), allocatable :: elements(:)
        integer, allocatable :: indices(:)
    contains
        procedure :: insert_at
        procedure :: get
    end type
contains
    subroutine insert_at<T>(self, index, element)
        requirements :: copyable(T)
        class(sparse_array_t<T>), intent(inout) :: self
        integer, intent(in) :: index
        type(T), intent(in) :: element

        ...
    end subroutine

    function get<T>(self, index) result(element)
        requirements :: copyable(T)
        class(sparse_array_t<T>), intent(in) :: self
        integer, intent(in) :: index
        type(T) :: element

        ...
    end function

    function map<T, U>(transformation, xs) result(ys)
        requirements :: copyable(T), copyable(U)
        interface
            pure function transformation_i(x) result(y)
                type(T), intent(in) :: x
                type(U) :: y
            end function
        end interface
        procedure(transformation_i) :: transformation
        type(sparse_array_t<T>), intent(in) :: xs
        type(sparse_array_t<U>) :: ys

        ys%indices = xs%indices
        ys%elements = etransformation(xs%elements)
    contains
        elemental function etransformation(x) result(y)
            type(T), intent(in) :: x
            type(U) :: y

            y = transformation(x)
        end function
    end function

    function filter<T>(array, predicate) result(filtered)
        requirements :: copyable(T)
        interface
            pure function predicate_i(element)
                type(T), intent(in) :: element
                logical :: predicate_i
            end function
        end interface
        type(sparse_array_t<T>), intent(in) :: array
        procedure(predicate_i) :: predicate
        type(sparse_array_t<T>) :: filtered

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

    function reduce<T, U>(array, accumulator, initial) result(combined)
        requirements :: copyable(T), copyable(U)
        interface
            function accumulator_i(x, y) result(z)
                type(U), intent(in) :: y
                type(T), intent(in) :: x
                type(U) :: z
            end function
        end interface
        type(sparse_array_t<T>), intent(in) :: array
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
    use sparse_array_m, only: sparse_array_t, map, filter, reduce

    implicit none

    type(sparse_array_t<integer>) :: at_squares
    type(sparse_array_t<varying_string>) :: words_at_squares
    type(sparse_array_t<varying_string>) :: squares_with_small_root_words

    call at_squares%insert_at(1, 1)
    call at_squares%insert_at(4, 2)
    call at_squares%insert_at(9, 3)
    call at_squares%insert_at(16, 4)
    call at_squares%insert_at(25, 5)
    call at_squares%insert_at(36, 6)

    words_at_squares = map<integer, varying_string>(at_squares, to_word)
    squares_with_small_root_words = filter<varying_string>(words_at_squares, shorter_than_four_characters)
    print *, reduce<varying_string, integer>(squares_with_small_root_words, count_letters, 0)) ! should be 9
    ! could also write as "one-liner" like
    ! print *, reduce<varying_string, integer>( &
    !         filter<vayring_string>( &
    !                 map<integer, varying_string>(at_squares, to_word), &
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
