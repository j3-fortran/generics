program main
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
