module either_m
    implicit none
    private
    public :: either_t

    generic :: E
    end generic

    generic :: A
    end generic

    type :: either_t<E, A>
        private
        logical :: was_ok_
        type(<E>) :: error_
        type(<A>) :: answer_
    contains
        private
        procedure, public :: was_ok
        procedure, public :: error
        procedure, public :: answer
    end type

    interface either_t
        module procedure from_error
        module procedure from_answer
    end interface
contains
    pure function from_error<E, A>(error) result(either)
        type(<E>), intent(in) :: error
        type(either_t<E, A>) :: either

        either%was_ok_ = .false.
        either%error_ = error
    end function

    pure function from_answer<E, A>(answer) result(either)
        type(<A>), intent(in) :: answer
        type(either_t<E, A>) :: either

        either%was_ok_ = .true.
        either%answer_ = answer
    end function

    pure function was_ok<E, A>(self)
        class(either_t<E, A>), intent(in) :: self
        logical :: was_ok

        was_ok = self%was_ok_
    end function

    pure function error<E, A>(self)
        class(either_t<E, A>), intent(in) :: self
        type(<E>) :: error

        if (self%was_ok()) error stop "attempted to get error when answer was present"
        error = self%error_
    end function

    pure function answer<E, A>(self)
        class(either_t<E, A>), intent(in) :: self
        type(<A>) :: answer

        if (.not. self%was_ok()) error stop "attempted to get answer when error was present"
        answer = self%answer_
    end function
end module

module something_error_prone_m
    use either_m, only: either_t
    use error_m, only: error_t ! omitted for brevity

    implicit none
    private
    public :: do_something_error_prone
contains
    pure function do_something_error_prone(x, y) result(either)
        integer, intent(in) :: x, y
        type(either_t<error_t, integer>) :: either

        if (y == 0) then
            either = either_t<error_t, integer>(error_t("tried to divide by zero"))
        else
            either = either_t<error_t, integer>(x / y)
        end if
    end function
end module

program main
    use do_something_error_prone_m, only: do_something_error_prone

    implicit none

    associate(either => do_something_error_prone(1, 0))
        if (either%was_ok()) then
            associate(answer => either%answer())
                ! now safely do something with the answer
                print *, answer + 1
            end associate
        else
            associate(error => either%error())
                ! figure out what to do with the error
                print *, error%to_string()
            end associate
        end if
    end associate
end program
