module fallible_m
    use category_theory_concepts, only: semigroup

    type :: fallible_t<err, res>
        requires :: semigroup(err)
        private
        logical :: is_err_
        type(err) :: error_
        type(res) :: answer_
    contains
        procedure :: failed
        procedure :: error
        procedure :: answer
    end type

    interface fallible_t
        module procedure from_error
        module procedure from_answer
    end interface

    interface and_then
        module procedure and_then_2_args
        ...
    end interface
contains
    function from_error<err, res>(error) result(fallible)
        type(err), intent(in) :: error
        type(fallible_t<err, res>) :: fallible

        fallible%error_ = error
        fallible%is_err_ = .true.
    end function

    function from_answer<err, res>(answer) result(fallible)
        type(res), intent(in) :: answer
        type(fallible_t<err, res>) :: fallible

        fallible%answer_ = answer
        fallible%is_err_ = .false.
    end function

    function failed(self)
        class(fallible_t<err, res>), intent(in) :: self
        logical :: failed

        failed = self%is_err_
    end function

    function error(self)
        class(fallible_t<err, res>), intent(in) :: self
        type(err) :: error

        if (self%is_err_) then
            error = self%error_
        else
            error stop "attempted to access error from answer"
        end if
    end function

    function answer(self)
        class(fallible_t<err, res>), intent(in) :: self
        type(res) :: answer

        if (self%is_err_) then
            error stop "attempted to access answer from error"
        else
            answer = self%answer_
        end if
    end function

    function and_then_2_args<err, x, y, res>(func, maybe_a, maybe_b) result(maybe_answer)
        abstract interface
            function func_i(a, b) result(maybe_answer)
                type(x), intent(in) :: a
                type(y), intent(in) :: b
                type(fallible_t<err, res>) :: maybe_answer
            end function
        end interface
        procedure(func_i) :: func
        type(fallible_t<err, x>), intent(in) :: maybe_a
        type(fallible_t<err, y>), intent(in) :: maybe_b
        type(fallible_t<err, res>) :: maybe_answer

        if (any([maybe_a%is_err_, maybe_b%is_err_])) then
            maybe_answer%is_err_ = .true.
            if (maybe_a%is_err_) then
                if (maybe_b%is_err_) then
                    maybe_answer%error_ = maybe_a%error_ // maybe_b%error_
                else
                    maybe_answer%error_ = maybe_a%error_
                end if
            else
                maybe_answer%error_ = maybe_b%error_
            end if
        else
            maybe_answer = func(maybe_a%answer_, maybe_b%answer_)
        end if
    end function
end module

program example
    use erorr_m, only: error_t
    use fallible_m, only: fallible_t, and_then
    use thing_m, only: thing_t

    associate(maybe => &
            and_then<error_t, integer, string_t, thing_t>( & ! This forwards the error along
                something_else_error_prone, & ! This shouldn't get called
                divide(42, 0), & ! This generates an error
                repeat_with_limit("Hello", 5, 50))) ! This generates a valid answer
        if (maybe%failed()) then ! This should be true
            print *, "Something went wrong:"
            print *, maybe%error() ! And we should get the error from divide
        else
            print *, "The answer is:"
            print *, maybe%answer() ! And we shouldn't get here
        end if
    end associate
contains
    function divide(numerator, denomenator) result(maybe_quotient)
        integer, intent(in) :: numerator, denomenator
        type(fallible_t<error_t, integer>) :: maybe_quotient

        if (denomenator == 0) then
            maybe_quotient = fallible_t<error_t, integer>(error_t("tried to divide by zero"))
        else
            maybe_quotient = fallible_t<error_t, integer>(numerator / denomenator)
        end if
    end function

    function repeat_with_limit(string, repetitions, limit) result(maybe_string)
        type(string_t), intent(in) :: string
        integer, intent(in) :: repetitions, limit
        type(fallible_t<error_t, string_t>) :: maybe_string

        if (len(string) * repetitions > limit) then
            maybe_string = fallible_t<error_t, string_t>(error_t("tried to make string too long"))
        else
            maybe_string = fallible_t<error_t, string_t>(repeat(string, repetitions))
        end if
    end function

    function something_else_error_prone(num, string) result(maybe_thing)
        integer, intent(in) :: num
        type(string_t), intent(in) :: string
        type(fallible_t<error_t, thing_t>) :: maybe_thing

        if (invalid_input(num, string)) then
            maybe_thing = fallible_t<error_t, thing_t>(error_t("invalid for some reason"))
        else
            maybe_thing = fallible_t<error_t, thing_t>(thing_t(...))
        end if
    end function
end program
