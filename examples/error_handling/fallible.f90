module fallible_m
    implicit none

    template fallible_tmpl(E, T, combine_errs)
        private
        public :: fallible_t

        requires bin_op(E, combine_errs)
        type T; end type

        type :: fallible_t
            private
            logical :: is_err_
            type(E) :: error_
            type(T) :: answer_
        contains
            procedure :: failed
            procedure :: error
            procedure :: answer
        end type

        interface fallible_t
            module procedure from_error
            module procedure from_answer
        end interface
    contains
        elemental function from_error(error) result(fallible)
            type(E), intent(in) :: error
            type(fallible_t) :: fallible

            fallible%error_ = error
            fallible%is_err_ = .true.
        end function

        elemental function from_answer(answer) result(fallible)
            type(T), intent(in) :: answer
            type(fallible_t) :: fallible

            fallible%answer_ = answer
            fallible%is_err_ = .false.
        end function

        elemental function failed(self)
            class(fallible_t), intent(in) :: self
            logical :: failed

            failed = self%is_err_
        end function

        elemental function error(self)
            class(fallible_t), intent(in) :: self
            type(E) :: error

            if (self%is_err_) then
                error = self%error_
            else
                error stop "attempted to access error from answer"
            end if
        end function

        elemental function answer(self)
            class(fallible_t), intent(in) :: self
            type(T) :: answer

            if (self%is_err_) then
                error stop "attempted to access error from answer"
            else
                answer = self%answer_
            end if
        end function
    end template

    template and_then_2_tmpl(E, T, U, V, combine_errs)
        private
        public :: and_then

        requires bin_op(E, combine_errs)
        type T; end type
        type U; end type
        type V; end type

        interface and_then
            module procedure and_then_
        end interface
    contains
        function and_then_(f, maybe_a, maybe_b) result(maybe_answer)
            instantiate fallible_tmpl(E, T, combine_errs), fallible_t_t => fallible_t
            instantiate fallible_tmpl(E, U, combine_errs), fallible_t_u => fallible_t
            instantiate fallible_tmpl(E, V, combine_errs), fallible_t_v => fallible_t
            interface
                function f_i(a, b) result(maybe_answer)
                    type(T), intent(in) :: a
                    type(U), intent(in) :: b
                    type(fallible_t_v) :: maybe_answer
                end function
            end interface
            procedure(f_i) :: f
            type(fallible_t_t), intent(in) :: maybe_a
            type(fallible_t_u), intent(in) :: maybe_b
            type(fallible_t_v) :: maybe_answer

            if (maybe_a%failed() .or. maybe_b%failed()) then
                if (maybe_a%failed()) then
                    if (maybe_b%failed()) then
                        maybe_answer = fallible_t_v(combine_errs( &
                                maybe_a%error(), maybe_b%error()))
                    else
                        maybe_answer = fallible_t_v(maybe_a%error())
                    end if
                else
                    maybe_answer = fallible_t_v(maybe_b%error())
                end if
            else
                maybe_answer = f(maybe_a%answer(), maybe_b%answer())
            end if
        end function
    end template
end module

program example
    use error_m, only: error_t, combine_errors
    use fallible_m, only: fallible_tmpl, and_then_2_tmpl
    use string_m, only: string_t
    use thing_m, only: thing_t

    instantiate and_then_2_tmpl(error_t, integer, string_t, thing_t, combine_errors)

    associate(maybe => &
            and_then( & ! This forwards the error along
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
        instantiate fallible_tmpl(error_t, integer)
        integer, intent(in) :: numerator, denomenator
        type(fallible_t) :: maybe_quotient

        if (denomenator == 0) then
            maybe_quotient = fallible_t(error_t("tried to divide by zero"))
        else
            maybe_quotient = fallible_t(numerator / denomenator)
        end if
    end function

    function repeat_with_limit(string, repetitions, limit) result(maybe_string)
        instantiate fallible_tmpl(error_t, string_t)
        type(string_t), intent(in) :: string
        integer, intent(in) :: repetitions, limit
        type(fallible_t) :: maybe_string

        if (len(string) * repetitions > limit) then
            maybe_string = fallible_(error_t("tried to make string too long"))
        else
            maybe_string = fallible_t(repeat(string, repetitions))
        end if
    end function

    function something_else_error_prone(num, string) result(maybe_thing)
        instantiate fallible_tmpl(error_t, thing_t)
        integer, intent(in) :: num
        type(string_t), intent(in) :: string
        type(fallible_t) :: maybe_thing

        if (invalid_input(num, string)) then
            maybe_thing = fallible_t(error_t("invalid for some reason"))
        else
            maybe_thing = fallible_t(thing_t(...))
        end if
    end function
end program
