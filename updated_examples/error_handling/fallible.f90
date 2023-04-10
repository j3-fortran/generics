module fallible_m
    implicit none

    template fallible_tmpl(E, T)
        private
        public :: fallible

        type, deferred :: T

        type :: fallible
            private
            logical :: is_err_
            type(E) :: error_
            type(T) :: answer_
        contains
            procedure :: failed
            procedure :: error
            procedure :: answer
        end type

        interface fallible
            module procedure from_error
            module procedure from_answer
        end interface
    contains
        elemental function from_error(error)
            type(E), intent(in) :: error
            type(fallible) :: from_error

            from_error%error_ = error
            from_error%is_err_ = .true.
        end function

        elemental function from_answer(answer)
            type(T), intent(in) :: answer
            type(fallible) :: from_answer

            from_answer%answer_ = answer
            from_answer%is_err_ = .false.
        end function

        elemental function failed(self)
            class(fallible), intent(in) :: self
            logical :: failed

            failed = self%is_err_
        end function

        elemental function error(self)
            class(fallible), intent(in) :: self
            type(E) :: error

            if (self%is_err_) then
                error = self%error_
            else
                error stop "attempted to access error from answer"
            end if
        end function

        elemental function answer(self)
            class(fallible), intent(in) :: self
            type(T) :: answer

            if (self%is_err_) then
                error stop "attempted to access error from answer"
            else
                answer = self%answer_
            end if
        end function
    end template

    requirement combinable(T, bin_op)
        type, deferred :: T
        elemental function bin_op(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end requirement

    template and_then_2_tmpl(E, T, U, V, combine_errs)
        private
        public :: and_then

        requires combinable(E, combine_errs)
        type, deferred :: T
        type, deferred :: U
        type, deferred :: V

        interface and_then
            procedure and_then_
        end interface
    contains
        function and_then_(f, maybe_a, maybe_b) result(maybe_answer)
            instantiate fallible_tmpl(E, T, combine_errs), fallible_t => fallible
            instantiate fallible_tmpl(E, U, combine_errs), fallible_u => fallible
            instantiate fallible_tmpl(E, V, combine_errs), fallible_v => fallible
            abstract interface
                function f_i(a, b) result(maybe_answer)
                    import :: T, U, fallible_v
                    implicit none
                    type(T), intent(in) :: a
                    type(U), intent(in) :: b
                    type(fallible_v) :: maybe_answer
                end function
            end interface
            procedure(f_i) :: f
            type(fallible_t), intent(in) :: maybe_a
            type(fallible_u), intent(in) :: maybe_b
            type(fallible_v) :: maybe_answer

            if (maybe_a%failed()) then
                if (maybe_b%failed()) then
                    maybe_answer = fallible_v(combine_errs( &
                            maybe_a%error(), maybe_b%error()))
                else
                    maybe_answer = fallible_v(maybe_a%error())
                end if
            else
                if (maybe_b%failed()) then
                    maybe_answer = fallible_v(maybe_b%error())
                else
                    maybe_answer = f(maybe_a%answer(), maybe_b%answer())
                end if
            end if
        end function
    end template
end module

program example
    use error_m, only: error, combine_errors
    use fallible_m, only: fallible_tmpl, and_then_2_tmpl
    use string_m, only: string
    use thing_m, only: thing

    instantiate and_then_2_tmpl(error, integer, string, thing, combine_errors)

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
        instantiate fallible_tmpl(error, integer)
        integer, intent(in) :: numerator, denomenator
        type(fallible) :: maybe_quotient

        if (denomenator == 0) then
            maybe_quotient = fallible(error("tried to divide by zero"))
        else
            maybe_quotient = fallible(numerator / denomenator)
        end if
    end function

    function repeat_with_limit(str, repetitions, limit) result(maybe_string)
        instantiate fallible_tmpl(error, string)
        type(string_t), intent(in) :: str
        integer, intent(in) :: repetitions, limit
        type(fallible) :: maybe_string

        if (len(str) * repetitions > limit) then
            maybe_string = fallible(error("tried to make string too long"))
        else
            maybe_string = fallible(repeat(str, repetitions))
        end if
    end function

    function something_else_error_prone(num, str) result(maybe_thing)
        instantiate fallible_tmpl(error, thing)
        integer, intent(in) :: num
        type(string), intent(in) :: str
        type(fallible) :: maybe_thing

        if (invalid_input(num, string)) then
            maybe_thing = fallible(error("invalid for some reason"))
        else
            maybe_thing = fallible(thing(...))
        end if
    end function
end program
