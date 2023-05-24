! DONEV: NOT FINISHED
module field_m
    !! field is a unit_ring that also has a division or inverse operation
    use unit_ring_m, only: unit_ring

    implicit none
    private
    public :: &
            field_only_division, &
            field_only_inverse, &
            field, &
            derive_field_from_division, &
            derive_field_from_inverse

    requirement field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        requires unit_ring(T, plus, zero, mult, one, minus, negate)
        elemental function divide(x, y) result(quotient)
            type(T), intent(in) :: x, y
            type(T) :: quotient
        end function
    end requirement

    requirement field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
        requires unit_ring(T, plus, zero, mult, one, minus, negate)
        elemental function invert(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse
        end function
    end requirement

    requirement field(T, plus, zero, mult, one, minus, negate, divide, invert)
        requires field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        requires field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
    end requirement

    template derive_field_from_division(T, plus, zero, mult, one, minus, negate, divide)
        requires field_only_division(T, plus, zero, mult, one, minus, negate, divide)

        private
        public :: invert

        generic :: invert => invert_
    contains
        elemental function invert_(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse

            inverse = divide(one(), x)
        end function
    end template

    template derive_field_from_inverse(T, plus, zero, mult, one, minus, negate, invert)
        requires field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)

        private
        public :: divide

        generic :: divide => divide_
    contains
        elemental function divide_(x, y) result(quotient)
            type(T), intent(in) :: x, y
            type(T) :: quotient

            quotient = mult(x, invert(y))
        end function
    end template
end module
