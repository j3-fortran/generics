module unit_ring_m
    !! A unit ring is a type that is a semiring with negation or minus operations.
    use semiring_m, only: semiring

    implicit none
    private
    public :: &
            unit_ring_only_minus, &
            unit_ring_only_negate, &
            unit_ring, &
            derive_unit_ring_from_minus, &
            derive_unit_ring_from_negate

    requirement unit_ring_only_minus(T, plus, zero, mult, one, minus)
        requires semiring(T, plus, zero, mult, one)
        elemental function minus(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference
        end function
    end requirement

    requirement unit_ring_only_negate(T, plus, zero, mult, one, negate)
        requires semiring(T, plus, zero, mult, one)
        elemental function negate(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated
        end function
    end requirement

    requirement unit_ring(T, plus, zero, mult, one, minus, negate)
        requires unit_ring_only_minus(T, plus, zero, mult, one, minus)
        requires unit_ring_only_negate(T, plus, zero, mult, one, negate)
    end requirement

    template derive_unit_ring_from_minus(T, plus, zero, mult, one, minus)
        requires unit_ring_only_minus(T, plus, zero, mult, one, minus)

        private
        public :: negate

        interface negate
            template procedure negate_
        end interface
    contains
        elemental function negate_(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated

            negated = minus(zero(), x)
        end function
    end template

    template derive_unit_ring_from_negate(T, plus, zero, mult, one, negate)
        requires unit_ring_only_negate(T, plus, zero, mult, one, negate)

        private
        public :: minus

        interface minus
            template procedure minus_
        end interface
    contains
        elemental function minus_(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference

            difference = plus(x, negate(y))
        end function
    end template
end module