module semiring_m
    !! A semiring is a type that is a monoid with two separate operations and zero values
    !! For example integer is a monoid with + and 0, or * and 1.
    use monoid_m, only: minimal_monoid

    implicit none
    private
    public :: semiring

    requirement semiring(T, plus, zero, mult, one)
        requires minimal_monoid(T, plus, zero)
        requires minimal_monoid(T, mult, one)
    end requirement
end module