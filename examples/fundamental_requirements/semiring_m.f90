module semiring_m
    use monoid_m, only: minimal_monoid

    implicit none
    private
    public :: semiring

    requirement semiring(T, plus, zero, mult, one)
        requires minimal_monoid(T, plus, zero)
        requires minimal_monoid(T, mult, one)
    end requirement
end module