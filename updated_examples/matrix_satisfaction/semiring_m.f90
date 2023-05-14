module semiring_m
    !! A semiring is a type that is a monoid with two separate operations and zero values
    !! For example integer is a monoid with + and 0, or * and 1.
    use monoid_m, only: monoid

    implicit none
    private
    public :: semiring

    requirement semiring(md1,md2)
        ! Donev: A bit verbose here
        satisfaction :: sg1=>semigroup(T,plus)
        satisfaction :: sg2=>semigroup(sg1%T,mult) ! Must be the same type T!
        satisfaction :: md1=>monoid(sg1, zero)
        satisfaction :: md2=>monoid(sg2, one)
    end requirement
end module
