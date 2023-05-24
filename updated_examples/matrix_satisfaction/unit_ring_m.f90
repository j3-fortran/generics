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

    ! Donev: For improved clarity/readbility,
    ! it may be better to rename this into unit_ring_only_oneop
    ! and rename minus->op since later we use this with minus->negate
    requirement unit_ring_only_minus(sr, minus)
        ! Donev: This is now where things get hairy for my Ada-like proposal
        ! I require repeating this which was already in the requirement semiring
        ! The reason for this is that I do not want implicit names
        ! The code below gives explicit names to all types/procedures
        ! which can therefore not conflict with other names
        ! It is like renaming by re-declaring
        satisfaction :: sg1=>semigroup(T,plus)
        satisfaction :: sg2=>semigroup(sg1%T,mult) ! Must be the same type T!
        satisfaction :: md1=>monoid(sg1, zero)
        satisfaction :: md2=>monoid(sg2, one)        
        
        satisfaction :: sr=>semiring(md1,md2)
        elemental function minus(x, y) result(difference)
            type(sg1%T), intent(in) :: x, y
            type(sg1%T) :: difference
        end function
    end requirement

    ! Donev: ALTERNATIVE syntax just to illustrate that this is also possible:
    requirement unit_ring_only_minus_alt(T, sr, minus)
        type, deferred :: T
        satisfaction :: sg1=>semigroup(T,plus)
        satisfaction :: sg2=>semigroup(T,mult)
        satisfaction :: md1=>monoid(sg1, zero)
        satisfaction :: md2=>monoid(sg2, one)        
        
        satisfaction :: sr=>semiring(md1,md2)
        elemental function minus(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference
        end function
    end requirement

    !---------------------
    ! Donev: Observe how my new design completely avoids
    ! repeating the same code again for unit_ring_only_negate   

    requirement unit_ring(sr, minus, negate)
        ! Donev: Duplicate code:
        satisfaction :: sg1=>semigroup(T,plus)
        satisfaction :: sg2=>semigroup(sg1%T,mult) ! Must be the same type T!
        satisfaction :: md1=>monoid(sg1, zero)
        satisfaction :: md2=>monoid(sg2, one)    
        
        ! Donev: Maybe it is OK to just allow the line below on its own
        ! which is equivalent to simply cut and pasting the contents inside the
        ! requirement semiring here, so the names like md1/md2 stay the same
        ! as they were when semiring was declared
        ! It leaves me a bit worried for possible issues so here I duplicate code
        satisfaction :: sr=>semiring(md1,md2)
        ! Donev: But now this is super simple, short, and without long argument lists,
        ! so not error prone and very clear mathematically
        ! Observe the two unit_rings share the same semiring trivially
        ! This declares minus and negate and says they share the same type T
        ! Donev: This one renames minus=>negate
        satisfaction :: urm=>unit_ring_only_minus(sr, minus)
        satisfaction :: urn=>unit_ring_only_minus(sr, negate)
    end requirement

    template derive_unit_ring_from_minus(urm, minus)
        ! Donev: Here I got lazy so instead of repeating the code
        ! to declare sr I just assumed we can do it like this
        ! which "implicitly" declares sr to be a semiring with default component names
        satisfaction :: urm=>unit_ring_only_minus(sr,minus) ! Also declares minus

        private
        public :: negate

        generic :: negate => negate_
    contains
        elemental function negate_(x) result(negated)
            ! Donev: The UGLY truth is that user must get the right component
            ! Since there is ultimately only one type T here, if we have inheritance
            ! this would simply say urm%T
            type(urm%sr%md1%sg%T), intent(in) :: x
            type(urm%sr%md1%sg%T) :: negated

            ! Donev: Observe that one cannot use sr%md1%zero or even sr%zero here
            ! unless we have some form of inheritance:
            negated = minus(sr%md1%empty, x) ! Use the zero element from first monoid
        end function
    end template

    template derive_unit_ring_from_negate(urn, negate)
        satisfaction :: urn=>unit_ring_only_minus(sr,negate) ! Also declares minus
        
        private
        public :: minus

        generic :: minus => minus_
    contains
        elemental function minus_(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference

            ! Brain fry set in by this point, hope I got this right:
            difference = urn%sg1%plus(x, negate(y))
            ! Donev: Or urn%md1%sg%combine, or urn%md1%sg1%plus
        end function

    end template

end module
