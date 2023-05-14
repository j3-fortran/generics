module monoid_m
    !! A monoid is a semigroup with a sensible "empty" or "zero" value.
    !! For sensible implementations combine(empty(), a) == combine(a, empty()) == a.
    ! Donev: I have added a new feature here where empty can be a constant
    ! instead of a function. This is orthogonal to the satisfaction proposal
    ! but I could not resist
    use semigroup_m, only: semigroup, extended_semigroup, derive_extended_semigroup

    implicit none
    private
    public :: monoid, extended_monoid, derive_extended_monoid

    ! Donev: Shortest, most reuse, but has downside that we now require
    ! md%sg%combine to access combine
    ! Cannot use md%combine without some sort of "inheritance" for requirements
    requirement monoid(sg, empty)
        satisfaction :: sg=>semigroup(T, combine)
        type(sg%T), constant :: empty ! Donev: New syntax
    end requirement
    
    ! Donev: ALTERNATIVE (programmer's choice, semantics is identical)
    ! Main advantages are:
    ! a) Monoid requirement and semigroup requirement can be developed independently
    ! (but exact explicit interfaces of methods must match, including keywords/names)
    ! b) One can directly access md%combine instead of md%sg%combine
    requirement monoid_alt(T, combine, empty)
        type, deferred :: T
        elemental function combine(x, y) result(combined)
            type(T), intent(in) :: x, y
            type(T) :: combined
        end function
    end requirement    

    requirement extended_monoid(esg, md, mconcat)
        satisfaction :: esg=>extended_semigroup(T, combine, sconcat, stimes)
        ! Donev: Important new syntax allowing to share/combine satisfactions
        ! In Ada this means that the monoid must use the same type and
        ! the same actual procedure combine as the extended semigroup
        ! (*not* just the same TKR signature or interface)
        satisfaction :: md=>monoid(esg%sg, empty) ! Donev: This "declares" what empty is
        ! Donev: ALTERNATIVE:
        ! satisfaction :: md=>monoid(esg%T, esg%combine, empty)        
            
        pure function mconcat(list) result(combined)
            type(esg%T), intent(in) :: list(:)
            type(esg%T) :: combined
        end function
    end requirement

    template derive_extended_monoid(md)
        ! Donev: New syntax
        satisfaction :: md=>monoid(semigroup(T, combine), empty))
        ! Donev: ALTERNATIVE:
        !satisfaction :: md=>monoid(T, combine, empty)

        private
        public :: stimes, mconcat
        
        ! Donev: New syntax that allows one to shorten component selection lists
        satisfaction :: sg=>semigroup(md%T, md%combine) ! Donev: Intermediary
        ! Or, one can directly do
        ! satisfaction :: sg=>md%sg ! Just acts as an alias/shortcut
        instantiate derive_extended_semigroup(sg), only: stimes
        ! Donev: I have assumed in these codes that one can also just do
        ! instantiate derive_extended_semigroup(semigroup(md%T, md%combine))
        ! without declaring an explicit satisfaction sg
        ! But I think allowing one to declare an intermediary sg is very useful

        generic :: mconcat => mconcat_
    contains
        pure function mconcat_(list) result(combined)
            ! Donev: One can use either sg%T and md%sg%T here, as they will be the same
            ! Observe how the intermediate name (alias, associated name) sg helps here
            type(sg%T), intent(in) :: list(:)
            type(sg%T) :: combined

            integer :: i

            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = sg%combine(combined, list(i))
                end do
            else
                combined = md%empty
            end if
        end function
    end template
end module
