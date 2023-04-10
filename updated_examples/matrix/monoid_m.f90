module monoid_m
    !! A monoid is a semigroup with a sensible "empty" or "zero" value.
    !! For sensible implementations combine(empty(), a) == combine(a, empty()) == a.
    use semigroup_m, only: semigroup, extended_semigroup, derive_extended_semigroup

    implicit none
    private
    public :: monoid, extended_monoid, derive_extended_monoid

    requirement monoid(T, combine, empty)
        requires semigroup(T, combine)
        pure function empty()
            type(T) :: empty
        end function
    end requirement

    requirement extended_monoid(T, combine, sconcat, stimes, empty, mconcat)
        requires extended_semigroup(T, combine, sconcat, stimes)
        requires monoid(T, combine, empty)
        pure function mconcat(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined
        end function
    end requirement

    template derive_extended_monoid(T, combine, empty)
        requires monoid(T, combine, empty)

        private
        public :: stimes, mconcat

        instantiate derive_extended_semigroup(T, combine), only: stimes

        generic :: mconcat => mconcat_
    contains
        pure function mconcat_(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined

            integer :: i

            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                combined = empty()
            end if
        end function
    end template
end module