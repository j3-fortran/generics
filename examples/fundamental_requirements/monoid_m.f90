module monoid_m
    !! A monoid is a semigroup with a sensible "empty" or "zero" value.
    !! For sensible implementations combine(empty(), a) == combine(a, empty()) == a.
    use semigroup_m, only: minimal_semigroup, semigroup, derive_semigroup

    implicit none
    private
    public :: minimal_monoid, monoid, derive_monoid

    requirement minimal_monoid(T, combine, empty)
        requires minimal_semigroup(T, combine)
        function empty()
            type(T) :: empty
        end function
    end requirement

    requirement monoid(T, combine, sconcat, stimes, empty, mconcat)
        requires semigroup(T, combine, sconcat, stimes)
        requires minimal_monoid(T, empty)
        function mconcat(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined
        end function
    end requirement

    template derive_monoid(T, combine, empty)
        requires minimal_monoid(T, combine, empty)

        private
        public :: sconcat, stimes, mconcat

        instantiate derive_semigroup(T, combine), only: sconcat, stimes

        interface mconcat
            template procedure mconcat_
        end interface
    contains
        function mconcat_(list) result(combined)
            type(T), intent(in) :: list
            type(T) :: combined

            integer :: i

            select case (size(list))
            case (0)
                combined = empty()
            case (1)
                combined = list(1)
            case (2:)
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            end select
        end function
    end template
end module