module semigroup_m
    !! A semigroup is a type with a sensible operation for combining two objects
    !! of that type to produce another object of the same type.
    !! A sensible operation has the associative property (i.e. (a + b) + c == a + (b + c))
    !! Given this property, it also makes sense to combine a list of objects of
    !! that type into a single object, or to repeatedly combine an object with
    !! itself. These operations can be derived in terms of combine.
    !! Examples include integer (i.e. +), and character (i.e. //)
    implicit none
    private
    public :: semigroup, extended_semigroup, derive_extended_semigroup

    requirement semigroup(T, combine)
        type, deferred :: T
        elemental function combine(x, y) result(combined)
            type(T), intent(in) :: x, y
            type(T) :: combined
        end function
    end requirement

    requirement extended_semigroup(T, combine, sconcat, stimes)
        requires semigroup(T, combine)
        pure function sconcat(list) result(combined)
            type(T), intent(in) :: list(:) !! Must contain at least one element
            type(T) :: combined
        end function
        elemental function stimes(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated
        end function
    end requirement

    template derive_extended_semigroup(T, combine)
        requires semigroup(T, combine)

        private
        public :: sconcat, stimes

        generic :: sconcat => sconcat_
        generic :: stimes => stimes_
    contains
        pure function sconcat_(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined

            integer :: i

            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                error stop "Attempted to sconcat empty list"
            end if
        end function

        elemental function stimes_(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated

            integer :: i

            if (n < 1) error stop "n must be > 0"
            repeated = a
            do i = 2, n
                repeated = combine(repeated, a)
            end do
        end function
    end template
end module