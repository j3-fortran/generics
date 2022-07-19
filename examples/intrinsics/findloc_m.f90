module findloc_m
    use comparable_m, only: comparable, comparisons_t

    implicit none
    private
    public :: findloc_tmpl

    template findloc_tmpl(T, less_than, result_kind)
        requires comparable(T, less_than)
        integer, parameter :: result_kind

        instantiate inner_findloc(T, less_than, result_kind, 1)
        instantiate inner_findloc(T, less_than, result_kind, 2)
        instantiate inner_findloc(T, less_than, result_kind, 3)
        instantiate inner_findloc(T, less_than, result_kind, 4)
        instantiate inner_findloc(T, less_than, result_kind, 5)
        instantiate inner_findloc(T, less_than, result_kind, 6)
        instantiate inner_findloc(T, less_than, result_kind, 7)
        instantiate inner_findloc(T, less_than, result_kind, 8)
        instantiate inner_findloc(T, less_than, result_kind, 9)
        instantiate inner_findloc(T, less_than, result_kind, 10)
        instantiate inner_findloc(T, less_than, result_kind, 11)
        instantiate inner_findloc(T, less_than, result_kind, 12)
        instantiate inner_findloc(T, less_than, result_kind, 13)
        instantiate inner_findloc(T, less_than, result_kind, 14)
        instantiate inner_findloc(T, less_than, result_kind, 15)

        template inner_findloc(T, less_than, result_kind, N)
            requires comparable(T, less_than)
            integer, parameter :: result_kind
            integer, parameter :: N

            private
            public :: findloc

            instantiate comparisons_t(T, less_than), only: operator(==)

            interface findloc
                module procedure findloc_no_dim
                module procedure findloc_with_dim
            end interface
        contains
            function findloc_no_dim(array, value, mask, back) result(location)
                type(T), intent(in), rank(N) :: array
                type(T), intent(in) :: value
                logical, intent(in), rank(N), optional :: mask
                logical, intent(in), optional :: back
                integer(result_kind) :: location(N)

                location = findloc(array == value, .true., mask, result_kind, back)
            end function

            function findloc_with_dim(array, value, dim, mask, back) result(locations)
                type(T), intent(in), rank(N) :: array
                type(T), intent(in) :: value
                integer, intent(in) :: dim
                logical, intent(in), rank(N), optional :: mask
                logical, intent(in), optional :: back

                integer(result_kind) :: i

                integer(result_kind), &
                bounds([(size(array, dim=i), i = 1, dim-1), (size(array, dim=1), i = dim+1, rank(N))]) :: locations

                locations = findloc(array == value, .true., dim, mask, result_kind, back)
            end function
        end template
    end template
end module
