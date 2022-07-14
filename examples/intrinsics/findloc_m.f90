module findloc_m
    use comparable_m, only: comparable, comparisons_t

    implicit none
    private
    public :: findloc_tmpl

    template findloc_tmpl(T, less_than, result_kind)
        requires comparable(T, less_than)
        integer, parameter :: result_kind

        private
        public :: findloc

        instantiate comparisons_t(T, less_than), only: operator(==)

        interface findloc
            module procedure findloc_no_dim
            module procedure findloc_with_dim
        end interface
    contains
        function findloc_no_dim(array, value, mask, back) result(location)
            type(T), intent(in) :: array(..), value
            logical, intent(in), optional :: mask(..)
            logical, intent(in), optional :: back
            integer(result_kind) :: location(rank(array))

            location = findloc(array == value, .true., mask, result_kind, back)
        end function

        function findloc_with_dim(array, value, dim, mask, back) result(locations)
            type(T), intent(in) :: array(..)
            type(T) :: value
            integer, intent(in) :: dim
            logical, intent(in), optional :: mask(..)
            logical, intent(in), optional :: back

            integer(result_kind) :: i

            integer(result_kind), &
            bounds([(size(array, dim=i), i = 1, dim-1), (size(array, dim=1), i = dim+1, rank(array))]) :: locations

            locations = findloc(array == value, .true., dim, mask, result_kind, back)
        end function
    end template
end module
