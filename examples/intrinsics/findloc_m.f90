module findloc_m
    use restrictions_m, only: equatable

    implicit none
    private
    public :: findloc_tmpl

    template findloc_tmpl(T, equals, result_kind)
        requires equatable(T, equals)
        integer, parameter :: result_kind

        interface findloc
            module procedure findloc_no_dim
            module procedure findloc_with_dim
        end interface
    contains
        function elemental_equals(lhs, rhs) result(eq)
            type(T), intent(in) :: lhs, rhs
            logical :: eq

            eq = equals(lhs, rhs)
        end function

        function findloc_no_dim(array, value, mask, back) result(location)
            type(T), intent(in) :: array(..), value
            logical, intent(in), optional :: mask(..)
            logical, intent(in), optional :: back
            integer(result_kind) :: location(rank(array))

            location = findloc(elemental_equals(array, value), .true., mask, back)
        end function

        function findloc_with_dim(array, value, dim, mask, back) result(locations)
            type(T), intent(in) :: array(..)
            type(T) :: value
            integer, intent(in) :: dim
            logical, intent(in), optional :: mask(..)
            logical, intent(in), optional :: back

            integer(result_kind) :: i

            integer(result_kind), &
            rank(rank(array)-1), &
            bounds([(size(array, dim=i), i = 1, dim-1), (size(array, dim=1), i = dim+1, rank(array))]) :: locations

            locations = findloc(elemental_equals(array, value), .true., dim, mask, back)
        end function
    end template
end module