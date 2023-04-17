module findloc_m
    implicit none
    private
    public :: findloc_tmpl

    requirement equatable(T, equals)
        type, deferred :: T
        elemental function equals(x, y)
            type(T), intent(in) :: x, y
            logical :: equals
        end function
    end requirement

    template findloc_tmpl(T, equals, result_kind)
        private
        public :: findloc
        requires equatable(T, equals)
        integer, constant :: result_kind

        template inner_findloc(N)
            private
            public :: findloc

            integer, constant :: N

            interface operator(==)
                procedure equals
            end interface

            interface findloc
                procedure findloc_no_dim
                procedure findloc_with_dim
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

                integer :: i

                integer(result_kind) :: locations([(size(array, dim=i), i = 1, dim-1), (size(array, dim=1), i = dim+1, rank(N))])

                locations = findloc(array == value, .true., dim, mask, result_kind, back)
            end function
        end template

        instantiate inner_findloc(1)
        instantiate inner_findloc(2)
        instantiate inner_findloc(3)
        instantiate inner_findloc(4)
        instantiate inner_findloc(5)
        instantiate inner_findloc(6)
        instantiate inner_findloc(7)
        instantiate inner_findloc(8)
        instantiate inner_findloc(9)
        instantiate inner_findloc(10)
        instantiate inner_findloc(11)
        instantiate inner_findloc(12)
        instantiate inner_findloc(13)
        instantiate inner_findloc(14)
        instantiate inner_findloc(15)
    end template
end module