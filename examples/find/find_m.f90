module find_m
    implicit none
    private
    public :: findloc_t, minloc_t, equatable, comparable

    restriction equatable(T, equals)
        type :: T
        end type
        interface
            function equals(x, y)
                type(T), intent(in) :: x, y
                logical :: equals
            end function
        end interface
    end restriction

    restriction comparable(T, equals, less_than)
        requires equatable(T, equals)
        interface
            function less_than(x, y)
                type(T), intent(in) :: x, y
                logical :: less_than
            end function
        end interface
    end restriction

    template findloc_t(T, equals)
        private
        public :: findloc
        requires equatable(T, equals)
        interface findloc
            module procedure findloc_
        end interface
    contains
        function findloc_(x, array) result(location)
            type(T), intent(in) :: x, array(:)
            integer :: location

            integer :: i

            location = 0
            do i = 1, size(array)
                if (equals(x, array(i))) then
                  location = i
                  return
                end if
            end do
        end function
    end template

    template minloc_t(T, equals, less_than)
        private
        public :: minloc
        requires comparable(T, equals, less_than)
        interface minloc
            module procedure minloc_
        end interface
    contains
        function minloc_(array) result(location)
            type(T), intent(in) :: array(:)
            integer :: location

            ...
        end function
    end template
end module
