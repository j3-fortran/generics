module restrictions_m
    implicit none
    private
    public :: equatable, comparable

    restriction equatable(T, equals)
        type :: T
        end type
        interface
            pure function equals(x, y)
                type(T), intent(in) :: x, y
                logical :: equals
            end function
        end interface
    end restriction

    restriction comparable(T, equals, less_than)
        requires equatable(T, equals)
        interface
            pure function less_than(x, y)
                type(T), intent(in) :: x, y
                logical :: less_than
            end function
        end interface
    end restriction
end module