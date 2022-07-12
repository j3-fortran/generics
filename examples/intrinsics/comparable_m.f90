module comparable_m
    implicit none
    private
    public :: comparable, comparisons_t

    restriction comparable(T, less_than)
        type :: T
        end type
        interface
            pure function less_than(x, y)
                type(T), intent(in) :: x, y
                logical :: less_than
            end function
        end interface
    end restriction

    template comparisons_t(T, less_than)
        requires comparable(T, less_than)
        private
        public :: &
                operator(<), &
                operator(>), &
                operator(<=), &
                operator(>=), &
                operator(==), &
                operator(/=)

        interface operator(<)
            module procedure elemental_less_than
        end interface

        interface operator(>)
            module procedure greater_than
        end interface

        interface operator(<=)
            module procedure less_than_or_equal
        end interface

        interface operator(>=)
            module procedure greater_than_or_equal
        end interface

        interface operator(==)
            module procedure equal
        end interface

        interface operator(/=)
            module procedure not_equal
        end interface
    contains
        elemental function elemental_less_than(lhs, rhs) result(lt)
            type(T), intent(in) :: lhs, rhs
            logical :: lt

            lt = less_than(lhs, rhs)
        end function

        elemental function greater_than(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: greater_than

            greater_than = rhs < lhs
        end function

        elemental function less_than_or_equal(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: less_than_or_equal

            less_than_or_equal = .not. (rhs < lhs)
        end function

        elemental function greater_than_or_equal(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: greater_than_or_equal

            greater_than_or_equal = .not. (lhs < rhs)
        end function

        elemental function equal(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: equal

            equal = .not.(lhs < rhs .or. rhs < lhs)
        end function

        elemental function not_equal(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: not_equal

            not_equal = lhs < rhs .or. rhs < lhs
        end function
    end template
end module