module sparse_array_m

    concept :: copyable
      assignment: T = T
    end concept

    type :: sparse_array_t<T>
        requirements(T) :: copyable
        private
        type(T), allocatable :: elements(:)
        integer, allocatable :: indices(:)
    contains
        procedure :: insert_at
        procedure :: get
    end type
contains
    subroutine insert_at<T>(self, index, element)
        requirements(T) :: copyable
        class(sparse_array_t<T>), intent(inout) :: self
        integer, intent(in) :: index
        type(T), intent(in) :: element

        ...
    end subroutine

    function get<T>(self, index) result(element)
        requirements(T) :: copyable
        class(sparse_array_t<T>), intent(in) :: self
        integer, intent(in) :: index
        type(T) :: element

        ...
    end function

    function map<T, U>(transformation, xs) result(ys)
        requirements(T) :: copyable
        requirements(U) :: copyable
        interface
            pure function transformation_i(x) result(y)
                type(T), intent(in) :: x
                type(U) :: y
            end function
        end interface
        procedure(transformation_i) :: transformation
        type(sparse_array_t<T>), intent(in) :: xs
        type(sparse_array_t<U>) :: ys

        ys%indices = xs%indices
        ys%elements = etransformation(xs%elements)
    contains
        elemental function etransformation(x) result(y)
            type(T), intent(in) :: x
            type(U) :: y

            y = transformation(y)
        end function
    end function

    function filter<T>(array, predicate) result(filtered)
        requirements(T) :: copyable
        interface
            pure function predicate_i(element)
                type(T), intent(in) :: element
                logical :: predicate_i
            end function
        end interface
        type(sparse_array_t<T>), intent(in) :: array
        procedure(predicate_i) :: predicate
        type(sparse_array_t<T>) :: filtered

        associate(matches => epredicate(array%elements))
            filtered%indices = pack(array%indices, matches)
            filtered%elements = pack(array%elements, matches)
        end associate
    contains
        elemental function epredicate(element)
            type(T), intent(in) :: element
            logical :: epredicate

            epredicate = predicate(element)
        end function
    end function

    function reduce<T, U>(array, accumulator, initial) result(combined)
        requirements(T) :: copyable
        requirements(U) :: copyable
        interface
            function accumulator_i(x, y) result(z)
                type(U), intent(in) :: y
                type(T), intent(in) :: x
                type(U) :: z
            end function
        end interface
        type(sparse_array_t<T>), intent(in) :: array
        procedure(accumulator_i) :: accumulator
        type(U), intent(in) :: initial
        type(U) :: combined

        integer :: i

        combined = accumulator(intial, self%elements(1))
        do i = 2, size(self%elements)
            combined = accumulator(combined, self%elements(i))
        end do
    end function
end module
