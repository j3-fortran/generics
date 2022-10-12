module non_empty_m
    implicit none
    private
    public :: non_empty_t

    template non_empty_t(T)
        type, deferred :: T

        private
        public :: non_empty

        type :: non_empty
            type(T) :: first_
            type(T), allocatable :: rest_(:)
        contains
            procedure :: first
            procedure :: rest
        end type

        interface non_empty_t
            template procedure construct
        end interface
    contains
        function construct(first, rest) result(non_empty)
            type(T), intent(in) :: first, rest(:)
            type(non_empty) :: non_empty

            non_empty%first_ = first
            non_empty%rest_ = rest
        end function

        function first(self)
            class(non_empty), intent(in) :: self
            type(T) :: first

            first = self%first_
        end function

        function rest(self)
            class(non_empty), intent(in) :: self
            type(T), allocatable :: rest(:)

            rest = self%rest_
        end function
    end template
end module