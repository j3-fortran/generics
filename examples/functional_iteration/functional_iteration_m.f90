module functional_iteration_m
    implicit none
    private
    public :: fold_t, scan_t

    template fold_t(T, U)
        type :: T; end type
        type :: U; end type

        private
        public :: fold

        interface fold
            module procedure fold_
        end interface

        abstract interface
            function bin_op(lhs, rhs) result(combined)
                type(T), intent(in) :: lhs
                type(U), intent(in) :: rhs
                type(U) :: combined
            end function
        end interface
    contains
        function fold_(init, array, op) result(folded)
            type(U), intent(in) :: init
            type(T), intent(in) :: array(:)
            procedure(bin_op) :: op
            type(U) :: folded

            integer :: i

            folded = init
            do i = 1, size(array)
                folded = op(folded, array(i))
            end do
        end function
    end template

    template scan_t(T)
        type :: T; end type

        private
        public :: scan

        interface scan
            module procedure scan_
        end interface

        abstract interface
            function bin_op(lhs, rhs) result(combined)
                type(T), intent(in) :: lhs
                type(T), intent(in) :: rhs
                type(T) :: combined
            end function
        end interface
    contains
        function scan_(array, op) result(scanned)
            type(T), intent(in) :: array(:)
            procedure(bin_op) :: op
            type(T) :: scanned(size(array))

            integer :: i

            if (size(array) > 0) then
                scanned(1) = array(1)
                do i = 2, size(array)
                    scanned(i) = op(scanned(i-1), array(i))
                end do
            end if
        end function
    end template
end module