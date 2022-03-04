module matmul_m
    implicit none
    private
    public :: matmul_t

    restriction matmulable(T, U, V, times, sum)
        type :: T; end type
        type :: U; end type
        type :: V; end type
        interface
            elemental function times(x, y) result(prod)
                type(T), intent(in) :: x
                type(U), intent(in) :: y
                type(V) :: prod
            end function

            function sum(arr)
                type(V) :: arr(:)
                type(V) :: sum
            end function
        end interface
    end restrction

    template matmul_t(T, U, V, times, sum)
        private
        public :: matmul
        requires matmulable(T, U, V, times, sum)
        interface matmul
            module procedure matmul_
        end interface
    contains
        function matmul_(A, B) result(C)
            type(T), intent(in) :: A(:,:)
            type(U), intent(in) :: B(:,:)
            type(V), allocatable :: C(:, :)

            integer :: i, j

            if (size(A, dim=2) /= size(B, dim=1)) error stop "Mismatched matrix sizes"
            allocate(C(size(A, dim=1), size(B, dim=2)))
            do concurrent(i = 1 : size(A, dim=1), j = 1 : size(B, dim=2))
                C(i,j) = sum(times(A(i,:),B(:,j)))
            end do
        end function
    end template
end module
