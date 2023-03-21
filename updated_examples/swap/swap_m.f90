module swap_m
    implicit none
    private
    public :: swap_t

    template swap_t(T)
        private
        public :: swap, swap_dyn

        type, deferred :: T

        template swap_rank_n(N)
            integer, constant :: N
            private
            public :: swap, swap_dyn

            interface swap
                procedure swap_
            end interface swap

            interface swap_dyn
                procedure swap_ptr
                procedure swap_alloc
            end interface swap_dyn
        contains
            subroutine swap_(x, y)
                type(T), rank(N), intent(inout):: x, y

                if (product(shape(x)) /= product(shape(y))) error stop "tried to swap objects of different size"
                if (any(shape(x) /= shape(y))) error stop "tried to swap objects of different shape"
                block
                    type(T), rank(N) :: tmp(shape(x))
                    tmp = x
                    x = y
                    y = tmp
                end block
            end subroutine swap_

            subroutine swap_ptr(x, y)
                type(T), rank(N), pointer, intent(inout) :: x, y
                type(T), rank(N), pointer :: tmp

                tmp => x
                x => y
                y => tmp
            end subroutine swap_ptr

            subroutine swap_alloc(x, y)
                type(T), rank(N), allocatable, intent(inout) :: x, y
                type(T), rank(N), allocatable :: tmp

                call move_alloc(from=x, to=tmp)
                call move_alloc(from=y, to= x)
                call move_alloc(from=tmp, to=y)
            end subroutine swap_alloc
        end template swap_rank_n

        instantiate swap_rank_n(0)
        instantiate swap_rank_n(1)
        instantiate swap_rank_n(2)
        instantiate swap_rank_n(3)
        instantiate swap_rank_n(4)
        instantiate swap_rank_n(5)
        instantiate swap_rank_n(6)
        instantiate swap_rank_n(7)
        instantiate swap_rank_n(8)
        instantiate swap_rank_n(9)
        instantiate swap_rank_n(10)
        instantiate swap_rank_n(11)
        instantiate swap_rank_n(12)
        instantiate swap_rank_n(13)
        instantiate swap_rank_n(14)
        instantiate swap_rank_n(15)
    end template swap_t
end module swap_m
