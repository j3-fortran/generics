module minloc_m
    use comparable_m, only: comparable, comparisons_t

    implicit none
    private
    public :: minloc_tmpl

    template minloc_tmpl(T, less_than, result_kind)
        requires comparable(T, less_than)
        integer, parameter :: result_kind

        private
        public :: minloc

        instantiate comparisons_t(T, less_than), only: operator(<)

        interface minloc
            module procedure minloc_no_dim
            module procedure minloc_with_dim
        end interface
    contains
        function minloc_no_dim(array, mask, back) result(location)
            type(T), intent(in) :: array(..)
            logical, intent(in), optional :: mask(..)
            logical, intent(in), optional :: back
            integer(result_kind) :: location(rank(array))

            logical :: back_
            integer(result_kind) :: i, idx(rank(array))

            if (rank(array) == 0) error stop "array must have rank > 0"
            if (present(mask)) then
                if (rank(array) /= rank(mask)) error stop "rank of array and mask must be equal"
                if (.not.all(shape(array) == shape(mask))) error stop "array and mask must have same shape"
            end if

            if (size(array) == 0) then
                location = [(0_result_kind, i = 1, rank(array))]
                return
            end if

            if (present(mask)) then
                if (.not.any(mask)) then
                    location = [(0_result_kind, i = 1, rank(array))]
                    return
                end if
            end if

            if (present(back)) then
                back_ = back
            else
                back_ = .false.
            end if
            if (back_) then
                idx = 1
            else
                idx = shape(array)
            end if
            location = idx

            do
                ! increment idx in element order
                do i = 1, rank(array)
                    if (back_) then
                        idx(i) = idx(i) - 1
                        if (idx(i) == 0) then
                            idx(i) = size(array, dim=i)
                        else
                            exit
                        end if
                    else
                        idx(i) = idx(i) + 1
                        if (idx(i) > size(array, dim=i)) then
                            idx(i) = 1
                        else
                            exit
                        end if
                    end if
                end do
                if (i > rank(array)) exit
                if (array(@idx) < array(@location)) then
                    if (present(mask)) then
                        if (mask(@idx)) then
                            location = idx
                        end if
                    else
                        location = idx
                    end if
                end if
            end do
        end function

        function minloc_with_dim(array, dim, mask, back) result(locations)
            type(T), intent(in) :: array(..)
            integer, intent(in) :: dim
            logical, intent(in), optional :: mask(..)
            logical, intent(in), optional :: back

            integer(result_kind) :: i

            integer(result_kind), &
            rank(rank(array)-1), &
            bounds([(size(array, dim=i), i = 1, dim-1), (size(array, dim=1), i = dim+1, rank(array))]) :: locations

            if (rank(array) == 0) error stop "array must have rank > 0"
            if (dim < 1) error stop "dim must be > 0"
            if (dim > rank(array)) error stop "dim must be <= rank of array"
            if (present(mask)) then
                if (rank(array) /= rank(mask)) error stop "rank of array and mask must be equal"
                if (.not.all(shape(array) == shape(mask))) error stop "array and mask must have same shape"
            end if

            select rank (array)
            rank (1)
                block
                    integer(result_kind) :: locs(1)
                    locs = minloc(array, mask, back)
                    locations = locs(1)
                end block
            rank default
                integer(result_kind) :: idx_front(1:dim-1)
                integer(result_kind) :: idx_back(dim+1:rank(array))
                idx_front = 1
                idx_back = 1
                do
                    if (present(mask)) then
                        locations(@idx_front, @idx_back) = minloc(array(@idx_front, :, @idx_back), dim=1, mask=mask(@idx_front, :, @idx_back), back=back)
                    else
                        locations(@idx_front, @idx_back) = minloc(array(@idx_front, :, @idx_back), dim=1, back=back)
                    end if
                    ! increment idx
                    do i = 1, dim-1
                        idx_front(i) = idx_front(i) + 1
                        if (idx_front(i) > size(array, dim=i)) then
                            idx_front(i) = 1
                        else
                            exit
                        end if
                    end do
                    if (i > dim-1) then
                        do i = dim+1, rank(array)
                            idx_back(i) = idx_back(i) + 1
                            if (idx_back(i) > size(array, dim=i)) then
                                idx_back(i) = 1
                            else
                                exit
                            end if
                        end do
                        if (i > rank(array)) exit
                    end if
                end do
            end select
        end function
    end template
end module