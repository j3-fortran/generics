module findloc_m
    use restrictions_m, only: equatable

    implicit none
    private
    public :: findloc_tmpl

    template findloc_tmpl(T, equals, result_kind)
        requires equatable(T, equals)
        integer, parameter :: result_kind

        interface findloc
            module procedure findloc_no_dim
            module procedure findloc_with_dim
        end interface
    contains
        function findloc_no_dim(array, value, mask, back) result(location)
            type(T), intent(in) :: array(..), value
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

            location = [(0_result_kind, i = 1, rank(array))]
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
                if (equals(array(@idx), value)) then
                    if (present(mask)) then
                        if (mask(@idx)) then
                            location = idx
                            return
                        end if
                    else
                        location = idx
                        return
                    end if
                end if
            end do
        end function

        function findloc_with_dim(array, value, dim, mask, back) result(locations)
            type(T), intent(in) :: array(..)
            type(T) :: value
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
                    locs = findloc(array, value, mask, back)
                    locations = locs(1)
                end block
            rank default
                integer(result_kind) :: idx_front(1:dim-1)
                integer(result_kind) :: idx_back(dim+1:rank(array))
                idx_front = 1
                idx_back = 1
                do
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
                    if (present(mask)) then
                        locations(@idx_back, @idx_back) = findloc(array(@idx_front, :, @idx_back), value, dim=1, mask=mask(@idx_front, :, @idx_back), back=back)
                    else
                        locations(@idx_back, @idx_back) = findloc(array(@idx_front, :, @idx_back), value, dim=1, back=back)
                    end if
                end do
            end select
        end function
    end template
end module