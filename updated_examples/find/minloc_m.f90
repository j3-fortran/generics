module minloc_m
    implicit none
    private
    public :: minloc_tmpl

    requirement comparable(T, less_than)
        type, deferred :: T
        elemental function less_than(x, y)
            type(T), intent(in) :: x, y
            logical :: less_than
        end function
    end requirement

    template minloc_tmpl(T, less_than, result_kind)
        private
        public :: minloc
        requires comparable(T, less_than)
        integer, constant :: result_kind

        template inner_minloc(N)
            private
            public :: minloc
            integer, constant :: N

            interface operator(<)
                procedure less_than
            end interface

            interface minloc
                procedure minloc_no_dim
                procedure minloc_with_dim
            end interface
        contains
            function minloc_no_dim(array, mask, back) result(location)
                type(T), intent(in), rank(N) :: array
                logical, intent(in), rank(N), optional :: mask
                logical, intent(in), optional :: back
                integer(result_kind) :: location(N)

                logical :: back_
                integer :: i, idx(N)

                if (present(mask)) then
                    if (.not.all(shape(array) == shape(mask))) error stop "array and mask must have same shape"
                end if

                if (size(array) == 0) then
                    location = 0_result_kind
                    return
                end if

                if (present(mask)) then
                    if (.not.any(mask)) then
                        location = 0_result_kind
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
                    if (present(mask)) then
                        if (mask(@idx) .and. .not.mask(@location)) then
                            location = idx
                        else if (mask(@idx) .and. array(@idx) < array(@location)) then
                            location = idx
                        end if
                    else
                        if (array(@idx) < array(@location)) location = idx
                    end if
                end do
            end function

            function minloc_with_dim(array, dim, mask, back) result(locations)
                type(T), intent(in), rank(N) :: array
                integer, intent(in) :: dim
                logical, intent(in), rank(N), optional :: mask
                logical, intent(in), optional :: back

                integer :: i

                integer(result_kind) :: locations([(size(array, dim=i), i = 1, dim-1), (size(array, dim=1), i = dim+1, rank(array))])

                if (dim < 1) error stop "dim must be > 0"
                if (dim > N) error stop "dim must be <= rank of array"
                if (present(mask)) then
                    if (.not.all(shape(array) == shape(mask))) error stop "array and mask must have same shape"
                end if

                if (N == 1) then
                    associate(locs => minloc(array, mask, back))
                        locations = locs(1)
                    end associate
                else
                block
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
                end block
                end if
            end function
        end template

        instantiate inner_minloc(1)
        instantiate inner_minloc(2)
        instantiate inner_minloc(3)
        instantiate inner_minloc(4)
        instantiate inner_minloc(5)
        instantiate inner_minloc(6)
        instantiate inner_minloc(7)
        instantiate inner_minloc(8)
        instantiate inner_minloc(9)
        instantiate inner_minloc(10)
        instantiate inner_minloc(11)
        instantiate inner_minloc(12)
        instantiate inner_minloc(13)
        instantiate inner_minloc(14)
        instantiate inner_minloc(15)
    end template
end module