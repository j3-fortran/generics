module matrix_m
    use monoid_m, only: derive_monoid
    use semiring_m, only: semiring
    use unit_ring_m, only: unit_ring_only_minus, derive_unit_ring_from_minus

    implicit none
    private
    public :: matrix_tmpl

    template matrix_tmpl(T, plus_t, zero_t, times_t, one_t, n)
        requires semiring(T, plus_t, zero_t, times_t, one_t)

        integer, constant :: n

        private
        public :: &
                matrix, &
                operator(+), &
                operator(*), &
                zero, &
                one, &
                matrix_subtraction_tmpl

        type :: matrix
            type(T) :: elements(n, n)
        end type

        interface operator(+)
            template procedure plus_matrix
        end interface

        interface zero
            template procedure zero_matrix
        end interface

        interface operator(*)
            template procedure times_matrix
        end interface

        interface one
            template procedure identity_matrix
        end interface

        template matrix_subtraction_tmpl(minus_t)
            requires unit_ring_only_minus(T, plus_t, zero_t, times_t, one_t, minus_t)

            private
            public :: operator(-), gaussian_solver_tmpl

            interface operator(-)
                template procedure minus_matrix
            end interface

            template gaussian_solver_tmpl(div_t)
                instantiate derive_unit_ring_from_minus(T, plus_t, zero_t, times_t, one_t, minus_t), only: negate
                requires field_only_division(T, plus_t, zero_t, times_t, one_t, minus_t, negate, div_t)

                private
                public :: operator(/)

                interface operator(/)
                    template procedure div_matrix
                end interface
            contains
                elemental function div_matrix(x, y) result(quotient)
                    type(matrix), intent(in) :: x, y
                    type(matrix) :: quotient

                    quotient = back_substitute(row_eschelon(x), y)
                end function

                pure function row_eschelon(x) result(reduced)
                    type(matrix), intent(in) :: x
                    type(matrix) :: reduced

                    integer :: i, ii, j
                    type(T) :: r

                    reduced = x

                    do i = 1, n
                        ! Assume pivot m(i,i) is not zero
                        do ii = i+1, n
                            r = div_t(reduced%elements(i,i), reduced%elements(ii,i))
                            reduced%elements(ii, i) = zero_t()
                            do j = i+1, n
                                reduced%elements(ii, j) = minus_t(reduced%elements(ii, j), times_t(reduced%elements(i, j), r))
                            end do
                        end do
                    end do
                end function

                pure function back_substitute(x, y) result(solved)
                    type(matrix), intent(in) :: x, y
                    type(matrix) :: solved

                    integer :: i, j
                    type(T) :: tmp(n)

                    solved = y
                    do i = n, 1, -1
                        tmp = zero_t
                        do j = i+1, n
                            tmp = plus(tmp, times(x%elements(i,j), solved%elements(:,j)))
                        end do
                        solved%elements(:,i) = div_t(minus(solved%elements(:, i), tmp), x%elements(i,i))
                    end do
                end function
            end template
        contains
            elemental function minus_matrix(x, y) result(difference)
                type(matrix), intent(in) :: x, y
                type(matrix) :: difference

                difference%elements = minus_t(x%elements, y%elements)
            end function
        end template
    contains
        elemental function plus_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            combined%elements = plus_t(x%elements, y%elements)
        end function

        pure function zero_matrix()
            type(matrix) :: zero_matrix

            zero_matrix%elements = zero_t()
        end function

        elemental function times_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            instantiate derive_monoid(T, plus_t, zero_t), only: sum => mconcat
            integer :: i, j

            do concurrent (i = 1:n, j = 1:n)
                combined%elements(i, j) = sum(times(x%elements(i,:), y%elements(:,j)))
            end do
        end function

        pure function identity_matrix()
            type(matrix) :: identity_matrix

            integer :: i

            identity_matrix%elements = zero_t()
            do concurrent (i = 1:n)
                identity_matrix%elements(i, i) = one_t()
            end do
        end function
    end template
end module