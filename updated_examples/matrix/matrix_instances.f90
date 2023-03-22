module real_matrix_m
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(real, operator(+), real_zero, operator(*), real_one, n), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-), gaussian_solver_tmpl
    instantiate gaussian_solver_tmpl(operator(/)), only: operator(/)
contains
    pure function real_zero()
        real :: real_zero

        real_zero = 0.
    end function

    pure function real_one()
        real :: real_one

        real_one = 1.
    end function
end module

module complex_matrix_m
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(complex, operator(+), complex_zero, operator(*), complex_one, n), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-), gaussian_solver_tmpl
    instantiate gaussian_solver_tmpl(operator(/)), only: operator(/)
contains
    pure function complex_zero()
        complex :: complex_zero

        complex_zero = 0.
    end function

    pure function complex_one()
        complex :: complex_one

        complex_one = 1.
    end function
end module

module integer_matrix_m
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(integer, operator(+), integer_zero, operator(*), integer_one, n), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-)
    ! Note, gaussian_solver wouldn't work correctly for integer matrix
contains
    pure function integer_zero()
        integer :: integer_zero

        integer_zero = 0
    end function

    pure function integer_one()
        integer :: integer_one

        integer_one = 1
    end function
end module

module tropical_semiring_m
    !! This is a useful representation for graphs for certain algorithms.
    use matrix_m, only: matrix_tmpl

    implicit none

    integer, parameter :: n = 10

    instantiate matrix_tmpl(real, min, real_inf, operator(+), real_zero, n), only: &
            matrix, operator(+), zero, operator(*)
contains
    pure function real_inf()
        use ieee_arithmetic, only: ieee_value, ieee_positive_inf

        real :: real_inf

        real_inf = ieee_value(real_inf, ieee_positive_inf)
    end function

    pure function real_zero()
        real :: real_zero

        real_zero = 0.
    end function
end module

module block_matrix_m
    use matrix_m, only: matrix_tmpl
    use real_matrix_m, only: &
            block_ => matrix, &
            operator(+), &
            zero_b => zero, &
            operator(*), &
            one_b => one, &
            operator(-), &
            operator(/)

    implicit none

    instantiate matrix_tmpl(block_, operator(+), zero_b, operator(*), one_b, 5), only: &
            matrix, operator(+), zero, operator(*), one, matrix_subtraction_tmpl
    instantiate matrix_subtraction_tmpl(operator(-)), only: operator(-), gaussian_solver_tmpl
    instantiate gaussian_solver_tmpl(operator(/)), only: operator(/)
end module