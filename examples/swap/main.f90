program main
    use swap_m, only: swap_t

    instantiate swap_t(integer), only: swap
    instantiate swap_t(real), only: swap

    integer :: i, j
    real :: x, y

    i = 3
    j = 4
    print *, "i = ", i, ", j = ", j
    call swap(i, j)
    print *, "i = ", i, ", j = ", j

    x = 3.1415
    y = 2.7183
    print *, "x = ", x, ", y = ", y
    call swap(x, y)
    print *, "x = ", x, ", y = ", y
end program
