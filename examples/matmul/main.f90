program main
    use matmul_m, only: matmul_t

    instantiate matmul_t(real, real, real, operator(*), sum), only: matmul

    real :: x(3, 2), y(2, 3), z(3, 3)

    x = reshape([(real(i), i = 1, 6)], [3, 2])
    y = reshape([(real(i), i = 6, 1, -1)], [2, 3])
    z = matmul(x, y)
    do i = 1, 3
      print *, z(i,:)
    end do
end program
