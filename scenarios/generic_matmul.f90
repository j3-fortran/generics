module generic_sum_m(TA, TB)
    implicit none
    private
    public :: gen_sum_req, generic_sum

    abstract interface gen_sum_req
        type(TB) operator(+)(a, b)
            type(TA), intent(in) :: a, b
        end operator(+)

        type(TB) operator(+)(a, b)
            type(TB), intent(in) :: a, b
        end operator(+)

        type(TB) function zero(a)
            type(TB), intent(in) :: a
        end subroutine

        assignment(=)(b, a)
            type(TB), intent(inout) :: b
            type(TA), intent(in) :: a
        end assignment(=)
    end interface

    interface generic_sum
        module procedure generic_sum_impl
    end interface
contains
    function generic_sum_impl(a) result(total)
        type(TA), intent(in) :: a(:)
        type(TB) :: total

        if (size(a) == 0) then
            total = zero(total)
        else if (size(a) == 1) then
            total = a(1)
        else if (size(a) == 2) then
            total = a(1) + a(2)
        else
            total = generic_sum(a( : size(a)/2)) + generic_sum(a(size(a)/2+1 : ))
        end if
    end function
end module

module generic_matmul_m(TU, TV, TW, TX, TY, TN, TP)
    use generic_sum_m(TX, TN), only: gen_sum_req_tx_tn => gen_sum_req, generic_sum
    use generic_sum_m(TY, TP), only: gen_sum_req_ty_tp => gen_sum_req, generic_sum

    implicit none
    private
    public :: gen_mm_req, generic_matmul

    abstract interface gen_mm_req
        elemental type(TX) operator(*)(u, v)
            type(TU), intent(in) :: u
            type(TV), intent(in) :: v
        end operator(*)

        elemental operator(*)(v, u) result(tv_times_tu)
            type(TV), intent(in) :: v
            type(TU), intent(in) :: u
            type(TY) :: tv_times_tu
        end operator(*)

        type(TZ) operator(+)(n, p)
            type(TN), intent(in) :: n
            type(TP), intent(in) :: p
        end operator(+)

        assignment(=)(w, z)
            type(TW), intent(inout) :: w
            type(TZ), intent(in) :: z
        end assignment(=)

        gen_sum_req_tx_tn
        gen_sum_req_ty_tp
    end interface
contains
    function generic_matmul(u, v) result(w)
        type(TU), intent(in) :: u(:, :)
        type(TV), intent(in) :: v(:, :)
        type(TW) :: w(size(u, 1), size(v, 2))

        call assert(size(u, 2) == size(v, 1))

        do concurrent(i=1:size(u, 1), j=1:size(v,2))
            w(i,j) = generic_sum(u(i,:) * v(:,j)) + generic_sum(v(:,j) * u(i,:))
        end do
    end function
end module
