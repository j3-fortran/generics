module template_simple_02_m

    requirement operator_r(T, U, V, binary_func)
        type, deferred :: T
        type, deferred :: U
        type, deferred :: V
        pure elemental function binary_func(lhs, rhs) result(res)
            type(T), intent(in) :: lhs
            type(U), intent(in) :: rhs
            type(V) :: res
        end function
    end requirement

    requirement cast_r(T, cast)
        type, deferred :: T
        pure elemental function cast(arg) result(res)
            integer, intent(in) :: arg
            type(T) :: res
        end function
    end requirement

contains

    pure elemental function cast_integer(arg) result(res)
        integer, intent(in) :: arg
        integer :: res
        res = arg
    end function

    pure elemental function cast_real(arg) result(res)
        integer, intent(in) :: arg
        real :: res
        res = arg
    end function

    pure function sub2{T,add,cast}(a) result(res)
        require :: operator_r(T, T, T, add), cast_r(T, cast)
        type(T), intent(in) :: a
        type(T) :: res
        res = add(a, cast(3))
    end function

    subroutine usage4()
        print *, sub2{real,operator(+),cast_real}(5.0)
        print *, sub2{real,operator(-),cast_real}(5.0)
        print *, sub2{integer,operator(+),cast_integer}(5)
        print *, sub2{integer,operator(-),cast_integer}(5)
    end subroutine

end module

program template_simple_02
    use template_simple_02_m

    call usage4()

end
