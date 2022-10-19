module numeric_requirements_m
    implicit none
    private
    public :: &
            semiring_r, &
            unit_ring_r, &
            field_r, &
            trig_r, &
            exp_r, &
            hyperbolic_r, &
            roots_r, &
            numeric_r, &
            ordered_r

    requirement semiring_r(T, mult, one, plus, zero)
        type, deferred :: T
        interface
            elemental function mult(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: mult
            end function

            pure function one()
                implicit none
                type(T) :: one
            end function

            elemental function plus(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: plus
            end function

            pure function zero()
                implicit none
                type(T) :: zero
            end function
        end interface
    end requirement

    requirement unit_ring_r(T, mult, one, plus, zero, minus, uminus)
        requires semiring_r(T, mult, one, plus, zero)
        interface
            elemental function minus(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: minus
            end function

            elemental function uminus(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: uminus
            end function
        end interface
    end requirement

    requirement field_r(T, mult, one, plus, zero, minus, uminus, division, inverse)
        requires unit_ring_r(T, mult, one, plus, zero, minus, uminus)
        interface
            elemental function division(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: division
            end function

            elemental function inverse(x)
                type(T), intent(in) :: x
                type(T) :: inverse
            end function
        end interface
    end requirement

    requirement trig_r(T, sin, cos, tan, asin, acos, atan, atan2, pi)
        type, deferred :: T
        interface
            elemental function sin(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: sin
            end function

            elemental function cos(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: cos
            end function

            elemental function tan(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: tan
            end function

            elemental function asin(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: asin
            end function

            elemental function acos(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: acos
            end function

            elemental function atan(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: atan
            end function

            elemental function atan2(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: atan2
            end function

            pure function pi()
                implicit none
                type(T) :: pi
            end function
        end interface
    end requirement

    requirement exp_r( &
            T, mult, one, plus, zero, minus, uminus, division, inverse, &
            e, exp, exp10, exp2, log, log10, log2, pow)
        requires field_r(T, mult, one, plus, zero, minus, uminus, division, inverse)

        pure function e()
            implicit none
            type(T) :: e
        end function

        elemental function exp(x)
            implicit none
            type(T), intent(in) :: x
            type(T) :: exp
        end function

        elemental function exp10(x)
            implicit none
            type(T), intent(in) :: x
            type(T) :: exp10
        end function

        elemental function exp2(x)
            implicit none
            type(T), intent(in) :: x
            type(T) :: exp2
        end function

        elemental function log(x)
            implicit none
            type(T), intent(in) :: x
            type(T) :: log
        end function

        elemental function log10(x)
            implicit none
            type(T), intent(in) :: x
            type(T) :: log10
        end function

        elemental function log2(x)
            implicit none
            type(T), intent(in) :: x
            type(T) :: log2
        end function

        elemental function pow(x, y)
            implicit none
            type(T), intent(in) :: x, y
            type(T) :: pow
        end function
    end requirement

    requirement hyperbolic_r( &
            T, mult, one, plus, zero, minus, uminus, division, inverse, &
            e, exp, exp10, exp2, log, log10, log2, pow, &
            sinh, cosh, tanh, asinh, acosh, atanh)
        requires exp_r( &
                T, mult, one, plus, zero, minus, uminus, division, inverse, &
                e, exp, exp10, exp2, log, log10, log2, pow)
        interface
            elemental function sinh(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: sinh
            end function

            elemental function cosh(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: cosh
            end function

            elemental function tanh(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: tanh
            end function

            elemental function asinh(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: asinh
            end function

            elemental function acosh(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: acosh
            end function

            elemental function atanh(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: atanh
            end function
        end interface
    end requirement

    requirement roots_r( &
            T, mult, one, plus, zero, minus, uminus, division, inverse, &
            sqrt, cbrt)
        requires field_r(T, mult, one, plus, zero, minus, uminus, division, inverse)
        interface
            elemental function sqrt(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: sqrt
            end function

            elemental function cbrt(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: cbrt
            end function
        end interface
    end requirement

    requirement numeric_r( &
            T, mult, one, plus, zero, minus, uminus, division, inverse, &
            e, exp, exp10, exp2, log, log10, log2, pow, &
            sin, cos, tan, asin, acos, atan, atan2, pi, &
            sinh, cosh, tanh, asinh, acosh, atanh, &
            sqrt, cbrt)
        requires trig_r(T, sin, cos, tan, asin, acos, atan, atan2, pi)
        requires hyperbolic_r( &
                T, mult, one, plus, zero, minus, uminus, division, inverse, &
                e, exp, exp10, exp2, log, log10, log2, pow, &
                sinh, cosh, tanh, asinh, acosh, atanh)
        requires roots_r( &
                T, mult, one, plus, zero, minus, uminus, division, inverse, &
                sqrt, cbrt)
    end requirement

    requirement ordered_r( &
            T, mult, one, plus, zero, &
            le, lt, ge, gt, abs, max, min)
        requires semiring_r(T, mult, one, plus, zero)
        interface
            elemental function le(x, y)
                implicit none
                type(T), intent(in) :: x, y
                logical :: le
            end function

            elemental function lt(x, y)
                implicit none
                type(T), intent(in) :: x, y
                logical :: lt
            end function

            elemental function ge(x, y)
                implicit none
                type(T), intent(in) :: x, y
                logical :: ge
            end function

            elemental function gt(x, y)
                implicit none
                type(T), intent(in) :: x, y
                logical :: gt
            end function

            elemental function abs(x)
                implicit none
                type(T), intent(in) :: x
                type(T) :: abs
            end function

            elemental function max(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: max
            end function

            elemental function min(x, y)
                implicit none
                type(T), intent(in) :: x, y
                type(T) :: min
            end function
        end interface
    end requirement
end module