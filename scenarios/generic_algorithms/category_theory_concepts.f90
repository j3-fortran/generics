module category_theory_concepts
    ! Note: Sensible implementations should satisfy
    ! associativity: a // (b // c) == (a // b) // c
    concept :: semigroup(T)
        operator(//)(lhs, rhs) result(combined)
            T, intent(in) :: lhs, rhs
            T :: combined
        end operator(//)
        function sconcat(xs, starting) result(combined)
            T, intent(in) :: xs(:), starting
            T :: combined
            default
                integer :: i
                combined = starting
                do i = 1, size(xs)
                    combined = combined // xs(i)
                end do
            end default
        end function
        function stimes(num, start) result(repeated)
            integer, intent(in) :: num
            T, intent(in) :: start
            T :: repeated
            default
                if (num < 1) then
                    if (monoid(T)) then
                        repeated = mempty(start)
                    else
                        error stop "attempted to repeat non-monoid 0 times"
                    end if
                else
                    repeated = sconcat([(start, i = 1, num-1)], start)
                end if
            end default
        end function
    end concept

    ! Note: Sensible implementations should satisfy
    ! identity: T // mempty(T) == T
    !           mempty(T) // T == T
    concept :: monoid(T)
        requirements :: semigroup(T)
        function mempty(mold)
            T, intent(in) :: mold
            T :: mempty
        end function
        function mappend(x, y) result(combined)
            T, intent(in) :: x, y
            T :: combined
            default
                combined = x // y
            end default
        end function
        function mconcat(xs) result(combined)
            T, intent(in) :: xs(:)
            T :: combined
            default
                combined = sconcat(xs, mempty(combined))
            end default
        end function
    end concept
end module

module maybe
    use category_theory_concepts, only: semigroup, monoid

    concept :: copyable(T)
        assignment: T = T
    end concept

    type :: maybe_t<T>
        private
        logical :: available
        T :: item
    end type
    if (semigroup(T)) then
        satisfies :: semigroup(maybe_t<T>)
        satisfies :: monoid(maybe_t<T>)
    end if

    interface maybe_t
        module procedure just
        module procedure nothing
    end interface

    if (semigroup(T)) then
        interface operator(//)
            module procedure concat
        end interface
    end if
contains
    function just(item) result(maybe)
        T, intent(in) :: item
        type(maybe_t<T>) :: maybe

        maybe%item = item
        maybe%available = .true.
    end function

    function nothing(mold) result(maybe)
        T, intent(in) :: mold
        type(maybe_t<T>) :: maybe

        maybe%available = .false.
    end function

    if (semigroup(T)) then
        function concat(lhs, rhs) result(combined)
            type(maybe_t<T>), intent(in) :: lhs, rhs
            type(maybe_t<T>) :: combined

            if (lhs%available) then
                if (rhs%available) then
                    combined%item = lhs%item // rhs%item
                    combined%available = .true.
                else
                    combined%item = lhs%item
                    combined%available = .true.
                end if
            else
                if (rhs%available) then
                    combined%item = rhs%item
                    combined%available = .true.
                else
                    combined%available = .false.
                end if
            end if
        end function

        function mempty(mold)
            type(maybe_t<T>), intent(in) :: mold
            type(maybe_t<T>) :: mempty

            mempty%available = .false.
        end function
    end if
end module
