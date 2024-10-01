! Note: All lines are legal unless a comment indicates otherwise

module m
    template tmpl{A}
        type, deferred :: A
        template inner{B}
            integer, deferred :: B
        end template
    ! contains   ! Illegal, template declarations do not have contains sections
    !    ...
    end template

    ! Note: tmpl2 is an example of wrapping an existing sub3 into
    ! a new template as an API grows, without breaking backwards
    ! compatability.
    template tmpl2{C}
        instantiate sub3{C}
    end template
contains
    template(tmpl) subroutine sub1(...)
        ! Example of legal code here:
        A :: obj
        instantiate inner{5}
    end template subroutine

    template(tmpl:inner) subroutine sub2(...)
        ! Example of legal code here:
        A :: obj
        print *, B
    end template subroutine

    template subroutine sub3{C}(...)
        type, deferred :: C
        ! Example of legal code here:
        C :: obj
        instantiate tmpl{C}
    end template subroutine
end module


subroutine usage1()
    use m
    instantiate tmpl{real}
    call sub1(...)
    call sub2(...) ! Illegal, sub2 not in scope
end subroutine

subroutine usage2()
    use m
    call sub3(...) ! Illegal unless C can be implied by arguments
    call sub3{integer}(...)
end subroutine

subroutine usage3()
    use m
    instantiate tmpl{real}
    instantiate inner{123}
    call sub2(...)
end subroutine

subroutine usage4()
    use m
    call tmpl{real}%sub1(...)
    call tmpl%sub1(...) ! Illegal unless A can be implied by arguments
    call tmpl{real}%inner{456}%sub2(...) ! Theoretically ok, but make illegal
                                         ! for readability/implementers' sanity
    call inner{456}%sub2(...) ! Illegal, inner not in scope
end subroutine

subroutine usage5()
    use m
    instantiate inner{123} ! Illegal, inner not in scope until after next line
    instantiate tmpl{real}
    call inner{456}%sub2(...)
    call inner%sub2(...) ! Illegal unless B can be implied by arguments,
                         ! which is impossible since B is an integer, so
                         ! this is always illegal.
end subroutine

subroutine usage6()
    use m
    instantiate sub3{integer}
    call sub3(...) ! Valid. C is always integer, regardless of arguments
    call sub3{real}(...)
    ! Note from an implementer's perspective: The two `sub3`s from the
    ! above two `call` statements would be two separate symbols. The 1st
    ! would be a standard procedure symbol, created by the `instantiate`
    ! statement on the previous line. The 2nd would refer to the template
    ! procedure itself, as if the instantiate line wasn't there.
end subroutine

subroutine usage7()
    use m
    instantiate tmpl2{character}
    call sub3(...) ! Valid. C is always character, just like usage6
    call sub3{real}(...) ! Valid. Like usage6, sub3 here refers to the
                         ! template procedure itself, not the instantiated
                         ! procedure acquired through tmpl2
    call tmpl2{character}%sub3(...)
    call tmpl2{character}%sub3{real}(...) ! Illegal: There is no template
                                          ! procedure sub3 within tmpl2,
                                          ! only an instantiated regular
                                          ! procedure sub3
end subroutine

subroutine usage8()
    use m
    instantiate tmpl{real}
    proc_with_proc_argument0(sub3) ! Illegal (I don't think we want to
                                   ! deal with templates being passed
                                   ! as actual arguments yet...)
    proc_with_proc_argument1(sub3{integer}) ! But rest here are valid!
    proc_with_proc_argument2(sub1)
    proc_with_proc_argument3(inner{789}%sub2)
end subroutine
