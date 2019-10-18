subroutine set(s)
  type(T), intent(in) ::s
  ...
end subroutine set

function get() result(s)
  type(T) ::s
  ...
end function get

subroutine set(s)
  character(*), intent(in) :: s
  ...
end subroutine set

function get() result(s)
  character(:), allocatable :: s
  ...
end function get
