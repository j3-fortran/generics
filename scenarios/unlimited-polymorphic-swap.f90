module unlimited_polymorphic_swap
  implicit none

contains

  subroutine swap(a, b)
    class(*), intent(inout), allocatable :: a, b
     !! 1. Not type-safe at compile-time (could be type-safe at runtime with same_type_as())
     !! 2. Necessiates allocatable attribute
    class(*), allocatable :: tmp
    tmp = a
    a = b
    b = tmp
  end subroutine

end module
