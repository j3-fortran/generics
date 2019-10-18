module multi
  implicit none

  type, abstract :: T
  end type T

  type, abstract :: U
  end type U

  interface T
     module procedure cast_from_u
  end interface T


end module multi
