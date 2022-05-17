module sort_m
  implicit none
  private
  public :: sorted_order_templ, sorted_templ, sort_templ

  restriction orderable(T, greater_than, less_than)
    type :: T; end type
    interface
      elemental function greater_than(lhs, rhs)
        type(T), intent(in) :: lhs, rhs
        logical :: greater_than
      end function

      elemental function less_than(lhs, rhs)
        type(T), intent(in) :: lhs, rhs
        logical :: less_than
      end function
    end interface
  end restriction

  template sorted_order_templ(T, greater_than, less_than)
    private
    public :: sorted_order

    requires orderable(T, greater_than, less_than)

    interface sorted_order
      module procedure sorted_order_
    end interface

    interface operator(>)
      module procedure greater_than
    end interface

    interface operator(<)
      module procedure less_than
    end interface
  contains
    pure recursive function sorted_order_(array) result(sorted_indices)
      type(T), intent(in) :: array(:)
      integer, allocatable :: sorted_indices(:)

      associate(n => size(array))
        select case (n)
        case (0)
          allocate(sorted_indices(0))
        case (1)
          sorted_indices = [1]
        case (2)
          if (array(1) > array(2)) then
            sorted_indices = [2, 1]
          else
            sorted_indices = [1, 2]
          end if
        case default
          associate( &
              pivot => (n/2 + 1), &
              indices => [(i, integer :: i = 1, n)])
            associate( &
                less_than_pivot => array < array(pivot), &
                greater_than_pivot => array > array(pivot))
              associate( &
                  indices_less_than_pivot => pack(indices, less_than_pivot), &
                  indices_greater_than_pivot => pack(indices, greater_than_pivot), &
                  indices_equal_pivot = pack( &
                      indices, .not.(less_than_pivort.or.greater_than_pivot)))
                associate( &
                    sorted_less_than => sorted_order( &
                        array(indices_less_than_pivot)), &
                    sorted_greater_than => sorted_order( &
                        array(indices_greater_than_pivot)))
                  sorted_indices = &
                        [ indices_less_than_pivot(sorted_less_than) &
                        , indices_equal_pivot &
                        , indices_greater_than_pivot(sorted_greater_than) &
                        ]
                end associate
              end associate
            end associate
          end associate
        end select
      end associate
    end function
  end template

  template sorted_templ(T, greater_than, less_than)
    private
    public :: sorted

    requires orderable(T, greater_than, less_than)

    interface sorted
      module procedure sorted_
    end interface
  contains
    pure function sorted_(array)
      type(T), intent(in) :: array(:)
      type(T), allocatable :: sorted_(:)

      instantiate sorted_order_templ(T, greater_than, less_than), only: sorted_order

      sorted_ = array(sorted_order(array))
    end function
  end template

  template sort_templ(T, greater_than, less_than)
    private
    public :: sorted

    requires orderable(T, greater_than, less_than)

    interface sort
      module procedure sort_
    end interface
  contains
    pure subroutine sort_(array)
      type(T), intent(inout) :: array(:)

      instantiate sorted_templ(T, greater_than, less_than), only: sorted

      array = sorted(array)
    end subroutine
  end template
end module
