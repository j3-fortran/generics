module sort_m
  implicit none
  private
  public :: sort_tmpl

  requirement orderable(T, greater_than, less_than)
    type, deferred :: T

    elemental function greater_than(lhs, rhs)
      type(T), intent(in) :: lhs, rhs
      logical :: greater_than
    end function

    elemental function less_than(lhs, rhs)
      type(T), intent(in) :: lhs, rhs
      logical :: less_than
    end function
  end restriction

  template sort_tmpl(T, greater_than, less_than)
    private
    public :: sorted_order, sorted, sort, &
              reverse_sorted_order, reverse_sorted, reverse_sort

    requires orderable(T, greater_than, less_than)

    template inner_sort_tmpl(T, greater_than, less_than)
      private
      public :: sorted_order, sorted, sort

      interface sorted_order
        procedure sorted_order_
      end interface

      interface operator(>)
        procedure greater_than
      end interface

      interface operator(<)
        procedure less_than
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

      pure function sorted_(array)
        type(T), intent(in) :: array(:)
        type(T), allocatable :: sorted_(:)

        sorted_ = array(sorted_order(array))
      end function

      pure subroutine sort_(array)
        type(T), intent(inout) :: array(:)

        array = sorted(array)
      end subroutine
    end template inner_sort_tmpl

    instantiate inner_sort_tmpl(T, greater_than, less_than)
    instantiate inner_sort_tmpl(T, less_than, greater_than), &
        reverse_sorted_order => sorted_order, &
        reverse_sorted => sorted, &
        reverse_sort => sort
  end template sort_tmpl
end module
