! printable_linkedlist.f90 --
!     Illustrate the use of nested templates and
!     of reusing a generic data type
!
!     We want the linked lists to have a print method
!
template printable_linked_lists
    implicit none

    !
    ! Get all the definitions and implementations in
    ! (Nothing needs to be renamed)
    !
    ! Note:
    ! Like with modules, a template results in an intermediate
    ! file that can be used elsewhere.
    !
    use_template linked_lists

    !
    ! Define the requirements for the generic type
    ! We reuse everything from the template (including
    ! the routines, so that "data_type" in the template
    ! is replaced by "printable_data_type")
    !
    type, replaces(data_type) :: printable_data_type
        ! No specific components required
    contains
        procedure(print_data_def) :: print_item
    end type printable_data_type

    !
    ! The signature of the print routine
    !
    abstract interface
        subroutine print_data_def( lun, item )
            integer, intent(in)       :: lun
            type(printable_data_type) :: item
        end subroutine print_data_def
    end interface

    !
    ! The new type we define, with the extra functionality
    ! is simply an extension to the one from the template
    !
    type, extends(linked_list_def) :: printable_list_def
        ! No components added
    contains
        procedure :: print => print_list
    end type printable_list_def
contains
subroutine print_list( list, lun )
    class(linked_list_def), intent(in), target :: list
    integer, intent(in)                        :: lun

    type(linked_list_def), pointer             :: item
    integer                                    :: i

    i = 1
    item => list
    do
        call print_item( lun, i, item%data )

        if ( associated( item%next ) ) then
            i = i + 1
            item => item%next
        else
            exit
        endif
    enddo
end subroutine print_list
end template printable_linked_lists

module linked_list_reals
    use_template printable_linked_lists, real => printable_data_type, linked_list_real => printable_list_def
    use_template printable_linked_lists, real, dimension(:), allocatable => printable_data_type, linked_list_multireal => printable_list_def

    !
    ! Make sure the two instantiated data types have the print method
    !
    interface print_item
        module procedure print_item_real
        module procedure print_item_real_array
    end interface

contains

subroutine print_item_real( lun, indx, data )
    integer, intent(in) :: lun
    integer, intent(in) :: indx
    real, intent(in) :: data

    write( lun, '(i5,a,e14.5)' ) indx, ':', data
end subroutine print_item_real

subroutine print_item_real_array( lun, indx, data )
    integer, intent(in) :: lun
    integer, intent(in) :: indx
    real, dimension(:), intent(in) :: data

    write( lun, '(i5,a,5e14.5,/,(6x,5e14.5))' ) indx, ':', data
end subroutine print_item_real_array

end module linked_list_reals

program test_linked_list
    use linked_list_reals

    type(linked_list_real)          :: mylist
    type(linked_list_multireal)     :: mylist_array
    integer                         :: i, j
    real                            :: value
    real, dimension(:), allocatable :: array

    do i = 1,10
        value = 0.1 * i
        call mylist%add( value )

        array = [(0.1 * j, j = 1,i)]
        call mylist_array%add( array )
    enddo

    open( 20, file = 'test_linked_list.out' )
    call mylist%print(20)
    call mylist_array%print(20)
end program test_linked_list
