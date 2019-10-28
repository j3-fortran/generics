! simple_linkedlist.f90 --
!     Very basic implementation of linked lists
!     Main purpose: illustrate the use of templates
!
!     Two instantiations:
!     Real and real, dimension(:), allocatable
!
template linked_lists
    implicit none

    private
    public :: linked_list_def

    !
    ! Requirements on the generic type
    ! Only one:
    ! Copies of the data are stored, so we need assignment.
    ! Can be default or user-defined.
    !
    type, generic :: data_type
        ! No specific components
    contains
        generic :: assignment(=)
    end type data_type

    !
    ! Define the linked list type - it does not
    ! do all that much
    !
    type linked_list_def
        type(linked_list_def), pointer :: next
        type(data_type)                :: data
    contains
        procedure :: add   => add_to_list
    end type linked_list_def
contains
subroutine add_to_list( list, data )
    class(linked_list_def), intent(inout) :: list
    type(data_type), intent(in)           :: data

    type(linked_list_def), pointer        :: item

    allocate( item )
    item%data = data

    item%next => list%next
    list%next => item
end subroutine add_to_list
end template linked_lists

!
! Instantiation:
! Define a module that allows two different types of linked lists,
! one storing scalar real data and another storing real arrays of
! adjustable size.
!
! Note:
! The routine add_to_list will appear twice in the module with
! different signatures. To avoid a name clash, the actual name in
! the instantiation should be mangled - and hidden via a
! private statement.
!
module linked_list_reals
    use_template linked_lists, real => data_type, linked_list_real => linked_list_def
    use_template linked_lists, real, dimension(:), allocatable => data_type, linked_list_multireal => linked_list_def
end module linked_list_multireals

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
end program test_linked_list
