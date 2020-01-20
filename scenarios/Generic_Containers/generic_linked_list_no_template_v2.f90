! generic_linked_list_no_template_v2.f90 --
!     Experiment with procedure pointers and generic programming
!
!     The idea:
!     We have a general data type, unlimited polymorphics,
!     that can be used to store anything. But we need an interface
!     between the actual data types and the polymorphics.
!     So, define a container (a simple linked list) that stores
!     the generic data and subclass that translates between the
!     generic and the specific data types.
!
!     Use a generic method to access specific implementations.
!
!     The generic type linked_list_def can actually store data
!     of any type, but we want to make sure that only specific
!     data are stored, in this case: reals and strings.
!
module linked_lists
    implicit none

    private
    public :: linked_list_def

    !
    ! Define the linked list type - it does not
    ! do all that much
    !
    type linked_list_def
        type(linked_list_def), pointer :: next => null()
        class(*), allocatable          :: data
    contains
        procedure       :: add_data   => add_to_list
        procedure       :: print_list => print_items
        generic, public :: add        => add_data
    end type linked_list_def
contains
subroutine add_to_list( list, data, dummy )
    class(linked_list_def), intent(inout) :: list
    class(*), intent(in)                  :: data
    integer, intent(in)                   :: dummy ! Needed to disambiguate the generics -
                                                   ! rather ugly

    type(linked_list_def), pointer        :: item

    allocate( item )
    allocate( item%data, source = data )

    item%next => list%next
    list%next => item
end subroutine add_to_list

subroutine print_items( list, lun, print_item )
    class(linked_list_def), intent(in), target :: list
    integer, intent(in)                        :: lun

    !
    ! Interface to print_item
    !
    interface
        subroutine print_item( lun, idx, data )
            integer, intent(in)            :: lun
            integer, intent(in)            :: idx
            class(*), intent(in)           :: data
        end subroutine print_item
    end interface

    type(linked_list_def), pointer             :: item
    integer                                    :: i

    i = 0
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
end subroutine print_items


end module linked_lists

module linked_lists_real
    use linked_lists

    implicit none

    private
    public :: linked_list_real

    !
    ! Define the specific linked list type - it does not
    ! do all that much
    !
    type, extends(linked_list_def) :: linked_list_real
    contains
        procedure ::       print    => print_list_real
        procedure ::       add_real => add_to_list_real
        generic, public :: add      => add_real
    end type linked_list_real

contains
subroutine add_to_list_real( list, data )
    class(linked_list_real), intent(inout) :: list
    real, intent(in), target               :: data

    class(*), pointer                      :: pdata

    pdata => data
    call list%linked_list_def%add_data( pdata, 0 )
end subroutine add_to_list_real

subroutine print_list_real( list, lun )
    class(linked_list_real), intent(in)    :: list
    integer, intent(in)                    :: lun

    call list%linked_list_def%print_list( lun, print_item_real )
end subroutine print_list_real

subroutine print_item_real( lun, idx, data )
    integer, intent(in)                    :: lun
    integer, intent(in)                    :: idx
    class(*), intent(in)                   :: data

    select type (data)
        type is (real)
            write(*,*) idx, data
    end select
end subroutine print_item_real

! Assumed-type arguments do not quite fit here ...
!
!subroutine add_to_list_any( list, data )
!    class(linked_list_real), intent(inout) :: list
!    type(*), intent(in), target            :: data
!
!    class(*), pointer                      :: pdata
!
!    !pdata => data
!    call list%add( data )
!end subroutine add_to_list_any

end module linked_lists_real

module linked_lists_string
    use linked_lists

    implicit none

    private
    public :: linked_list_string

    !
    ! Define the linked list type - it does not
    ! do all that much
    !
    type, extends(linked_list_def) :: linked_list_string
    contains
        procedure       :: print      => print_list_string
        procedure       :: add_string => add_to_list_string
        generic, public :: add        => add_string
    end type linked_list_string

contains
subroutine add_to_list_string( list, data )
    class(linked_list_string), intent(inout) :: list
    character(len=*), intent(in), target   :: data

    class(*), pointer                      :: pdata

    pdata => data
    call list%linked_list_def%add_data( pdata, 0 )
end subroutine add_to_list_string

subroutine print_list_string( list, lun )
    class(linked_list_string), intent(in)  :: list
    integer, intent(in)                    :: lun

    call list%linked_list_def%print_list( lun, print_item_string )
end subroutine print_list_string

subroutine print_item_string( lun, idx, data )
    integer, intent(in)                    :: lun
    integer, intent(in)                    :: idx
    class(*), intent(in)                   :: data

    select type (data)
        type is (character(len=*))
            write(*,*) idx, data
    end select
end subroutine print_item_string

end module linked_lists_string

program procpoint_v2
    use linked_lists_real
    use linked_lists_string

    type(linked_list_real)   :: list
    type(linked_list_string) :: list_strings

    call list%add( 1.0 )
    call list%add( 2.0 )
    call list%add( 3.0 )

    call list%print( 10 )

    call list_strings%add( "A" )
    call list_strings%add( "B" )
    call list_strings%add( "C" )
    call list_strings%print( 10 )

end program procpoint_v2
