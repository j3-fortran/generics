! dictionary.f90 --
!     Simple place holder:
!     Dictionaries are data structures that allow the user to retrieve pieces
!     of data via a key (typically a character string)
!
!     Dictionaries as intended here hold data of the same type, so that
!     various specific types may be needed. If they were to hold a variety
!     of data types, the nyou would need a way to distinguish the type per
!     retrieved piece of data. Unlimited polymorphic variables are useful
!     for that, but they do complicate matters.
!
!     Note:
!     Dictionaries are just one of commonly encountered data structures
!     where generic features are useful.
!
!     Methods (keep it at a minimum):
!     - add: to add a new element to the dictionary
!     - get/retrieve: to get an element by key
!     - has_key: to check that a particular key exists
!
!     The key itself: can be varied - instantiated as an array of integers
!     (usually it is a character string)
!
!     Instantiation: within the program
!     - integer, dimension(:), allocatable as the key
!     - some derived type with a user-defined assignment
!
!     (shows that you do not need to create a new module this way)
!
template dictionary
    implicit none

    !
    ! Get all the definitions and implementations in
    ! - a simple linked list will do for the current purposes
    ! (Nothing needs to be renamed)
    !
    ! Note:
    ! Like with modules, a template results in an intermediate
    ! file that can be used elsewhere.
    !
    use_template linked_lists

    !
    ! Define the requirements for the two (!) generic types
    ! We reuse everything from the template (including
    ! the routines, so that "data_type" in the template
    ! is replaced by "key_value_type")
    !
    type, generic :: key_type
    contains
        generic :: assignment(=)
        generic :: operator(.eq.)
    end type key_type

    type, generic :: value_type
    contains
        generic :: assignment(=)
    end type value_type

    !
    ! The items in the linked list should now hold both
    ! the key and the value
    !
    ! Note:
    ! This is an actual definition of the generic data type
    ! to be held in the container, but it is constructed
    ! from generic types. I have introduced the keyword
    ! "implements" to help the compiler distinguish.
    !
    ! Question: is that really necessary?
    !
    type, implements(data_type) :: key_value_type
        type(key_type)         :: key
        type(value_type)       :: value
    contains
        procedure :: equal => equal_key
    end type key_value_type

    type, extends(linked_list_def) :: dictionary_type
        type(key_value_type)   :: key_value
    contains
        procedure :: add => add_key_value      !! Still t obe added!
        procedure :: get => get_value
        procedure :: has_key => has_item_key
    end type dictionary_type

contains
logical function equal_key( item, key )
    class(key_value_type), intent(in) :: item
    type(key_type), intent(in)        :: key

    equal_key = (item%key_value%key == key)
end function equal_key

! Note:
! The compiler should probably be smart enough to see
! that no allocatable/pointer attribute is required,
! even if the data type "key_type" is defined that way.
!
logical function has_item_key( dictionary, key )
    class(dictionary_type), intent(in), target :: dictionary
    type(key_type), intent(in)                 :: key

    type(dictionary_type), pointer             :: item

    has_key =  .false.
    item    => dictionary
    do
        if ( item%key_value%key == key ) then
            has_key = .true.
            exit
        else
            if ( associated( item%next ) ) then
                item => item%next
            endif
        endif
    enddo
end function has_key

subroutine get_value( dictionary, key, value )
    class(dictionary_type), intent(in), target :: dictionary
    type(key_type), intent(in)                 :: key
    type(value_type), intent(out)              :: value

    type(dictionary_type), pointer             :: item

    item => dictionary
    do
        if ( item%key_value%key == key ) then
            value = item%key_value%value
            exit
        else
            if ( associated( item%next ) ) then
                item => item%next
            endif
        endif
    enddo
end subroutine get_value
end template dictionary

program test_dictionary
    implicit none

    type coordinate_type
        real :: x, y, z
    end type coordinate_type

    interface operator(.eq.)
        logical function equal_integers( array1, array2 )
            integer, dimension(:), intent(in) :: array1, array2
        end function
    end interface

    use_template dictionary, integer, dimension(:), allocatable => key_type, &
                             coordinate_type => value_type,
                             my_dictionary_type => dictionary_type

    type(my_dictionary_type) :: my_dictionary

    call my_dictionary%add( [1,2], coordinate_type(1.0,2.0,3.0) )
    call my_dictionary%add( [2,3], coordinate_type(2.0,3.0,4.0) )
    call my_dictionary%add( [3,4,5], coordinate_type(3.0,4.0,5.0) )

    write(*,*) 'Key: [1,2] - available? ', my_dictionary%has_key([1,2])
    if ( my_dictionary%has_key([1,2]) ) then
        write(*,*) 'Value for key [1,2] = ', my_dictionary%get([1,2])
    else
        write(*,*) 'Key [1,2] unknown'
    endif
contains
logical function equal_integers( array1, array2 )
    integer, dimension(:), intent(in) :: array1, array2

    if ( size(array1) == size(array2) ) then
        equal_integers = all( array1 == array2 )
    else
        equal_integers = .false.
    endif
end function equal_integers
end program test_dictionary
