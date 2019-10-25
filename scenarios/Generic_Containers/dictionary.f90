! dictionary.f90 --
!     Simple place holder:
!     Dictionaries are data structures that allow the user to retrieve pieces
!     of data via a key (typically a character string)
!
!     Dictionaries ass intended here hold data of the same type, so that
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
!     The key itself: character(len=:), allocatable :: key
!     Instantiations within ONE module:
!     - integer, dimension(:), allocatable
!     - some derived type with a user-defined assignment
!
!     And the requirements for the data type must be listed
!     (assignment in this case)
!
