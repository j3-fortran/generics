This document is intended to be a list of requirements that are to be distilled from the various use cases.   Ideally this will drive the creation of a formal paper for generics that will be submitted to the Fortran committee.

Each requirement should specify one or more motivating use cases.

Requirement:   Provide a mechanism to associate actual arguments possessing attributes (e.g., dimension, pointer, allocatable, ...) with template procedure dummy arguments declared as simple types that wrap the actual type.

Use cases:
   - Containers with elements that are arrays, or pointers

Example:

     TYPE :: ARRAY_WRAP_1D<T>
        TYPE(T), ALLOCATABLE :: data(:)
     END TYPE
     
     SUBROUTINE S<T>(x)
         TYPE(T) :: x
         ...
     END SUBROUTINE
     
     ...
     
     REAL :: x(5)
     x = [1,2,3,4,5]
     ! component "data" of template type T 
     ! argument associated with actual x
     CALL S<ARRAY_WRAP_1D<REAL>>(x)
     
Potential approach:  introduce new type attribute "WRAPPER" or "ASSOCIATED".  Such types can only have one data component, and allow all operations permitted for that contained data component.

     TYPE, WRAPPER :: ARRAY_WRAP_1D<T>
        TYPE(T) :: data(:) ! Note neither allocatable, nor pointer
     END TYPE
     
Type ARRAY_WRAP_1D<REAL>  supports all operators supported on real arrays.  But it does not support indexing.
