module GenericIntrinsics_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private



   ! Ordered according to promotion rules:  binary operations
   ! result in the type which is later in the list.

   
   public :: GenericInteger
   public :: GenericReal
   public :: GenericComplex


   integer, parameter :: DEFAULT_INTEGER_KIND = kind(1)
   integer, parameter :: DEFAULT_REAL_KIND = kind(1.)
   
   type :: GenericInteger(kind)
      integer, kind :: kind = DEFAULT_INTEGER_KIND
      integer(kind=kind) :: value
   contains
      procedure :: plus_int_int
      procedure :: plus_int_real
      procedure :: plus_int_complex
      generic :: operator(+) => plus_int_int
      generic :: operator(+) => plus_int_real
      generic :: operator(+) => plus_int_complex
      procedure :: times_int_int
      procedure :: times_int_real
      procedure :: times_int_complex
      generic :: operator(*) => times_int_int
      generic :: operator(*) => times_int_real
      generic :: operator(*) => times_int_complex
   end type GenericInteger

   type :: GenericReal(kind)
      integer, kind :: kind = DEFAULT_REAL_KIND
      real(kind=kind) :: value
   contains
      procedure :: plus_real_int
      procedure :: plus_real_real
      procedure :: plus_real_complex
      generic :: operator(+) => plus_real_int
      generic :: operator(+) => plus_real_real
      generic :: operator(+) => plus_real_complex
      procedure :: times_real_int
      procedure :: times_real_real
      procedure :: times_real_complex
      generic :: operator(*) => times_real_int
      generic :: operator(*) => times_real_real
      generic :: operator(*) => times_real_complex
   end type GenericReal
      
   type :: GenericComplex(kind)
      integer, kind :: kind = DEFAULT_REAL_KIND
      complex(kind=kind) :: value
   contains
      procedure :: plus_complex_int
      procedure :: plus_complex_real
      procedure :: plus_complex_complex
      generic :: operator(+) => plus_complex_int
      generic :: operator(+) => plus_complex_real
      generic :: operator(+) => plus_complex_complex
      procedure :: times_complex_int
      procedure :: times_complex_real
      procedure :: times_complex_complex
      generic :: operator(*) => times_complex_int
      generic :: operator(*) => times_complex_real
      generic :: operator(*) => times_complex_complex
   end type GenericComplex
      



contains


   !=================
   ! Integer add
   !=================

   function plus_int_int(a, b) result(c)
      class(GenericInteger(*)), intent(in) :: a
      type(GenericInteger(kind=*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, range(a%value) > range(b%value))
      type(GenericInteger(kind=RESULT_KIND)) :: c
      c%value = a%value + b%value
   end function plus_int_int
      
   function plus_int_real(a, b) result(c)
      class(GenericInteger(*)), intent(in) :: a
      type(GenericReal(*)), intent(in) :: b
      type(GenericReal(kind=b%kind)) :: c
      c%value = a%value + b%value
   end function plus_int_real
      
   function plus_int_complex(a, b) result(c)
      class(GenericInteger(*)), intent(in) :: a
      type(GenericComplex(*)), intent(in) :: b
      type(GenericComplex(kind=b%kind)) :: c
      c%value = a%value + b%value
   end function plus_int_complex
      

   !=================
   ! Integer multiply
   !=================

   function times_int_int(a, b) result(c)
      class(GenericInteger(*)), intent(in) :: a
      type(GenericInteger(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, range(a%value) > range(b%value))
      type(GenericInteger(kind=RESULT_KIND)) :: c
      c%value = a%value * b%value
   end function times_int_int
      
   function times_int_real(a, b) result(c)
      class(GenericInteger(*)), intent(in) :: a
      type(GenericReal(*)), intent(in) :: b
      type(GenericReal(kind=b%kind)) :: c
      c%value = a%value * b%value
   end function times_int_real
      
      
   function times_int_complex(a, b) result(c)
      class(GenericInteger(*)), intent(in) :: a
      type(GenericComplex(*)), intent(in) :: b
      type(GenericComplex(kind=b%kind)) :: c
      c%value = a%value * b%value
   end function times_int_complex
      

   !=================
   ! Real add
   !=================

   function plus_real_int(a, b) result(c)
      class(GenericReal(*)), intent(in) :: a
      type(GenericInteger(*)), intent(in) :: b
      type(GenericReal(kind=a%kind)) :: c
      c%value = a%value + b%value
   end function plus_real_int
      
   function plus_real_real(a, b) result(c)
      class(GenericReal(*)), intent(in) :: a
      type(GenericReal(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericReal(kind=RESULT_KIND)) :: c
      c%value = a%value + b%value
   end function plus_real_real
      
   function plus_real_complex(a, b) result(c)
      class(GenericReal(*)), intent(in) :: a
      type(GenericComplex(*))), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericComplex(kind=RESULT_KIND)) :: c
      c%value = a%value + b%value
   end function plus_real_complex
      

   !=================
   ! Real multiply
   !=================


   function times_real_int(a, b) result(c)
      class(GenericReal(*)), intent(in) :: a
      type(GenericInteger(*)), intent(in) :: b
      type(GenericReal(kind=a%kind)) :: c
      c%value = a%value * b%value
   end function times_real_int
      
   function times_real_real(a, b) result(c)
      class(GenericReal(*)), intent(in) :: a
      type(GenericReal(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericReal(kind=RESULT_KIND)) :: c
      c%value = a%value * b%value
   end function times_real_real
      
   function times_real_complex(a, b) result(c)
      class(GenericReal(*)), intent(in) :: a
      type(GenericComplex(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericComplex(kind=RESULT_KIND)) :: c
      c%value = a%value * b%value
   end function times_real_complex
      
   !=================
   ! Complex add
   !=================

   function plus_complex_int(a, b) result(c)
      class(GenericComplex(*)), intent(in) :: a
      type(GenericInteger(*)), intent(in) :: b
      type(GenericComplex(kind=a%kind)) :: c
      c%value = a%value + b%value
   end function plus_complex_int
      
   function plus_complex_real(a, b) result(c)
      class(GenericComplex(*)), intent(in) :: a
      type(GenericReal(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericComplex(kind=RESULT_KIND)) :: c
      c%value = a%value + b%value
   end function plus_complex_real
      
   function plus_complex_complex(a, b) result(c)
      class(GenericComplex(*)), intent(in) :: a
      type(GenericComplex(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericComplex(kind=RESULT_KIND)) :: c
      c%value = a%value + b%value
   end function plus_complex_complex
      

   !=================
   ! Complex multiply
   !=================

   function times_complex_int(a, b) result(c)
      class(GenericComplex(*)), intent(in) :: a
      type(GenericInteger(*)), intent(in) :: b
      type(GenericComplex(kind=a%kind)) :: c
      c%value = a%value * b%value
   end function times_complex_int
      
   function times_complex_real(a, b) result(c)
      class(GenericComplex(*)), intent(in) :: a
      type(GenericReal(*)), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericComplex(kind=RESULT_KIND)) :: c
      c%value = a%value * b%value
   end function times_complex_real
      
   function times_complex_complex(a, b) result(c)
      class(GenericComplex(*)), intent(in) :: a
      type(GenericComplex(*))), intent(in) :: b
      integer, parameter :: RESULT_KIND = merge(a%kind, b%kind, precision(a%value) > precision(b%value))
      type(GenericComplex(kind=RESULT_KIND)) :: c
      c%value = a%value * b%value
   end function times_complex_complex
      
      
end module GenericIntrinsics_mod
