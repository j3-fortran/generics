module GenericIntrinsics_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private


   ! With slight simplification, the F2018 standard defines 4 "numeric"
   ! intrisics that must be supported by standard conforming processors.
   ! Thes are INTEGER, REAL, DOUBLE PRECISION, and COMPLEX .   The standard
   ! also requires at least one long integer:
   ! "The processor shall provide at least one representation method
   !   with a decimal exponent range greater than or equal to 18."
   ! This may or not be the same as the default INTEGER.  Here we
   ! assume that it is a different kind that can be accessed by INT64,
   ! from ISO_FORTRAN_ENV.
   ! (This is the case with all existing compilers that support F2008.)

   ! The standard also requires a complex kind corresponding to double
   ! precision.

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
      procedure :: plus_int_int64
      procedure :: plus_int_real
      procedure :: plus_int_real_dp
      procedure :: plus_int_complex
      procedure :: plus_int_complex_dp
      procedure :: plus_int64_int
      procedure :: plus_int64_int64
      procedure :: plus_int64_real
      procedure :: plus_int64_real_dp
      procedure :: plus_int64_complex
      procedure :: plus_int64_complex_dp
      generic :: operator(+) => plus_int_int
      generic :: operator(+) => plus_int_int64
      generic :: operator(+) => plus_int_real
      generic :: operator(+) => plus_int_real_dp
      generic :: operator(+) => plus_int_complex
      generic :: operator(+) => plus_int_complex_dp
      generic :: operator(+) => plus_int64_int
      generic :: operator(+) => plus_int64_int64
      generic :: operator(+) => plus_int64_real
      generic :: operator(+) => plus_int64_real_dp
      generic :: operator(+) => plus_int64_complex
      generic :: operator(+) => plus_int64_complex_dp
      procedure :: times_int_int
      procedure :: times_int_int64
      procedure :: times_int_real
      procedure :: times_int_real_dp
      procedure :: times_int_complex
      procedure :: times_int_complex_dp
      procedure :: times_int64_int
      procedure :: times_int64_int64
      procedure :: times_int64_real
      procedure :: times_int64_real_dp
      procedure :: times_int64_complex
      procedure :: times_int64_complex_dp
      generic :: operator(*) => times_int_int
      generic :: operator(*) => times_int_int64
      generic :: operator(*) => times_int_real
      generic :: operator(*) => times_int_real_dp
      generic :: operator(*) => times_int_complex
      generic :: operator(*) => times_int_complex_dp
      generic :: operator(*) => times_int64_int
      generic :: operator(*) => times_int64_int64
      generic :: operator(*) => times_int64_real
      generic :: operator(*) => times_int64_real_dp
      generic :: operator(*) => times_int64_complex
      generic :: operator(*) => times_int64_complex_dp
   end type GenericInteger

   type :: GenericReal(kind)
      integer, kind :: kind = DEFAULT_REAL_KIND
      real(kind=kind) :: value
   contains
      procedure :: plus_real_int
      procedure :: plus_real_int64
      procedure :: plus_real_real
      procedure :: plus_real_real_dp
      procedure :: plus_real_complex
      procedure :: plus_real_complex_dp
      procedure :: plus_real_dp_int
      procedure :: plus_real_dp_int64
      procedure :: plus_real_dp_real
      procedure :: plus_real_dp_real_dp
      procedure :: plus_real_dp_complex
      procedure :: plus_real_dp_complex_dp
      generic :: operator(+) => plus_real_int
      generic :: operator(+) => plus_real_int64
      generic :: operator(+) => plus_real_real
      generic :: operator(+) => plus_real_real_dp
      generic :: operator(+) => plus_real_complex
      generic :: operator(+) => plus_real_complex_dp
      generic :: operator(+) => plus_real_dp_int
      generic :: operator(+) => plus_real_dp_int64
      generic :: operator(+) => plus_real_dp_real
      generic :: operator(+) => plus_real_dp_real_dp
      generic :: operator(+) => plus_real_dp_complex
      generic :: operator(+) => plus_real_dp_complex_dp
      procedure :: times_real_int
      procedure :: times_real_int64
      procedure :: times_real_real
      procedure :: times_real_real_dp
      procedure :: times_real_complex
      procedure :: times_real_complex_dp
      procedure :: times_real_dp_int
      procedure :: times_real_dp_int64
      procedure :: times_real_dp_real
      procedure :: times_real_dp_real_dp
      procedure :: times_real_dp_complex
      procedure :: times_real_dp_complex_dp
      generic :: operator(*) => times_real_int
      generic :: operator(*) => times_real_int64
      generic :: operator(*) => times_real_real
      generic :: operator(*) => times_real_real_dp
      generic :: operator(*) => times_real_complex
      generic :: operator(*) => times_real_complex_dp
      generic :: operator(*) => times_real_dp_int
      generic :: operator(*) => times_real_dp_int64
      generic :: operator(*) => times_real_dp_real
      generic :: operator(*) => times_real_dp_real_dp
      generic :: operator(*) => times_real_dp_complex
      generic :: operator(*) => times_real_dp_complex_dp
   end type GenericReal
      
   type :: GenericComplex(kind)
      integer, kind :: kind = DEFAULT_REAL_KIND
      complex(kind=kind) :: value
   contains
      procedure :: plus_complex_int
      procedure :: plus_complex_int64
      procedure :: plus_complex_real
      procedure :: plus_complex_real_dp
      procedure :: plus_complex_complex
      procedure :: plus_complex_complex_dp
      procedure :: plus_complex_dp_int
      procedure :: plus_complex_dp_int64
      procedure :: plus_complex_dp_real
      procedure :: plus_complex_dp_real_dp
      procedure :: plus_complex_dp_complex
      procedure :: plus_complex_dp_complex_dp
      generic :: operator(+) => plus_complex_int
      generic :: operator(+) => plus_complex_int64
      generic :: operator(+) => plus_complex_real
      generic :: operator(+) => plus_complex_real_dp
      generic :: operator(+) => plus_complex_complex
      generic :: operator(+) => plus_complex_complex_dp
      generic :: operator(+) => plus_complex_dp_int
      generic :: operator(+) => plus_complex_dp_int64
      generic :: operator(+) => plus_complex_dp_real
      generic :: operator(+) => plus_complex_dp_real_dp
      generic :: operator(+) => plus_complex_dp_complex
      generic :: operator(+) => plus_complex_dp_complex_dp
      procedure :: times_complex_int
      procedure :: times_complex_int64
      procedure :: times_complex_real
      procedure :: times_complex_real_dp
      procedure :: times_complex_complex
      procedure :: times_complex_complex_dp
      procedure :: times_complex_dp_int
      procedure :: times_complex_dp_int64
      procedure :: times_complex_dp_real
      procedure :: times_complex_dp_real_dp
      procedure :: times_complex_dp_complex
      procedure :: times_complex_dp_complex_dp
      generic :: operator(*) => times_complex_int
      generic :: operator(*) => times_complex_int64
      generic :: operator(*) => times_complex_real
      generic :: operator(*) => times_complex_real_dp
      generic :: operator(*) => times_complex_complex
      generic :: operator(*) => times_complex_complex_dp
      generic :: operator(*) => times_complex_dp_int
      generic :: operator(*) => times_complex_dp_int64
      generic :: operator(*) => times_complex_dp_real
      generic :: operator(*) => times_complex_dp_real_dp
      generic :: operator(*) => times_complex_dp_complex
      generic :: operator(*) => times_complex_dp_complex_dp
   end type GenericComplex
      



contains


   !=================
   ! Integer add
   !=================

   function plus_int_int(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericInteger(kind=a%kind)) :: c
      c%value = a%value + b%value
   end function plus_int_int
      
   function plus_int_int64(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericInteger(kind=INT64)) :: c
      c%value = a%value + b%value
   end function plus_int_int64
      
   function plus_int_real(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_int_real
      
   function plus_int_real_dp(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_int_real_dp
      
   function plus_int_complex(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_int_complex
      
   function plus_int_complex_dp(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_int_complex_dp
      
   function plus_int64_int(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericInteger(kind=INT64)) :: c
      c%value = a%value + b%value
   end function plus_int64_int
      
   function plus_int64_int64(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericInteger(kind=INT64)) :: c
      c%value = a%value + b%value
   end function plus_int64_int64
      
   function plus_int64_real(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_int64_real
      
   function plus_int64_real_dp(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_int64_real_dp
      
   function plus_int64_complex(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_int64_complex

   function plus_int64_complex_dp(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_int64_complex_dp

   !=================
   ! Integer multiply
   !=================


   function times_int_int(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericInteger(kind=kind(1))) :: c
      c%value = a%value * b%value
   end function times_int_int
      
   function times_int_int64(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericInteger(kind=INT64)) :: c
      c%value = a%value * b%value
   end function times_int_int64
      
   function times_int_real(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_int_real
      
   function times_int_real_dp(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_int_real_dp
      
   function times_int_complex(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_int_complex
      
   function times_int_complex_dp(a, b) result(c)
      class(GenericInteger(kind=kind(1))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_int_complex_dp
   

   function times_int64_int(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericInteger(kind=INT64)) :: c
      c%value = a%value * b%value
   end function times_int64_int
      
   function times_int64_int64(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericInteger(kind=INT64)) :: c
      c%value = a%value * b%value
   end function times_int64_int64
      
   function times_int64_real(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_int64_real
      
   function times_int64_real_dp(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_int64_real_dp
      
   function times_int64_complex(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_int64_complex
      
   function times_int64_complex_dp(a, b) result(c)
      class(GenericInteger(kind=INT64)), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_int64_complex_dp
      

   !=================
   ! Real add
   !=================

   function plus_real_int(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_real_int
      
   function plus_real_int64(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_real_int64
      
   function plus_real_real(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_real_real
      
   function plus_real_real_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_real_dp
      
   function plus_real_complex(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_real_complex
      
   function plus_real_complex_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_complex_dp
      
   function plus_real_dp_int(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_dp_int
      
   function plus_real_dp_int64(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_dp_int64
      
   function plus_real_dp_real(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_dp_real
      
   function plus_real_dp_real_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_dp_real_dp
      
   function plus_real_dp_complex(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_dp_complex
      
   function plus_real_dp_complex_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_real_dp_complex_dp
      

   !=================
   ! Real multiply
   !=================

   function times_real_int(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_real_int
      
   function times_real_int64(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_real_int64
      
   function times_real_real(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_real_real
      
   function times_real_real_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_real_dp
      
   function times_real_complex(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_real_complex
      
   function times_real_complex_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_complex_dp
      
   function times_real_dp_int(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_dp_int
      
   function times_real_dp_int64(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_dp_int64
      
   function times_real_dp_real(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_dp_real
      
   function times_real_dp_real_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericReal(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_dp_real_dp
      
   function times_real_dp_complex(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_dp_complex
      
   function times_real_dp_complex_dp(a, b) result(c)
      class(GenericReal(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_real_dp_complex_dp
      
   !=================
   ! Complex add
   !=================

   function plus_complex_int(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_complex_int
      
   function plus_complex_int64(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      integer, parameter :: RESULT_KIND = a%kind
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_complex_int64
      
   function plus_complex_real(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_complex_real
      
   function plus_complex_real_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_real_dp
      
   function plus_complex_complex(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value + b%value
   end function plus_complex_complex
      
   function plus_complex_complex_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_complex_dp
      

   function plus_complex_dp_int(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_dp_int
      
   function plus_complex_dp_int64(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      integer, parameter :: RESULT_KIND = a%kind
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_dp_int64
      
   function plus_complex_dp_real(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_dp_real
      
   function plus_complex_dp_real_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_dp_real_dp
      
   function plus_complex_dp_complex(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_dp_complex
      
   function plus_complex_dp_complex_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value + b%value
   end function plus_complex_dp_complex_dp
      

   !=================
   ! Complex multiply
   !=================

   function times_complex_int(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_complex_int
      
   function times_complex_int64(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_complex_int64
      
   function times_complex_real(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_complex_real
      
   function times_complex_real_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_real_dp
      
   function times_complex_complex(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.))) :: c
      c%value = a%value * b%value
   end function times_complex_complex
      
   function times_complex_complex_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_complex_dp
      
   function times_complex_dp_int(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=kind(1))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_dp_int
      
   function times_complex_dp_int64(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericInteger(kind=INT64)), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_dp_int64
      
   function times_complex_dp_real(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_dp_real
      
   function times_complex_dp_real_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericReal(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_dp_real_dp
      
   function times_complex_dp_complex(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_dp_complex
      
   function times_complex_dp_complex_dp(a, b) result(c)
      class(GenericComplex(kind=kind(1.d0))), intent(in) :: a
      type(GenericComplex(kind=kind(1.d0))), intent(in) :: b
      type(GenericComplex(kind=kind(1.d0))) :: c
      c%value = a%value * b%value
   end function times_complex_dp_complex_dp
      
      
end module GenericIntrinsics_mod
