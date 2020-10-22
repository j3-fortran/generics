module GenericIntrinsics_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private


   ! With slight simplification, the F2018 standard defines 4 "numeric"
   ! intrisics that must be supported by standard conforming processors.
   ! Thes are INTEGER, REAL, DOUBLE PRECISION, and COMPLEX.   The standard
   ! also requires at least one long integer:
   ! "The processor shall provide at least one representation method
   !   with a decimal exponent range greater than or equal to 18."
   ! This may or not be the same as the default INTEGER.  Here we
   ! assume that it is a different kind that can be accessed by INT64,
   ! from ISO_FORTRAN_ENV.
   ! (This is the case with all existing compilers that support F2008.)


   ! Ordered according to promotion rules:  binary operations
   ! result in the type which is later in the list.

   
   public :: GenericInteger
   public :: GenericInteger64
   public :: GenericReal
   public :: GenericDoublePrecision
   public :: GenericComplex


   type :: GenericInteger
      integer :: value
   contains
      procedure :: plus_int_int
      procedure :: plus_int_int64
      procedure :: plus_int_real
      procedure :: plus_int_dp
      procedure :: plus_int_complex
      generic :: operator(+) => plus_int_int
      generic :: operator(+) => plus_int_int64
      generic :: operator(+) => plus_int_real
      generic :: operator(+) => plus_int_dp
      generic :: operator(+) => plus_int_complex
      procedure :: times_int_int
      procedure :: times_int_int64
      procedure :: times_int_real
      procedure :: times_int_dp
      procedure :: times_int_complex
      generic :: operator(*) => times_int_int
      generic :: operator(*) => times_int_int64
      generic :: operator(*) => times_int_real
      generic :: operator(*) => times_int_dp
      generic :: operator(*) => times_int_complex
   end type GenericInteger

   type :: GenericInteger64
      integer(kind=INT64) :: value
   contains
      procedure :: plus_int64_int
      procedure :: plus_int64_int64
      procedure :: plus_int64_real
      procedure :: plus_int64_dp
      procedure :: plus_int64_complex
      generic :: operator(+) => plus_int64_int
      generic :: operator(+) => plus_int64_int64
      generic :: operator(+) => plus_int64_real
      generic :: operator(+) => plus_int64_dp
      generic :: operator(+) => plus_int64_complex
      procedure :: times_int64_int
      procedure :: times_int64_int64
      procedure :: times_int64_real
      procedure :: times_int64_dp
      procedure :: times_int64_complex
      generic :: operator(*) => times_int64_int
      generic :: operator(*) => times_int64_int64
      generic :: operator(*) => times_int64_real
      generic :: operator(*) => times_int64_dp
      generic :: operator(*) => times_int64_complex
   end type GenericInteger64
      
      
   type :: GenericReal
      real :: value
   contains
      procedure :: plus_real_int
      procedure :: plus_real_int64
      procedure :: plus_real_real
      procedure :: plus_real_dp
      procedure :: plus_real_complex
      generic :: operator(+) => plus_real_int
      generic :: operator(+) => plus_real_int64
      generic :: operator(+) => plus_real_real
      generic :: operator(+) => plus_real_dp
      generic :: operator(+) => plus_real_complex
      procedure :: times_real_int
      procedure :: times_real_int64
      procedure :: times_real_real
      procedure :: times_real_dp
      procedure :: times_real_complex
      generic :: operator(*) => times_real_int
      generic :: operator(*) => times_real_int64
      generic :: operator(*) => times_real_real
      generic :: operator(*) => times_real_dp
      generic :: operator(*) => times_real_complex
   end type GenericReal
      
   type :: GenericDoublePrecision
      double precision :: value
   contains
      procedure :: plus_dp_int
      procedure :: plus_dp_int64
      procedure :: plus_dp_real
      procedure :: plus_dp_dp
      procedure :: plus_dp_complex
      generic :: operator(+) => plus_dp_int
      generic :: operator(+) => plus_dp_int64
      generic :: operator(+) => plus_dp_real
      generic :: operator(+) => plus_dp_dp
      generic :: operator(+) => plus_dp_complex
      procedure :: times_dp_int
      procedure :: times_dp_int64
      procedure :: times_dp_real
      procedure :: times_dp_dp
      procedure :: times_dp_complex
      generic :: operator(*) => times_dp_int
      generic :: operator(*) => times_dp_int64
      generic :: operator(*) => times_dp_real
      generic :: operator(*) => times_dp_dp
      generic :: operator(*) => times_dp_complex
   end type GenericDoublePrecision



   type :: GenericComplex
      complex :: value
   contains
      procedure :: plus_complex_int
      procedure :: plus_complex_int64
      procedure :: plus_complex_real
      procedure :: plus_complex_dp
      procedure :: plus_complex_complex
      generic :: operator(+) => plus_complex_int
      generic :: operator(+) => plus_complex_int64
      generic :: operator(+) => plus_complex_real
      generic :: operator(+) => plus_complex_dp
      generic :: operator(+) => plus_complex_complex
      procedure :: times_complex_int
      procedure :: times_complex_int64
      procedure :: times_complex_real
      procedure :: times_complex_dp
      procedure :: times_complex_complex
      generic :: operator(*) => times_complex_int
      generic :: operator(*) => times_complex_int64
      generic :: operator(*) => times_complex_real
      generic :: operator(*) => times_complex_dp
      generic :: operator(*) => times_complex_complex
   end type GenericComplex
      


contains


   !=================
   ! Integer add
   !=================

   function plus_int_int(a, b) result(c)
      type(GenericInteger) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int_int
      
   function plus_int_int64(a, b) result(c)
      type(GenericInteger64) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int_int64
      
   function plus_int_real(a, b) result(c)
      type(GenericReal) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int_real

   function plus_int_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int_dp
   
   function plus_int_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int_complex

   !=================
   ! Integer multiply
   !=================

   function times_int_int(a, b) result(c)
      type(GenericInteger) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value * b%value
   end function times_int_int
      
   function times_int_int64(a, b) result(c)
      type(GenericInteger64) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value * b%value
   end function times_int_int64
      
   function times_int_real(a, b) result(c)
      type(GenericReal) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value * b%value
   end function times_int_real

   function times_int_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value * b%value
   end function times_int_dp
   
   function times_int_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericInteger), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value * b%value
   end function times_int_complex
      

   !=================
   ! Integer64 add
   !=================

   function plus_int64_int(a, b) result(c)
      type(GenericInteger64) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int64_int
      
   function plus_int64_int64(a, b) result(c)
      type(GenericInteger64) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int64_int64
      
   function plus_int64_real(a, b) result(c)
      type(GenericReal) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int64_real

   function plus_int64_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int64_dp
   
   function plus_int64_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value + b%value
   end function plus_int64_complex

   !=================
   ! Integer64 multiply
   !=================

   function times_int64_int(a, b) result(c)
      type(GenericInteger64) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value * b%value
   end function times_int64_int
      
   function times_int64_int64(a, b) result(c)
      type(GenericInteger64) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value * b%value
   end function times_int64_int64
      
   function times_int64_real(a, b) result(c)
      type(GenericReal) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value * b%value
   end function times_int64_real

   function times_int64_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value * b%value
   end function times_int64_dp
   
   function times_int64_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericInteger64), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value * b%value
   end function times_int64_complex
      


   !=================
   ! Real add
   !=================

   function plus_real_int(a, b) result(c)
      type(GenericReal) :: c
      class(GenericReal), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value + b%value
   end function plus_real_int
      
   function plus_real_int64(a, b) result(c)
      type(GenericReal) :: c
      class(GenericReal), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value + b%value
   end function plus_real_int64
      
   function plus_real_real(a, b) result(c)
      type(GenericReal) :: c
      class(GenericReal), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value + b%value
   end function plus_real_real

   function plus_real_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericReal), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value + b%value
   end function plus_real_dp
   
   function plus_real_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericReal), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value + b%value
   end function plus_real_complex
      

   !=================
   ! Real multiply
   !=================

   function times_real_int(a, b) result(c)
      type(GenericReal) :: c
      class(GenericReal), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value * b%value
   end function times_real_int
      
   function times_real_int64(a, b) result(c)
      type(GenericReal) :: c
      class(GenericReal), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value * b%value
   end function times_real_int64
      
   function times_real_real(a, b) result(c)
      type(GenericReal) :: c
      class(GenericReal), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value * b%value
   end function times_real_real

   function times_real_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericReal), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value * b%value
   end function times_real_dp
   
   function times_real_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericReal), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value * b%value
   end function times_real_complex
      

   !=====================
   ! Double Precision add
   !=====================

   function plus_dp_int(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value + b%value
   end function plus_dp_int
      
   function plus_dp_int64(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value + b%value
   end function plus_dp_int64
      
   function plus_dp_real(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value + b%value
   end function plus_dp_real

   function plus_dp_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value + b%value
   end function plus_dp_dp
   
   function plus_dp_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value + b%value
   end function plus_dp_complex
      

   !==========================
   ! Double Precision multiply
   !==========================

   function times_dp_int(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value * b%value
   end function times_dp_int
      
   function times_dp_int64(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value * b%value
   end function times_dp_int64
      
   function times_dp_real(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value * b%value
   end function times_dp_real

   function times_dp_dp(a, b) result(c)
      type(GenericDoublePrecision) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value * b%value
   end function times_dp_dp
   
   function times_dp_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericDoublePrecision), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value * b%value
   end function times_dp_complex
      


   !=====================
   ! Complex add
   !=====================

   function plus_complex_int(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value + b%value
   end function plus_complex_int

      
   function plus_complex_int64(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value + b%value
   end function plus_complex_int64
      
   function plus_complex_real(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value + b%value
   end function plus_complex_real

   function plus_complex_dp(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value + b%value
   end function plus_complex_dp
   
   function plus_complex_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value + b%value
   end function plus_complex_complex
      

   !==========================
   ! Complex multiply
   !==========================

   function times_complex_int(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericInteger), intent(in) :: b
      c%value = a%value * b%value
   end function times_complex_int
      
   function times_complex_int64(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericInteger64), intent(in) :: b
      c%value = a%value * b%value
   end function times_complex_int64
      
   function times_complex_real(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericReal), intent(in) :: b
      c%value = a%value * b%value
   end function times_complex_real

   function times_complex_dp(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericDoublePrecision), intent(in) :: b
      c%value = a%value * b%value
   end function times_complex_dp
   
   function times_complex_complex(a, b) result(c)
      type(GenericComplex) :: c
      class(GenericComplex), intent(in) :: a
      type(GenericComplex), intent(in) :: b
      c%value = a%value * b%value
   end function times_complex_complex
      
end module GenericIntrinsics_mod
