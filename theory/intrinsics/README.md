The purpose of this directory is to explore issues related to ensuring
that intrinsic types are on an equal footing with user defined types.

The main concern has to do with "concepts" (ref C++).  If Fortran
generics provide a similar mechanism for restricting type-name
parameters in a template, it would presumably be based upon the
interfaces defined by type-bound procedures.  But intrinsic types do
not have type-bound procedures.

Consider the case where we want to require that a type has the "+"
operator.  This might be expressed something like:

```f90
type :: T
contains
   procedure :: plus
   generic :: operator(+) => plus
end type

interface
   function plus(a,b) result(c)
      type(T), intent(out) :: c
      class(T), intent(in) :: a
      type(T), intent(in) :: b
   end function plus
end interface
```

Intrinsic types would fail this requirement even though integer, real,
and complex types all support the intended operation.

There are many issues here, and this document will likely grow considerably.

