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


Variants:

- V1: Brute force - each intrinsic type/kind is a separate generic type

- V2: Leverage F2003 parameterized types.  This reduces the number of
      derived types that are needed, but unfortunately does nothing to
      reduce the number of type-bound functions that must be
      explicitly written.  This example mostly just shows that F2003
      type parameters are very unlike template parameters in precisely
      this respect.  If the kind-type parameters for dummy arguments
      could be assumed, this would change dramatically.  (See V3)

- V3: If F202y were to relax the constraint that kind-type parameters
      for dummy arguments cannot be assumed, things could get
      considerably simpler.  V3 demonstrates this.  The number of
      type-bound functions that must be written goes down from 72 in
      V2, to just 9 in V3.  And the ratio improves further for
      compilers that provide additional kinds for these types.

      



      
