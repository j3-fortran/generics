The files in this directory are an attempt to demonstrate several
ideas that were bantered about at the last subgroup telecon:

  1. Introduce a new TEMPLATE construct - sort of like an inner
     parameterized module.
  2. How can a map procedure be combined with a rank-agnostic Array
     wrapper template, to provide operations on the wrapped elements?
  3. Use procedures instead of operators as template arguments.  (More
     general, and sidesteps some anoying notation issues to a degree.)
	 
	 
	 
I chose to try to implement an AXPY template that implements `a*x +
y`.  Conventionally, this is for the case where `a` is a scalar and
`x` and `y` are 1D arrays, but here we want something that will work
with any types and ranks that are conformable, but wrapped as Array
types.

Files:

 - AXPY.F90 - AXPY template and some useful RESTRICTIONS
 - Array.F90 - Array template, with copy constructors, and element map
   function
 - driver.F90 - sews it together with
   - `a` being a real scalar (wrapped),
   - `x` being a 2D real array (wrapped),
   - and `y` being a 2D real array.
   
   
Note that I've intentionally made the templates maximally general,
which in particular means that they take more type parameters for
intermediate types that might be different.  Note that there is 
no restriction on the `map_t` template for the arrays to be conformable,
but I did assume that W = V was conformable.

Some observations:

1. I think the TEMPLATE mechanism is much cleaner than the
   parameterized module mechanism.  Being able to define a 2nd
   template in the same scope that uses the first template is a real
   windfall.
2. We have no way to specify that arrays are conformable.  There is a
   runtime aspect of this that is inescapable, as the underlying
   shapes can be different, but in theory I would like the `map_t`
   template to require that either all ranks are equal except those
   that are scalar:
	 - `(rank_U == rank_T) .or. rank_u*rank_T = 0`
     - `rank_V == max(rank_T, rank_U)`
   Do we want to be able to do this?
3. Procedures might be more general, but this example really does
   scream for using operators.

4. UGH!!!  This seems much much more complicated for a simple AXPY
   template than I would have thought.
   - OTOH, doing a full blown block matrix-matrix multiply this way
     may not be _that_ much worse.    So maybe just big first step?
 

 
