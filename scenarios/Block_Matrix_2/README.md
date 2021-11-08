This is an attemtp to use a separate TEMPLATE construct to implement
a matrix multiplication template that can work with block matrices.


Issues & observations

1. I gave up trying to do this with procedures.  Operators are simply
   much cleaner when struggling with basic concepts and syntax.
2. There really should be a requirement to have a "zero" value for
   some of the types involved.  But this gets tricky with block
   matrices.  We can only define a "zero" value if the shape of the
   block is known in advance.  This could be done by passing shape as
   an extra parameter to the block.  The alternative, implemented
   here, is that the algorithm requires that we don't ever have
   degenerate matrices (`#rows or #colums = 0`).  We can then infer the
   shape from other information.
3. Having both templates in the same module does not realy help with the issue
   of privat components.
4. One must be careful about ELEMENTAL.  I've introduced `.matmul.` to
   be safe, but still probably have issues in this implementation.   Might be best
   if .matmul. could not work on Fortran arrays directly at all?
5. Pedagogical examples might be better if we gave different operators
   at each level instead of reusing +, *, .matmul.
   
