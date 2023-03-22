The matrix example is best understood by reading in the following order.

Requirements that build on each other:

* semigroup_m.f90
* monoid_m.f90
* semiring_m.f90
* unit_ring_m.f90
* field_m.f90

Example of how a square matrix template can take advantage of the requirements,
and how templates can be nested to allow for "optional" functionality.

* matrix_m.f90

Example of how matrices can be instantiated,
how it allows block matrices to be easily constructed,
and how it allows for powerful new use cases.