This document is intended to be a list of requirements that are to be distilled
from the various use cases.   Ideally this will drive the creation of a formal
paper for generics that will be submitted to the Fortran committee.

Each requirement should specify one or more motivating use cases.

Requirement:   It must be possible to write a subroutine that has access to the private components of more than one instantiation of a templated type.    Without this requirement, multiple instantiations of a type would generally require the procedure to be defined in a separate module and would therefore not have access to the private components.

Use case:   map() function from a container of type T to a container of type U

Example: https://github.com/j3-fortran/generics/blob/355b9e7a0b5f7993e2b7f47eeae6f7f582d0b73d/scenarios/generic_algorithms/sparse_array_m.f90#L1-L169

