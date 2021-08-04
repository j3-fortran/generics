This document is intended to be a list of requirements that are to be distilled
from the various use cases.   Ideally this will drive the creation of a formal
paper for generics that will be submitted to the Fortran committee.

Each requirement should specify one or more motivating use cases.



Terminology used in this paper:

A template is a program unit that is parameterized by one or more
template parameters.

A template instantiation is the program unit defined by a template and
a particular set of actual template parameters.

A template parameter can be any of:
  - type name
  - constant expression
  - procedure name
  - operator
