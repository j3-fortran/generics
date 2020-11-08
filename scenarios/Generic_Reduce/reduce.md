# The generic reduce function

## Overview of the directory

This directory explores different semantics and supporting syntax for
Fortran generics by examining different ways of writing and expressing
a generic REDUCE function. While an attempt has been made to make the
example syntaxes straightforward extensions of the planned F202X syntax,
no attempt has been made to make the examples compilable by a Fortran
compiler, with or without a preprocessor. If a user knows of simple
changes that will allow an example to retain its basic structure and be
compilable with a preprocessor, they are encouraged to edit the example
to make the appropriate changes. Any other improvements in the examples
will also be appreciated.

## The reduce function

A reduce function nominally takes as its "arguments" a structure and an
operation (either a function or an operator) with two arguments of the
same type as the elements of the structure, and whose result is the same
type as the elements of the structure. It may also take as an argument a
value that, as an operand of the operation, results in returning the
value of the other argument to the operation. This value allows a
definition for reduce for an empty structure. For our example reduce,
we assume a rank 1 array with at least one element as the structure so
the value argument is unneeded. The reduce function applies the
operation to the result of previous applications of the operation and
successive elements of the structure, resulting, in the end, in a scalar
value of the type of the elements of the structure. It is assumed that
the application of the operation to the first element of the structure
returns that element. Perhaps the most common usage of a reduce
function is with a "+" operator as its operation, resulting in a
summation function, so it is useful to be able to supply an operator as
its operation argument. Other common usages are with with binary maximum
and minimum functions yielding maximums and minimums over the whole
structure.

## Issues in defining a reduce function

Complications in defining a generic reduce in Fortran include:

1. It can be defined either as part of a generic module or as a stand
alone generic function;

2. The operation can be either a function or an operator;

3. The operation can be either an argument to the function or a
parameter to the generic;

4. The operation can be either polymorphic, having the CLASS attribute
for at least one of its arguments, or non-polymorphic, having the TYPE
attribute for both its arguments;

5. The operation can be either type-bound, defined in the
*type-bound-procedure-part* of the *derived-type-def*, or
non-type-bound, defined outside of the *type-bound-procedure-part*;

6. A stand alone generic function could either have the type and
operation as explicit parameters, or have them inferred from use;

7. The generic could be required to satisfy strong concepts or allow
instantiation time type checking;

8. The element type, in principle, could have a LEN parameter; and

9. Structures other than rank 1 arrays will often want their own reduce.

## The examples

The examples address these issues as follows:

1. Examples of both generic modules and stand alone generic functions
are provided;

2. Operators are treated as synonyms for functions with one or two
arguments, that can be passed as arguments or parameters with either the
syntax OPERATOR(*defined-operator*) or perhaps the syntax
"*defined-operator*". They could also be treated as type-bound
operations;

3. Examples of treating both the operation as an argument to the
function and as a parameter to the generic are provided;

4. It is assumed that only the first argument may have the CLASS
attribute, and that a keyword will be found, e.g. NATURE or MATCH, that
will match both TYPE and CLASS;

5. It is assumed that type-bound procedures as well as non-type bound
procedures can be passed as generic arguments/parameters, or separate
reduce functions will have to be written for the two categories. An
example of a separate type-bound reduce is provided;

6. Examples of generic functions with both explicit and inferred
parameters are provided;

7. I have attempted to follow strong concepts for all examples, i.e.,
interfaces are always provided for the operation;

8. No attempt has been made to address the LEN parameter issue; and

9. It is assumed that structures where a reduce operation is useful will
define their own reduce operation. For such structures an iteration
construct would be useful.


The example files are as follows:

`attribute_interface_reduce.F90` is a generic module defined using
"function" syntax that has T with an operator `+` with the signature
(T,T)=>T implied by a type attribute. Defining the presence of an
operator with an attribute really only works for the relational and
numeric operators.

`generic_function_reduce.F90` is a generic function defined using
"function" syntax that has a function `fun` as a parameter to the
generic reduce.

`generic_module_reduce.F90` is a generic module using "function" syntax
that has a function `fun` as an argument to the reduce function.

`inferred_type_reduce.F90` is a generic function defined using type
inference that has a function `fun` as an argument to the function
reduce.

`template_function_reduce.F90` is a generic function defined using
"template" syntax that has a function `fun` as a type-bound procedure
to the type of the array.

`template_module_reduce.F90`is a generic module defined using "template"
syntax that has a function `fun` as an parameter to the template in the
hope that it will result in inlining.

## Discussion of the examples

The most common characteristic of the examples is that, like most
Fortran code, they are wordy. The core of all the examples is a mere six
lines of code:

```Fortran
    if ( size(x, kind=int64) == 0 ) &
      error stop "In REDUCE, X must have at least one element."
    reduce = x(1)
    do i = 2, size(x,kind=int64)
       reduce = fun(reduce, x(i))
    end do
```

but the type declarations, interfaces, and boilerplate expands the
examples to typically about twenty-eight lines of code, largely
independent of the options chosen for each file, with oneexception.
About eight of those additional lines are related to the interface
needed for the "strong concepts," but these lines would normally be
present in a non-generic version of the same code. The exception is
the file that uses a type attribute to imply the interface. Only the
starting and ending lines of the "requirements" block add to the lines
in ways that would not be present in a non-generic version of the code.

The most flexible examples are those with the operation either a
parameter to the generic or an argument to the function. Making the
operation type-bound, or specified by an attribute, greatly reduces the
flexibility of the reduce generic/template. The specification of the
operation as a generic parameter might be more likely to be inlined,
compared to specification as a function argument. Note that the type
inference approach almost requires that the operation be an argument to
the reduce function, however this approach is slightly less verbose in
definition and on instantiation, as it doesn't need the type parameter.

The generic procedure appears to be more flexible for single procedures
than a generic module, in that multiple generic procedures can be
defined in a single module, and not all of them have to be
instantiatable at once unlike a single generic module defining multiple
procedures. However you can always do one procedure per generic module,
and the main impact will be on the documentation, and not on the amount
of code.

## Generic requirements for a reduce function

Based on the above a generic reduce function has the following
requirements:

- a generic construct with scope equivalent to that of a module or
  procedure;

- the ability to have at least one type as a parameter;

- the ability to associate an assignment operation with a type, either
  implicitly as a property of all types, or as a property in the type
  definition, or as a parameter to the generic with a defined
  interface;

- the ability to associate an arbitrary procedure with a type
  parameter, either as part of the type definition or as an explicit
  parameter to the generic with a defined interface;

- the ability to associate an arbitrary operator with a type parameter,
  either as part of the type definition or as an explicit parameter to
  the generic with a defined interface, or on instantiation as an
  alternative syntax for specifying a "function" with the same
  interface as a procedure parameter to the generic;

- the ability to treat the type associated operators and procedures
  uniformly regardless of whether they are type bound versus
  non-type-bound, or whether their operands are polymorphic versus
  non-polymorphic;

- the ability to type check the generic prior to instantiation; and

- for non-array structures, the ability to iterate over the active
  elements of the structure.

While not required a generic reduce function would benefit from the
following capabilities:

- instantiation in the same specification part that defines the type
  of the elements of the structure; and

- a language defined iteration construct.

While not required a generic reduce function might benefit from the
following capability:

- the ability to specialize based on whether or not the type of a
  parameter has a LEN parameter.
