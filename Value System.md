# A Value System for evaluating alternatives

This paper attempts to collect our thoughts on creating an overarching value system for evaluating generic programming alternatives.

## What's a "Value System"?
Simply put, a _value system_ is a short list of values that help determine the relative _goodness_ of a solution.

Each value may be described in a word or very short phrase (e.g., "run-time performance").
In order to avoid it being something of a Rorschach test, though,
we describe in a bit more detail what we mean by that label.
We include rationale for why it is a value.

### Independence
In an ideal world, the values are independent of (orthogonal to) each other.
That is, satisfying one value doesn't have a high correlation with satisfying a different value.
We may need to describe its relationship to other values (especially to emphasize hw they don't overlap).

### Priority
Some values will be considered more important than other values.
In an ideal world, we can prioritize the values in strict order, from most important to least important.
We regard more highly solutions which favor the most important values.
For the entire list of values, we describe the rationale for the relative rankings.

(Numerically inclined people may think in terms of applying _weights_ to each value, with more important values being given larger weight.)

### Non Values
It is sometimes helpful to define things that are explicitly _not_ goals of the effort.


### Consensus
When dealing with complex technical or social issues, we know that we do not live in an ideal world.
At best, we will have some values that correlate somewhat with other values.
And we may never get consensus on any more than a partial order of the values.

Gaining consensus on these values _before_ examining alternatives is an important step in gaining consensus on the outcome of the evaluation.


## Why create an explicit Value System?
There are several benefits to defining the values and their relative priorities.
Briefly,
* **Tractability:** Without an explicit value system, it can be impossible to consistently evaluate a complex set of alternatives. Each person involved in the process will use their own (implicit) value system in the evaluation.
* **Negotiaton:** It is relatively easy to negotiate different points of view on the values, and their respective rationales, to be able to come to a group consensus.
* **Initial consensus:** It can be the first place the group arrives at what they _do_ agree on.
* **Communication** It's easier to explain to others outside the initial group the explicit goals any solution must satisfy. Explaining the value system first allows others to see the context. It can provide a place to negotiate and gain consensus with the larger group.
* **Consistent evaluations of alternatives:** Alternatives are evaluated against the same "lens". Important considerations are always treated as important. Secondary considerations don't dominate the evaluation.


## Examples of values for Fortran generic programming
To begin the discussion, here is are a number of values that have come up in the generics conversations (that I have been part of).

There are many other potential values.

These are listed in no particular order.


### Utility
The solution should solve real-world problems faced by Fortran programmers.

Use cases should not be limited to toy problems, but show how the solution significantly improves what can be accomplished in the existing language.


### Run-time performance
The solution shouldn't have a negative impact on run-time performance.

The solution should have no negative impact on the run-time performance of programs that don't use the facility.


### Be Fortran-y
The solution should fit in well with the current Fortran language, including facilities for
* parameterized derived type
* modules and submodules
* procedure pointers


### Implementability
The solution can be implemented in existing compilers without having to rewrite major portions of the compiler.


### Non value: Marketability
The target audience for the solution is existing Fortran programmers.

It is not a goal to dramatically increase Fortran's popularity or market share in the general programming populace.

If it happens, that would be great.
But we are not tailoring or evaluating solutions in this regard.
