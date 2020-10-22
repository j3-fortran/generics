# Go

The Go language does not have templates, but there is a recent proposal to add
them:

https://go.googlesource.com/proposal/+/refs/heads/master/design/go2draft-type-parameters.md

An example of a template `T` of a type / constraint `any` (constraint is the
same as concept in C++):

    // Note: the type `any` is already defined implicitly
    type any interface {
    }

    // Print prints the elements of a slice.
    // It should be possible to call this with any slice value.
    func Print[T any](s []T) { // Just an example, not the suggested syntax.
        for _, v := range s {
            fmt.Println(v)
        }
    }

A template of type `any` must work with any user type, and so only a few
[basic
operations](https://go.googlesource.com/proposal/+/refs/heads/master/design/go2draft-type-parameters.md#operations-permitted-for-any-type) are allowed. It is implicitly
[defined
](https://go.googlesource.com/proposal/+/refs/heads/master/design/go2draft-type-parameters.md#the-constraint) as above.

As an example, this function will not compile because type `any` does not have a
`.String()` method:

    // This function is INVALID.
    func Stringify[T any](s []T) (ret []string) {
        for _, v := range s {
            ret = append(ret, v.String()) // INVALID
        }
        return ret
    }

To fix that, we have to define a new constraint:

    // Stringer is a type constraint that requires the type argument to have
    // a String method and permits the generic function to call String.
    // The String method should return a string representation of the value.
    type Stringer interface {
        String() string
    }

and use it in the `Stringify` function:

    // Stringify calls the String method on each element of s,
    // and returns the results.
    func Stringify[T Stringer](s []T) (ret []string) {
        for _, v := range s {
            ret = append(ret, v.String())
        }
        return ret
    }

This approach is what C++ calls *strong concepts*.

Some of the
[features of this approach](https://go.googlesource.com/proposal/+/refs/heads/master/design/go2draft-type-parameters.md#constraints):

* When the `Stringify` generic function compiles and no errors are reported,
  that function is valid no matter how it is called. The `Stringer` constraint
  is part of the function signature.

* We don‘t want to derive the constraints from whatever Stringify happens to do (in this case, call the String method). If we did, a minor change to Stringify might change the constraints. That would mean that a minor change could cause code far away, that calls the function, to unexpectedly break. It’s fine for Stringify to deliberately change its constraints, and force callers to change. What we want to avoid is Stringify changing its constraints accidentally.

* This means that the constraints must set limits on both the type arguments passed by the caller and the code in the generic function. The caller may only pass type arguments that satisfy the constraints. The generic function may only use those values in ways that are permitted by the constraints. This is an important rule that we believe should apply to any attempt to define generic programming in Go: generic code can only use operations that its type arguments are known to implement.


This is to be contrasted with traditional C++ templates which have no concepts,
that have the following property:

* The `Stringify` function gets compiled, but it can still fail at the call site
  with a complicated (nested) error such as no method `.String()`.

* The constaints are not part of the function signature, and as a result the
  compiler must examine the function itself at every call site to ensure
  everything works, which causes long / unfriendly error messages and long
  compile times.

See also the proposal's comparison with C++ and Rust:

* [Comparison with
  C++](https://go.googlesource.com/proposal/+/refs/heads/master/design/go2draft-type-parameters.md#comparison-with-c)
* [Comparison with
  Rust](https://go.googlesource.com/proposal/+/refs/heads/master/design/go2draft-type-parameters.md#comparison-with-rust)


**Question**: What is the difference between Go templates with constraints /
interfaces (this proposal) and the existing
[Go interfaces](https://golangbot.com/interfaces-part-1/)?

**Answer**: It seems that Go interfaces are resolved at runtime, while Go
templates are strictly resolved at compile time. That might be the only main
difference.
