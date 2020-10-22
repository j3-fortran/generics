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
Go interfaces ([1](https://golangdocs.com/interfaces-in-golang) and [2](https://golangbot.com/interfaces-part-1/))?

**Answer**: It seems that Go interfaces are resolved at runtime, while Go
templates are strictly resolved at compile time. That might be the only main
difference.

# C++

## Traditional Templates

The `Stringify` example in C++ would look like:

    template <typename T>
    std::string Stringify(const std::vector<T> &s) {
        std::string ret;
        for (auto &v : s) {
            ret.append(v.String());
        }
        return ret;
    }

And this compiles. Here is how it could be used in the main program:

    class MyT
    {
    public:
        std::string String() const {
            return "X";
        }
    };

    int main() {
        std::vector<MyT> v(3);
        std::cout << Stringify(v) << std::endl;
        return 0;
    }

See the [traditional.cpp](./traditional.cpp) file for a complete program that
compiles and runs.

There are *no concepts*. As a result, if you remove the `String()` method from
`MyT`, you get the following error message with g++ 10:

    a.cpp: In instantiation of ‘std::string Stringify(const std::vector<T>&) [with T = MyT; std::string = std::__cxx11::basic_string<char>]’:
    a.cpp:20:29:   required from here
    a.cpp:9:22: error: ‘const class MyT’ has no member named ‘String’
        9 |         ret.append(v.String());
          |                    ~~^~~~~~

This error gets a lot more complicated and nested in practice if the
`Stringify` function calls other templates functions and the error is in the
inner most one.

But even in this simple example, the error that is reported is in the
`Stringify` function, which compiled fine on its own, but failed at
instantiation time. This is the main feature of *no concepts*.

## C++20 Concepts Light

C++20 has [light
concepts](https://en.cppreference.com/w/cpp/language/constraints). The
above `Stringify` example with C++ concepts would look like:

    template<typename T>
    concept Stringer = requires(const T &t) {
        { t.String() } -> std::same_as<std::string>;
    };

    template <Stringer T>
    std::string Stringify(const std::vector<T> &s) {
        std::string ret;
        for (auto &v : s) {
            ret.append(v.String());
        }
        return ret;
    }

And it would be used exactly as before.
See the [concepts-light.cpp](./concepts-light.cpp) file for a complete program
that compiles and runs.

If you remove the `String()` method from `MyT`, you get an error:

    concepts-light.cpp: In function ‘int main()’:
    concepts-light.cpp:30:29: error: use of function ‘std::string Stringify(const std::vector<T>&) [with T = MyT; std::string = std::__cxx11::basic_string<char>]’ with unsatisfied constraints
       30 |     std::cout << Stringify(v) << std::endl;
          |                             ^
    concepts-light.cpp:15:13: note: declared here
       15 | std::string Stringify(const std::vector<T> &s) {
          |             ^~~~~~~~~
    concepts-light.cpp:15:13: note: constraints not satisfied
    concepts-light.cpp: In instantiation of ‘std::string Stringify(const std::vector<T>&) [with T = MyT; std::string = std::__cxx11::basic_string<char>]’:
    concepts-light.cpp:30:29:   required from here
    concepts-light.cpp:10:9:   required for the satisfaction of ‘Stringer<T>’ [with T = MyT]
    concepts-light.cpp:10:20:   in requirements with ‘const T& t’ [with T = MyT]
    concepts-light.cpp:11:15: note: the required expression ‘t.String()’ is invalid
       11 |     { t.String() } -> std::same_as<std::string>;
          |       ~~~~~~~~^~
    cc1plus: note: set ‘-fconcepts-diagnostics-depth=’ to at least 2 for more detail

Compared to the "traditional templates", this error now happens at the call
site, saying that the user type `MyT` is not satisfying the concept `Stringer`,
because it does not have the `.String()` method.

If however we change the `Stringify` function to call a method `String2()`
instead of `String()`, the function still compiles fine, but we get an error at
instantiation time:

    concepts-light.cpp: In instantiation of ‘std::string Stringify(const std::vector<T>&) [with T = MyT; std::string = std::__cxx11::basic_string<char>]’:
    concepts-light.cpp:33:29:   required from here
    concepts-light.cpp:18:22: error: ‘const class MyT’ has no member named ‘String2’; did you mean ‘String’?
       18 |         ret.append(v.String2());
          |                    ~~^~~~~~~
          |                    String

And if we remove the `main()` function, we do not get any error at all and the
`Stringify` function happily compiles.

It is for this reason we are calling this *concepts light*. If they were strong
concepts such as in the Go language, the error would happen while compiling the
function `Stringify` and it would say the Concept `Stringer` does not have a
`.String2` method, and it would not say anything about `MyT`, as that would be
irrelevant.

In other words, the concepts light will check user's code that the `MyT` type
satisfies the `Stringer` concept. But they will not check the library code, that
the `Stringify` function is written correctly and only uses methods declared by
the `Stringer` concept.
