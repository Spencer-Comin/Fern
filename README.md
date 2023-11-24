
> [!CAUTION]
> **I have abandoned this iteration of Fern. I've been slowly rewriting and reimagining Fern in OCaml. That repo is still private, but if you want to see what's going on there please reach out and I may add you as a collaborator.**

# Fern
 A toy programming language built with LLVM and Prolog
 
## A quick rundown
Let's look at [an example](examples/passing_function.frn) to see how everything works.
To start off, here's the whole thing:
```
{:
    [Int32, Int32 -> Int32]
        sum;

    [End -> IO]
        main;
    
    [(Int32, Int32 -> Int32), Int32, Int32 -> Int32]
        apply;
:}

std::putInt32;
std::debug;

sum := a, b -> a + b;

apply := f, a, b -> f(a, b);

main := putInt32(debug(),
    apply(sum, 1, 2)
);
```

Let's break it down to look at the individual components.

### Type info
At the top of the example there's a section wrapped in `{:` `:}`.
This is the type info section of the file.
In here there are a few type declarations that look like
```
[Foo]
    bar;
```
This declaration says that this file defines a `bar` with type `Foo`.
If we wanted both a `bar` and a `baz` with type `Foo` we could do
```
[Foo]
    bar;
    baz;
```

Now that we know that, we can look at the actual type declarations from the example.
First:
```
[Int32, Int32 -> Int32]
    sum;
```
`Int32` is the 32 bit signed integer type. `,` is the structuring operator.
`->` is the function definition operator. Putting those together,
the type we have here is a function that takes a structure<sup>*</sup> of two `Int32`s as input and outputs an `Int32`.
Later in the example there will be a definition of `sum` which has this type.

\* In Fern, functions types are a mapping from just *one* input type (on the left of the `->`) to just *one* output type (on the right of the `->`),
and therefore functions are mappings from just *one* input argument to just *one* output value.
To get the effect of multiple arguments or returns, we use structures and structure types.

On to the next type declaration:
```
[End -> IO]
    main;
````
`End` is Fern's terminal or singleton type; as an input type it's used to indicate that a function takes no arguments.
`IO` is the input/output stream type. Later, when we see the definition of `main`,
we'll see that it doesn't take an (explicit<sup>**</sup>) argument, which is why its input type is `End`,
and it prints to an output stream, which is why its output type is `IO`.

Now for the last type declaration:
```
[(Int32, Int32 -> Int32), Int32, Int32 -> Int32]
    apply;
````
The type for `apply` is a bit more involved. First, in the parentheses,
we see the same type that we had for `sum` (this is intentional),
a function that takes a structure of two `Int32`s as input and outputs an `Int32`.
Structured together with this function type we have two more `Int32`s.
At the output of our function type we have a single `Int32`.
All together, `apply` will have a type of a function taking a structure containing a function and two `Int32`s and outputting an `Int32`.

### Imports
Importing in Fern is super simple, you just write the name you are importing and the namespace you are importing it from.
```
std::putInt32;
std::debug;
```
In our example we are importing `putInt32` and `debug` from the `std` namespace.
We don't need to include their types in the type info section,
as the compiler finds the types while resolving the imports.
If you want to know their types, you can look in [std.types.frn](runtime/std.types.frn).
`putInt32` takes a structure of an `IO` output stream and an `Int32` value and returns the output stream with the value put on it.
`debug` gives us an output stream for debugging (technically, it gives us the `stderr` output stream).

### Definitions
Definitions look like
```
a := x;
```
which would mean `a` is defined as `x` (whatever `x` is).

In our example, we have
```
sum := a, b -> a + b;
```
`sum` is defined as a function that takes a structure of `a` and `b` and returns their sum (duh).

Next,
```
apply := f, a, b -> f(a, b);
```
`apply` is a function that takes a structure of `f`, `a`, and `b`,
passes a structure built with `a` and `b` to `f`, and return the result.

Last,
```
main := putInt32(debug(),
    apply(sum, 1, 2)
);
```
You might notice that there is no `->` here, so how can `main` be a function?
Remember that `main`s input type is `End`, so it takes no argmuments.
In Fern, a non-function definition is equivalent to a function definition with an `End` input.
Alternatively we could have indicated this with an empty parentheses input:
```
main := () -> putInt32(debug(),
    apply(sum, 1, 2)
);
```
To explain what `main` is doing, we'll start from the inside and work our way out.
We call `debug` without arguments<sup>**</sup> to get a debug output stream.
Our `sum` function from earlier is structured together with number literals `1` and `2` and passed to `apply`
(This is why the type of `sum` shows up in the type declaration of `apply`).
Next we structure together the output debug stream with the result of `apply` and pass that to `putInt32`.

`main` is a special function name, as it indicates the entry point to the program.
When we run this example, Fern calls `main` with a `nil`<sup>***</sup> argument,
and on the output we will see `3` printed.

\*\* The compiler implicitly puts in a `nil`<sup>***</sup> argument, to satisfy the `End` input type.
There must always be *one* input and *one* output.

\*\*\* `nil` is the only value that has the `End` type.

## TODO
- [ ] Generate executables
- [ ] Closures
- [ ] Match expressions
- [ ] String literals
- [x] Floating point literals
- [ ] Type sums
