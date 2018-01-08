# greskell - Haskell binding for Gremlin graph query language

''This package is work in progress. Be patient...''

greskell is a toolset to build and execute [Gremlin graph query language](http://tinkerpop.apache.org/gremlin.html) in Haskell.

Features:

- Monadic interface to manage variable bindings.
- Type-safe DSL to construct `GraphTraversal`s.
- Parser of [GraphSON](http://tinkerpop.apache.org/docs/3.3.1/dev/io/#graphson) data format.

Contents:

- [The Greskell type](#the-greskell-type)
- (TBW)

## The Greskell type

At the core of greskell is the `Greskell` type. `Greskell a` represents a Gremlin expression that evaluates to the type `a`.

```haskell
import Data.Greskell.Greskell (Greskell, toGremlin)

literalHoge :: Greskell Text
literalHoge = "foo"

literalInt :: Greskell Int
literalInt = 200
```

You can convert `Greskell` into Gremlin `Text` script by `toGremlin` function.

```haskell
spec_toGremlin_Text = toGremlin literalHoge `shouldBe` "\"foo\""
```

`Greskell` implements instances of `IsString`, `Num`, `Fractional` etc. so you can use methods of these classes to build `Greskell`.

```haskell
spec_toGremlin_Int = toGremlin (literalInt + 30 * 20) `shouldBe` "(200)+((30)*(20))"
```


## Build variable binding

## The GTraversal type

## Author

Toshio Ito <debug.ito@gmail.com>
