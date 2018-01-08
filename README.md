# greskell - Haskell binding for Gremlin graph query language

__This package is work in progress. Be patient...__

greskell is a toolset to build and execute [Gremlin graph query language](http://tinkerpop.apache.org/gremlin.html) in Haskell.

Features:

- Monadic interface to manage variable bindings.
- Type-safe DSL to construct `GraphTraversal`s.
- Parser of [GraphSON](http://tinkerpop.apache.org/docs/3.3.1/dev/io/#graphson) data format.

Contents:

- [The Greskell type](#the-greskell-type)
- (TBW)


## Prelude

Because this README is a test script, first we import common modules.

```haskell common
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Test.Hspec
```

## The Greskell type

At the core of greskell is the `Greskell` type. `Greskell a` represents a Gremlin expression that evaluates to the type `a`.

```haskell Greskell
import Data.Greskell.Greskell (Greskell, toGremlin)

literalText :: Greskell Text
literalText = "foo"

literalInt :: Greskell Int
literalInt = 200
```

You can convert `Greskell` into Gremlin `Text` script by `toGremlin` function.

```haskell Greskell
main = hspec $ describe "Greskell" $ do
  it "espaces string" $ toGremlin literalText `shouldBe` "\"foo\""
```

`Greskell` implements instances of `IsString`, `Num`, `Fractional` etc. so you can use methods of these classes to build `Greskell`.

```haskell Greskell
  it "is a Num" $ toGremlin (literalInt + 30 * 20) `shouldBe` "(200)+((30)*(20))"
```


## Build variable binding

## The GTraversal type

## Author

Toshio Ito <debug.ito@gmail.com>
