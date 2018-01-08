# greskell - Haskell binding for Gremlin graph query language

__This package is work in progress. Be patient...__

greskell is a toolset to build and execute [Gremlin graph query language](http://tinkerpop.apache.org/gremlin.html) in Haskell.

Features:

- Monadic interface to manage variable bindings.
- Type-safe DSL to construct `GraphTraversal`s.
- Parser of [GraphSON](http://tinkerpop.apache.org/docs/3.3.1/dev/io/#graphson) data format.

__NOTE: for now greskell doesn't support connecting to a Gremlin server. For that purpose, use [gremlin-haskell](http://hackage.haskell.org/package/gremlin-haskell).__

Contents:

- [The Greskell type](#the-greskell-type)
- (TBW)


## Prelude

Because this README is also a test script, first we import common modules.

```haskell common
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
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

Gremlin Server supports [parameterized scripts](http://tinkerpop.apache.org/docs/3.3.1/reference/#parameterized-scripts), where a client can send a Gremlin script and variable binding.

greskell's `Binder` monad is a simple monad that manages bound variables and their values. With `Binder`, you can inject Haskell values into Greskell.

```haskell Binder
import Data.Greskell.Greskell (Greskell, toGremlin)
import Data.Greskell.Binder (Binder, newBind, runBinder)

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 100
```

`newBind` creates a new Gremlin variable unique in the `Binder`'s monadic context, and returns that variable.

```haskell Binder
main = hspec $ describe "Binder" $ do
  it "creates parameterized script" $ do
    let (script, binding) = runBinder $ plusTen 50
    toGremlin script `shouldBe` "(__v0)+(100)"
    binding `shouldBe` HM.fromList [("__v0", A.Number 50)]
```

`runBinder` function returns the `Binder`'s monadic result and the created binding.

## The GTraversal type

## Author

Toshio Ito <debug.ito@gmail.com>
