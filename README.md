# greskell - Haskell binding for Gremlin graph query language

__This package is work in progress. Be patient...__

greskell is a toolset to build and execute [Gremlin graph query language](http://tinkerpop.apache.org/gremlin.html) in Haskell.

Features:

- Monadic interface to manage variable bindings.
- Type-safe DSL to construct `GraphTraversal`s.
- Parser of [GraphSON](http://tinkerpop.apache.org/docs/current/dev/io/#graphson) data format.

__NOTE: for now greskell doesn't support connecting to a Gremlin server. For that purpose, use [gremlin-haskell](http://hackage.haskell.org/package/gremlin-haskell).__

Contents:

- [The Greskell type](#the-greskell-type)
- (TBW)


## Prelude

Because this README is also a test script, first we import common modules.

```haskell common
{-# LANGUAGE OverloadedStrings #-}
import Control.Category ((>>>))
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import Data.Function ((&))
import Data.Void (Void)
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
main = hspec $ specify "Greskell" $ do
  toGremlin literalText `shouldBe` "\"foo\""
```

`Greskell` implements instances of `IsString`, `Num`, `Fractional` etc. so you can use methods of these classes to build `Greskell`.

```haskell Greskell
  toGremlin (literalInt + 30 * 20) `shouldBe` "(200)+((30)*(20))"
```

## Build variable binding

Gremlin Server supports [parameterized scripts](http://tinkerpop.apache.org/docs/current/reference/#parameterized-scripts), where a client can send a Gremlin script and variable binding.

greskell's `Binder` monad is a simple monad that manages bound variables and their values. With `Binder`, you can inject Haskell values into Greskell.

```haskell Binder
import Data.Greskell.Greskell (Greskell, toGremlin)
import Data.Greskell.Binder (Binder, newBind, runBinder)
import qualified Database.TinkerPop as TP -- from gremlin-haskell

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 100
```

`newBind` creates a new Gremlin variable unique in the `Binder`'s monadic context, and returns that variable.

```haskell Binder
main = hspec $ specify "Binder" $ do
  let (script, binding) = runBinder $ plusTen 50
  toGremlin script `shouldBe` "(__v0)+(100)"
  binding `shouldBe` HM.fromList [("__v0", A.Number 50)]
```

`runBinder` function returns the `Binder`'s monadic result and the created binding.

To execute the script and binding, use [gremlin-haskell](http://hackage.haskell.org/package/gremlin-haskell) package.

```haskell Binder
executeExample :: IO ()
executeExample = do
  let (script, binding) = runBinder $ plusTen 50
  TP.run "localhost" 8182 $ \connection -> do
    result <- TP.submit connection (toGremlin script) (Just binding)
    print result
```


## GTraversal and Walk

greskell has a domain-specific language (DSL) for building Gremlin [Traversal](http://tinkerpop.apache.org/docs/current/reference/#traversal) object. Two data types, `GTraversal` and `Walk`, are especially important in this DSL.

`GTraversal` is simple. It's just the greskell counterpart of [GraphTraversal](http://tinkerpop.apache.org/javadocs/current/full/org/apache/tinkerpop/gremlin/process/traversal/dsl/graph/GraphTraversal.html) class in Gremlin.

`Walk` is a little tricky. It represents a chain of one or more method calls on a GraphTraversal object. In Gremlin, those methods are called "[graph traversal steps](http://tinkerpop.apache.org/docs/current/reference/#graph-traversal-steps)." greskell defines those traversal steps as functions returning a `Walk` object.

For example,

```haskell GTraversal
import Data.Greskell.Greskell (toGremlin, Greskell)
import Data.Greskell.GTraversal
  ( GTraversal, Transform, Walk, source, vertices,
    gHasLabel, gHas2, (&.), ($.)
  )
import Data.Greskell.Graph (AVertex)

allV :: GTraversal Transform Void AVertex
allV = source "g" & vertices []

isPerson :: Walk Transform AVertex AVertex
isPerson = gHasLabel "person"

isMarko :: Walk Transform AVertex AVertex
isMarko = gHas2 "name" ("marko" :: Greskell Text)

main = hspec $ specify "GTraversal" $ do
  toGremlin (allV &. isPerson &. isMarko)
    `shouldBe`
    "g.V().hasLabel(\"person\").has(\"name\",\"marko\")"
```

In the above example, `allV` is the GraphTraversal obtained by `g.V()`. `isPerson` and `isMarko` are method calls of `.hasLabel` and `.has` steps, respectively. `(&.)` operator combines a `GTraversal` and `Walk` to get an expression that the graph traversal steps are executed on the GraphTraversal.

The above example also uses `AVertex` type. `AVertex` is a type for a graph vertex. We will explain it in detail later in [Graph structure types](#graph-structure-types).

Note that we use `(&)` operator in the definition of `allV`. `(&)` operator from [Data.Function](http://hackage.haskell.org/package/base/docs/Data-Function.html) module is just the flip of `($)` operator. Likewise, greskell defines `($.)` operator, so we could also write the above expression as follows.

```haskell GTraversal
  (toGremlin $ isMarko $. isPerson $. vertices [] $ source "g")
    `shouldBe`
    "g.V().hasLabel(\"person\").has(\"name\",\"marko\")"
```



## Type parameters of GTraversal and Walk

## WalkType

## Graph structure types

## GraphSON parser

## Author

Toshio Ito <debug.ito@gmail.com>
