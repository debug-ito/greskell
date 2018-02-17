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
- [Build variable binding](#build-variable-binding)
- [DSL for graph traversals](#dsl-for-graph-traversals)
- [Type parameters of GTraversal and Walk](#type-parameters-of-gtraversal-and-walk)
- [Restrict effect of GTraversal by WalkType](#restrict-effect-of-gtraversal-by-walktype)
- [Graph structure types](#graph-structure-types)
- [GraphSON parser](#graphson-parser)
- [Make your own graph structure types](#make-your-own-graph-structure-types)


## Prelude

Because this README is also a test script, first we import common modules.

```haskell common
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeFamilies #-}
import Control.Applicative (empty)
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Function ((&))
import Text.Heredoc (here)
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


## DSL for graph traversals

greskell has a domain-specific language (DSL) for building Gremlin [Traversal](http://tinkerpop.apache.org/docs/current/reference/#traversal) object. Two data types, `GTraversal` and `Walk`, are especially important in this DSL.

`GTraversal` is simple. It's just the greskell counterpart of [GraphTraversal](http://tinkerpop.apache.org/javadocs/current/full/org/apache/tinkerpop/gremlin/process/traversal/dsl/graph/GraphTraversal.html) class in Gremlin.

`Walk` is a little tricky. It represents a chain of one or more method calls on a GraphTraversal object. In Gremlin, those methods are called "[graph traversal steps](http://tinkerpop.apache.org/docs/current/reference/#graph-traversal-steps)." greskell defines those traversal steps as functions returning a `Walk` object.

For example,

```haskell GTraversal
import Data.Greskell.Greskell (toGremlin, Greskell)
import Data.Greskell.GTraversal
  ( GTraversal, Transform, Walk, source, sV,
    gHasLabel, gHas2, (&.), ($.)
  )
import Data.Greskell.Graph (AVertex)

allV :: GTraversal Transform () AVertex
allV = source "g" & sV []

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
  (toGremlin $ isMarko $. isPerson $. sV [] $ source "g")
    `shouldBe`
    "g.V().hasLabel(\"person\").has(\"name\",\"marko\")"
```

## Type parameters of GTraversal and Walk

`GTraversal` and `Walk` both have the same type parameters.

```haskell
GTraversal walk_type start end
Walk       walk_type start end
```

`GTraversal` and `Walk` both take the traversers with data of type `start`, and emit the traversers with data of type `end`. We will explain `walk_type` [later](#restrict-effect-of-gtraversal-by-walktype).

`Walk` is very similar to function `(->)`. That is why it is an instance of `Category`, so you can compose `Walk`s together. The example in the last section can also be written as

```haskell GTraversal
  let composite_walk = isPerson >>> isMarko
  (toGremlin $ source "g" & sV [] &. composite_walk )
    `shouldBe`
    "g.V().hasLabel(\"person\").has(\"name\",\"marko\")"
```

## Restrict effect of GTraversal by WalkType

The first type parameter of `GTraversal` and `Walk` is called "walk type". Walk type is a type marker to describe effect of the graph traversal. There are three walk types, `Filter`, `Transform` and `SideEffect`. All of them are instance of `WalkType` class.

- Walks of `Filter` type do filtering only. It takes input traversers and emits some of them. It does nothing else. Example: `.has` and `.filter` steps.
- Walks of `Transform` type may transform the input traversers but have no side effects. Example: `.map` and `.out` steps.
- Walks of `SideEffect` type may alter the "side effect" context of the Traversal object or the state outside the Traversal object. Example: `.aggregate` and `.addE` steps.

Walk types are hierarchical. `Transform` is more powerful than `Filter`, and `SideEffect` is more powerful than `Transform`. You can "lift" a walk with a certain walk type to one with a more powerful walk type by `liftWalk` function.

```haskell WalkType
import Data.Greskell.GTraversal
  ( Walk, Filter, Transform, SideEffect, GTraversal,
    liftWalk, source, sV, (&.), gHas1, gAddV, gValues
  )
import Data.Greskell.Graph (AVertex)
import Data.Greskell.Greskell (toGremlin)

hasAge :: Walk Filter AVertex AVertex
hasAge = gHas1 "age"

hasAge' :: Walk Transform AVertex AVertex
hasAge' = liftWalk hasAge
```

Now what are these walk types useful for? Well, it allows you to build graph traversals in a safer way than you do with plain Gremlin.

In Haskell, we can distinguish pure and non-pure functions using, for example, `IO` monad. Likewise, we can limit power of traversals by using `Filter` or `Transform` walk types explicitly. That way, we can avoid executing unwanted side-effect accidentally.

```haskell WalkType
nameOfPeople :: Walk Filter AVertex AVertex -> GTraversal Transform () Text
nameOfPeople pfilter =
  source "g" & sV [] &. liftWalk pfilter &. gValues ["name"]

newPerson :: Walk SideEffect s AVertex
newPerson = gAddV "person"

main = hspec $ specify "liftWalk" $ do
  ---- This compiles
  toGremlin (nameOfPeople hasAge)
    `shouldBe` "g.V().has(\"age\").values(\"name\")"

  ---- This doesn't compile.
  ---- It's impossible to pass a SideEffect walk to an argument that expects Filter.
  -- toGremlin (nameOfPeople newPerson)
  --   `shouldBe` "g.V().addV(\"person\").values(\"name\")"
```

In the above example, `nameOfPeople` function takes a `Filter` walk and creates a `Transform` walk. There is no way to pass a `SideEffect` walk (like `gAddV`) to `nameOfPeople` because `Filter` is weaker than `SideEffect`. That way, we can be sure that the result traversal of `nameOfPeople` function never has any side-effect (thus its walk type is just `Transform`.)


## Graph structure types

Graph structure interfaces in Gremlin are represented as type-classes. We have `Element`, `Vertex`, `Edge` and `Property` type-classes for the interfaces of the same name.

The reason why we use type-classes is that it allows you to define your own data types as a graph structure. See ["Make your own graph structure types"](#make-your-own-graph-structure-types) below in detail.

Nonetheless, it is convenient to have some generic data types we can use for graph structure types. For that purpose, we have `AVertex`, `AEdge`, `AVertexProperty` and `AProperty` types.

Those types are useful because some functions are too polymorphic for the compiler to infer the types for its "start" and "end".

```haskell monomorphic
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.Graph (AVertex)
import Data.Greskell.GTraversal
  ( GTraversal, Transform,
    source, (&.), sV, gOut, sV', gOut',
  )

main = hspec $ specify "monomorphic walk" $ do
  ---- This doesn't compile
  -- toGremlin (source "g" & sV [] &. gOut []) `shouldBe` "g.V().out()"

  -- This compiles, with type annotation.
  let gv :: GTraversal Transform () AVertex
      gv = source "g" & sV []
      gvo :: GTraversal Transform () AVertex
      gvo = gv &. gOut []
  toGremlin gvo `shouldBe` "g.V().out()"
  
  -- This compiles, with monomorphic functions.
  toGremlin (source "g" & sV' [] &. gOut' []) `shouldBe` "g.V().out()"
```

In the above example, `sV` and `gOut` are polymorphic with `Vertex` constraint, so the compiler would complain about the ambiguity. In that case, you can add explicit type annotations of `AVertex` type, or use monomorphic versions, `sV'` and `gOut'`.


## GraphSON parser

`A` in `AVertex` stands for "Aeson". That means this type implements `FromJSON` instance from [Data.Aeson](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html) module. The `FromJSON` instance parses text encoded in GraphSON format.

[GraphSON](http://tinkerpop.apache.org/docs/current/dev/io/#graphson) is a format to encode graph structure types into JSON. As of this writing, there are three slightly different versions of GraphSON. `AVertex`, `AEdge`, `AVertexProperty` and `AProperty` support all of GraphSON version 1, 2 and 3. However, that makes their structures a little complicated.

To support GraphSON decoding, we introduced a data type called `GraphSON`. `GraphSON a` has data of type `a` and optional "type string" that describes the type of that data.

```haskell GraphSON
import Data.Greskell.GraphSON (GraphSON(..))
import Data.Greskell.Graph
  ( AVertex(..), AVertexProperty(..),
    fromProperties
  )

vertex_GraphSONv1 = [here|
{
  "id" : 1,
  "label" : "person",
  "type" : "vertex",
  "properties" : {
    "name" : [ {
      "id" : 0,
      "value" : "marko"
    } ]
  }
}
|]

vertex_GraphSONv3 = [here|
{
  "@type" : "g:Vertex",
  "@value" : {
    "id" : {
      "@type" : "g:Int32",
      "@value" : 1
    },
    "label" : "person",
    "properties" : {
      "name" : [ {
        "@type" : "g:VertexProperty",
        "@value" : {
          "id" : {
            "@type" : "g:Int64",
            "@value" : 0
          },
          "value" : "marko",
          "label" : "name"
        }
      } ]
    }
  }
}
|]

decoded_vertex_GraphSONv1 =
  GraphSON
  { gsonType = Nothing,
    gsonValue =
      AVertex 
      { avId = GraphSON Nothing (A.Number 1),
        avLabel = "person",
        avProperties = fromProperties [
          AVertexProperty
          { avpId = GraphSON Nothing (A.Number 0),
            avpLabel = "name",
            avpValue = GraphSON Nothing (A.String "marko"),
            avpProperties = mempty
          }
        ]
      }
  }

decoded_vertex_GraphSONv3 =
  GraphSON
  { gsonType = Just "g:Vertex",
    gsonValue =
      AVertex 
      { avId = GraphSON (Just "g:Int32") (A.Number 1),
        avLabel = "person",
        avProperties = fromProperties [
          AVertexProperty
          { avpId = GraphSON (Just "g:Int64") (A.Number 0),
            avpLabel = "name",
            avpValue = GraphSON Nothing (A.String "marko"),
            avpProperties = mempty
          }
        ]
      }
  }


main = hspec $ specify "GraphSON" $ do
  A.decode vertex_GraphSONv1 `shouldBe` Just decoded_vertex_GraphSONv1
  A.decode vertex_GraphSONv3 `shouldBe` Just decoded_vertex_GraphSONv3
```

As you can see in the above example, the vertex object in GraphSON version 3 has `@type` and `@value` fields, while version 1 does not. `AVertex` can parse both versions. The `@type` field, if present, is set to `gsonType` field of `GraphSON` data type.


## Make your own graph structure types

When you use a graph database, I think you usually encode your application-specific data types as graph data structures, and store them in the database. greskell supports directly embedding your application-specific data types into graph data structures.

For example, let's make the following `Person` type a graph vertex.

```haskell own_types
import Data.Greskell.Graph
  ( Element(..), Vertex, AVertexProperty, AVertex(..),
    lookupOneValue
  )
import Data.Greskell.GraphSON (GraphSON(..))
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.GTraversal
  ( GTraversal, Transform,
    source, sV, gHasLabel, gHas2, (&.)
  )

data Person =
  Person
  { personId :: Int,
    personName :: Text,
    personAge :: Int
  }
```

In that case, just make it instances of `Element` and `Vertex` type-classes.

```haskell own_types
instance Element Person where
  type ElementID Person = Int
  type ElementProperty Person = AVertexProperty

instance Vertex Person
```

`Element` type-class has two associated types.

- `ElementID` is the type of the vertex ID. It depends on your graph database implementation and settings.
- `ElementProperty` is the type of the property of the vertex. If you don't care, just use `AVertexProperty`.

Once `Person` is a `Vertex`, you can use it in greskell's traversal DSL.

```haskell own_types
main = hspec $ specify "your own graph types" $ do
  let get_marko :: GTraversal Transform () Person
      get_marko = source "g" & sV [] &. gHasLabel "person" &. gHas2 "name" "marko"
  toGremlin get_marko `shouldBe` "g.V().hasLabel(\"person\").has(\"name\",\"marko\")"
```

In addition, you can easily implement `FromJSON` instance for `Person` type using `AVertex`.

```haskell own_types
instance A.FromJSON Person where
  parseJSON v = fromAVertex =<< A.parseJSON v
    where
      fromAVertex :: AVertex -> A.Parser Person
      fromAVertex av = do
        guard (avLabel av == "person")
        pid <- A.parseJSON $ gsonValue $ avId av
        name <- maybe empty (A.parseJSON . gsonValue) $ lookupOneValue "name" $ avProperties av
        age <- maybe empty (A.parseJSON . gsonValue) $ lookupOneValue "age" $ avProperties av
        return $ Person pid name age
```

Using `AVertex` as an intermediate type, you can now parse GraphSON (in any version!) vertex into `Person` type.


## Todo

- Complete graph traversal steps API.
- Develop WebSocket transport for Gremlin Server.


## Author

Toshio Ito <debug.ito@gmail.com>
