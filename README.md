# greskell - Haskell binding for Gremlin graph query language

greskell is a toolset to build and execute [Gremlin graph query language](http://tinkerpop.apache.org/gremlin.html) in Haskell.

Contents:

- [The Greskell type](#the-greskell-type)
- [Build variable binding](#build-variable-binding)
- [Submit to the Gremlin Server](#submit-to-the-gremlin-server)
- [DSL for graph traversals](#dsl-for-graph-traversals)
- [Type parameters of GTraversal and Walk](#type-parameters-of-gtraversal-and-walk)
- [Restrict effect of GTraversal by WalkType](#restrict-effect-of-gtraversal-by-walktype)
- [Submit GTraversal](#submit-gtraversal)
- [Graph structure types](#graph-structure-types)
- [GraphSON parser](#graphson-parser)
- [Make your own graph structure types](#make-your-own-graph-structure-types)
- [Write and read properties to/from the graph](#write-and-read-properties-tofrom-the-graph)
    - [Write properties](#write-properties)
    - [Read properties](#read-properties)
    - [Embed property data types](#embed-property-data-types)


## Prelude

Because this README is also a test script, first we import common modules.

```haskell common
{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances #-}
import Control.Applicative ((<$>), (<*>))
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import Data.Function ((&))
import Test.Hspec
```

To run the examples in this README, run `stack test test-readme`. See [test-readme directory](https://github.com/debug-ito/greskell/tree/master/test-readme) to see how this works.


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

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 10
```

`newBind` creates a new Gremlin variable unique in the `Binder`'s monadic context, and returns that variable.

```haskell Binder
main = hspec $ specify "Binder" $ do
  let (script, binding) = runBinder $ plusTen 50
  toGremlin script `shouldBe` "(__v0)+(10)"
  binding `shouldBe` KM.fromList [("__v0", A.Number 50)]
```

`runBinder` function returns the `Binder`'s monadic result and the created binding.


## Submit to the Gremlin Server

To connect to the Gremlin Server and submit your Gremlin script, use [greskell-websocket](http://hackage.haskell.org/package/greskell-websocket) package.

```haskell submit
import Control.Exception.Safe (bracket, try, SomeException)
import Data.Foldable (toList)
import Data.Greskell.Greskell (Greskell) -- from greskell package
import Data.Greskell.Binder -- from greskell package
  (Binder, newBind, runBinder)
import Network.Greskell.WebSocket -- from greskell-websocket package
  (connect, close, submit, slurpResults)
import System.IO (hPutStrLn, stderr)

submitExample :: IO [Int]
submitExample =
  bracket (connect "localhost" 8182) close $ \client -> do
    let (g, binding) = runBinder $ plusTen 50
    result_handle <- submit client g (Just binding)
    fmap toList $ slurpResults result_handle

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 10

main = hspec $ specify "submit" $ do
  egot <- try submitExample :: IO (Either SomeException [Int])
  case egot of
    Left e -> do
      hPutStrLn stderr ("submit error: " ++ show e)
      hPutStrLn stderr ("  We ignore the error. Probably there's no server running?")
    Right got -> do
      hPutStrLn stderr ("submit success: " ++ show got)
      got `shouldBe` [60]
```

`submit` function sends a `Greskell` to the server and returns a `ResultHandle`. `ResultHandle` is a stream of evaluation results returned by the server. `slurpResults` gets all items from `ResultHandle`.


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
isMarko = gHas2 "name" "marko"

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

`Walk` is very similar to function `(->)`. That is why it is an instance of `Category`, so you can compose `Walk`s together. The example in the previous section can also be written as

```haskell GTraversal
  let composite_walk = isPerson >>> isMarko
  toGremlin (source "g" & sV [] &. composite_walk)
    `shouldBe`
    "g.V().hasLabel(\"person\").has(\"name\",\"marko\")"
```

## Restrict effect of GTraversal by WalkType

The first type parameter of `GTraversal` and `Walk` is called "walk type". Walk type is a type marker to describe effect of the graph traversal. There are three walk types, `Filter`, `Transform` and `SideEffect`. All of them are instance of `WalkType` class.

- Walks of `Filter` type do filtering only. It takes input traversers and emits some of them. It does nothing else. Example: `.has` and `.filter` steps.
- Walks of `Transform` type may transform the input traversers but have no side effects. Example: `.map` and `.out` steps.
- Walks of `SideEffect` type may alter the "side effect" context of the Traversal object or the state outside the Traversal object. Example: `.aggregate` and `.addV` steps.

Walk types are hierarchical. `Transform` is more powerful than `Filter`, and `SideEffect` is more powerful than `Transform`. You can "lift" a walk with a certain walk type to one with a more powerful walk type by `liftWalk` function.

```haskell WalkType
import Data.Greskell.GTraversal
  ( Walk, Filter, Transform, SideEffect, GTraversal,
    liftWalk, source, sV, (&.),
    gHasLabel, gHas1, gAddV, gValues, gIdentity
  )
import Data.Greskell.Graph (AVertex)
import Data.Greskell.Greskell (toGremlin)
import Network.Greskell.WebSocket (Client, ResultHandle, submit)

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
  source "g" & sV [] &. gHasLabel "person" &. liftWalk pfilter &. gValues ["name"]

newPerson :: Walk SideEffect s AVertex
newPerson = gAddV "person"

main = hspec $ specify "liftWalk" $ do
  ---- This compiles
  toGremlin (nameOfPeople hasAge)
    `shouldBe` "g.V().hasLabel(\"person\").has(\"age\").values(\"name\")"

  ---- This doesn't compile.
  ---- It's impossible to pass a SideEffect walk to an argument that expects Filter.
  -- toGremlin (nameOfPeople newPerson)
  --   `shouldBe` "g.V().hasLabel(\"person\").addV(\"person\").values(\"name\")"
```

In the above example, `nameOfPeople` function takes a `Filter` walk and creates a `Transform` GTraversal. There is no way to pass a `SideEffect` walk (like `gAddV`) to `nameOfPeople` because `Filter` is weaker than `SideEffect`. That way, we can be sure that the result traversal of `nameOfPeople` function never has any side-effect (thus its walk type is just `Transform`.)


## Submit GTraversal

You can submit `GTraversal` directly to the Gremlin Server. Submitting `GTraversal c s e` yeilds `ResultHandle e`, so you can get the traversal results in a stream.

```haskell WalkType
getNameOfPeople :: Client -> IO (ResultHandle Text)
getNameOfPeople client = submit client (nameOfPeople gIdentity) Nothing
```


## Graph structure types

Graph structure interfaces in Gremlin are represented as type-classes in greskell. We have `Element`, `Vertex`, `Edge` and `Property` type-classes for the interfaces of the same name.

The reason why we use type-classes is that it allows you to define your own data types as a graph structure. See ["Make your own graph structure types"](#make-your-own-graph-structure-types) below in detail.

As the basis of graph structure types, we have `AVertex`, `AEdge`, `AVertexProperty` and `AProperty` types. You might need those types because some functions are too polymorphic for the compiler to infer the types for its "start" and "end".

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

`A` in `AVertex` stands for "Aeson". That means this type is based on the data type from [Data.Aeson](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html) module. With Aeson, greskell implements parsers for GraphSON.

[GraphSON](http://tinkerpop.apache.org/docs/current/dev/io/#graphson) is a format to encode graph structure types into JSON. [greskell-websocket](http://hackage.haskell.org/package/greskell-websocket) uses GraphSON to communicate with the Gremlin Server.

To support GraphSON decoding, we introduced the following symbols:

- `GraphSON` type: `GraphSON a` has data of type `a` and optional "type string" that describes the type of that data.
- `GValue` type: basically Aeson's `Value` enhanced with `GraphSON`.
- `FromGraphSON` type-class: types that can be parsed from `GValue`. It's analogous to Aeson's `FromJSON`.

`AVertex`, `AEdge`, `AVertexProperty` and `AProperty` types implement `FromGraphSON` instance, so you can directly obtain those types from the Gremlin Server.

```haskell WalkType
getAllVertices :: Client -> IO (ResultHandle AVertex)
getAllVertices client = submit client (source "g" & sV []) Nothing
```

Since greskell-1.0.0.0, `AVertex`, `AEdge` and `AVertexProperty` are just references to graph elements, and they don't keep any properties. To read properties from graph elements, see "[Read properties](#read-properties)" below.


## Make your own graph structure types

Often your graph data model is heterogeneous, that is, you have more than one types of vertices and edges with different meanings. Just using `AVertex` and `AEdge` for them easily leads to invalid graph operations. Let's distinguish them by Haskell's type system.

To make your own graph structure types, just wrap the base types with `newtype`.

```haskell own_types2
import Data.Greskell.Graph (AVertex, AEdge, ElementData, Element, Vertex, Edge)
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.GTraversal
  ( GTraversal, Walk, Transform, gOut, gOutE, gHasLabel,
    source, sV, (&.)
  )

-- | A @person@ vertex.
newtype VPerson = VPerson AVertex
        deriving (Eq,Show,FromGraphSON,ElementData,Element,Vertex)

-- | A @software@ vertex.
newtype VSoftware = VSoftware AVertex
        deriving (Eq,Show,FromGraphSON,ElementData,Element,Vertex)

-- | A @knows@ edge.
newtype EKnows = EKnows AEdge
        deriving (Eq,Show,FromGraphSON,ElementData,Element,Edge)

-- | A @created@ edge.
newtype ECreated = ECreated AEdge
        deriving (Eq,Show,FromGraphSON,ElementData,Element,Edge)
```

For each graph type, you need to derive `FromGraphSON`, `ElementData`, `Element` and `Vertex` or `Edge`. Note that you need to enable `GeneralizedNewtypeDeriving` and `UndecidableInstances` extensions of GHC to derive those instances.

With those graph element types, you can also define your own traversal steps.

```haskell own_types2
allPersons :: GTraversal Transform () VPerson
allPersons = source "g" & sV [] &. gHasLabel "person"

gOutKnows :: Walk Transform VPerson VPerson
gOutKnows = gOut ["knows"]

gOutCreated :: Walk Transform VPerson VSoftware
gOutCreated = gOut ["created"]

gOutEKnows :: Walk Transform VPerson EKnows
gOutEKnows = gOutE ["knows"]
```

Using those customized traversal steps, you can make your Gremlin scripts more type-safe and rich in semantics.

```haskell own_types2
main = hspec $ specify "own types" $ do
  toGremlin (allPersons &. gOutCreated)
    `shouldBe` "g.V().hasLabel(\"person\").out(\"created\")"

  toGremlin (allPersons &. gOutKnows)
    `shouldBe` "g.V().hasLabel(\"person\").out(\"knows\")"

  ---- This doesn't compile because the end of gOutCreated is VSoftware
  ---- but the start of gOutKnows is VPerson.
  -- toGremlin (allPersons &. gOutCreated &. gOutKnows)
  --   `shouldBe` "g.V().hasLabel(\"person\").out(\"created\").out(\"knows\")"
```

## Write and read properties to/from the graph

Writing and reading complex properties to/from the graph database is a little tricky. This section explains how we do that with greskell.

As a prelude for this section, we import the following modules first.

```haskell graph_io
import Control.Exception.Safe (bracket)
import Data.Foldable (toList)
import Data.Greskell.AsLabel (AsLabel)
import Data.Greskell.Binder (Binder, runBinder, newBind)
import Data.Greskell.Extra (writeKeyValues, (<=:>), (<=?>))
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.GraphSON (FromGraphSON(..), GValue)
import Data.Greskell.Graph
  ( AVertex, AEdge, ElementData, Element, Vertex, Edge,
    Key, Keys(KeysNil)
  )
import Data.Greskell.GTraversal
  ( Walk, GTraversal, SideEffect, Transform, liftWalk,
    source, sAddV, gValueMap, gProject, gByL,
    gOutV, gInV, gValues, sV, sV', sE,
    (<*.>), (&.), ($.), (<$.>),
    unsafeCastEnd,
    gHas2, gTo, gV, gAddE, gProperty, gDrop
  )
import Data.Greskell.PMap
  ( PMap, Multi, Single, PMapLookupException,
    lookupAs, lookupAs', pMapToFail
  )
import Network.Greskell.WebSocket
  ( connect, close, submit, submitPair,
    slurpResults, drainResults
  )

import Test.Hspec.NeedEnv (EnvMode(Want), needEnvHostPort)

main = hspec $ do
  specWrite
  specRead1
  specRead2
  specIO
```

### Write properties

To write properties of graph elements into the database, you use ".property" step of Gremlin. greskell offers some utility functions to make it a little easier.

First, define a data type for your application, and its corresponding vertex type.

```haskell graph_io
type Name = Text

data Person =
  Person
  { personName :: Name,
    personAge :: Int,
    personCompany :: Maybe Text
    -- ^ Name of the company the person works for, if any.
  }
  deriving (Show,Eq,Ord)

-- | A Vertex corresponding to 'Person'.
newtype VPerson = VPerson AVertex
        deriving (Eq,Show,FromGraphSON,ElementData,Element,Vertex)
```

Then, define `Key`s for properties of `Person`.

```haskell graph_io
keyName :: Key VPerson Name
keyName = "name"

keyAge :: Key VPerson Int
keyAge = "age"

keyCompany :: Key VPerson (Maybe Text)
keyCompany = "company"
```

We will use those `Key`s to write and read properties to/from the graph database. I know it's boring to define `Key`s manually like the above example. Future versions of greskell may support some ways to generate keys from a record type.

Anyway, once you set up the `Key`s, you can use `writeKeyValues` to make a series of ".property" steps for a `Person`.

```haskell graph_io
-- | Write 'Person' properties into a 'VPerson' vertex.
writePerson :: Person -> Binder (Walk SideEffect VPerson VPerson)
writePerson p = fmap writeKeyValues $ sequence $
                [ keyName <=:> personName p,
                  keyAge <=:> personAge p,
                  keyCompany <=?> personCompany p
                ]

-- | Add a new 'VPerson' vertex.
addPerson :: Person -> Binder (GTraversal SideEffect () VPerson)
addPerson p = writePerson p <*.> (pure $ sAddV "person" $ source "g")

specWrite :: Spec
specWrite =
  specify "property writers" $ do
    let p1 = Person "josh" 32 (Just "marko")
        (script1, binding1) = runBinder $ addPerson p1
    toGremlin script1 `shouldBe`
      "g.addV(\"person\").property(\"name\",__v0).property(\"age\",__v1).property(\"company\",__v2).identity()"
    binding1 `shouldBe`
      KM.fromList [ ("__v0", A.String "josh"),
                    ("__v1", A.Number 32),
                    ("__v2", A.String "marko")
                  ]
```

Note that properties in the Haskell program are sent to the Gremlin Server using variable binding. That is why we use `Binder` monad and monadic operators like `<=:>`, `<=?>` and `<*.>`.

Note also that we should use `<=?>` (not `<=:>`) to write an optional field `personCompany`. Basically TinkerPop's graph implementations don't allow writing "null" as a property value. So, if the optional field does not have a value, you should not generate ".property" step for it. The operator `<=?>` and `writeKeyValues` function take care of it.

```haskell graph_io
    let p2 = Person "peter" 35 Nothing
        (script2, binding2) = runBinder $ addPerson p2
    toGremlin script2 `shouldBe`
      "g.addV(\"person\").property(\"name\",__v0).property(\"age\",__v1).identity()"
    binding2 `shouldBe`
      KM.fromList [ ("__v0", A.String "peter"),
                    ("__v1", A.Number 35)
                  ]
```

### Read properties

The most basic way to read properties from the graph is to use ".valueMap" step. In greskell, you can use `gValueMap` function, which generates a `PMap` object as a result.

```haskell graph_io
personProps :: Walk Transform VPerson (PMap Multi GValue)
personProps = gValueMap KeysNil

specRead1 :: Spec
specRead1 = specify "property readers1" $ do
  toGremlin (source "g" & sV [] &. personProps) `shouldBe` "g.V().valueMap()"
```

`PMap` is a map of property key-values. You can use the `Key`s to get values from it.

```haskell graph_io
parsePerson :: PMap Multi GValue -> Either PMapLookupException Person
parsePerson pm =
  Person
  <$> (lookupAs keyName pm)
  <*> (lookupAs keyAge pm)
  <*> (lookupAs' keyCompany pm)
```

Note that you should use `lookupAs'` (not `lookupAs`) to read an optional field. `lookupAs'` treats lack of the key as `Nothing`, while `lookupAs` treats it as an error.

If you need more information than `gValueMap` can provide, you should probably use `gProject`. You often see such a case when you deal with data models for edges.

```haskell graph_io
data Knows =
  Knows
  { knowSubject :: Name,
    -- ^ Name of a person who knows
    knowObject :: Name,
    -- ^ Name of a person who is known
    knowWeight :: Double
  }
  deriving (Show,Eq,Ord)

-- | An Edge corresponding to 'Knows'.
newtype EKnows = EKnows AEdge
        deriving (Eq,Show,FromGraphSON,ElementData,Element,Edge)

keyWeight :: Key EKnows Double
keyWeight = "weight"
```

`knowSubject` and `knowObject` fields are not included in the properties of a "knows" edge, but they are properties of "person" vertices the edge connects. To get all information at once, `gProject` is useful.

```haskell graph_io
labelSubject :: AsLabel Name
labelSubject = "sub"

labelObject :: AsLabel Name
labelObject = "obj"

labelProps :: AsLabel (PMap Single GValue)
labelProps = "props"

knowsInfo :: Walk Transform EKnows (PMap Single GValue)
knowsInfo = gProject
            ( gByL labelSubject (gKnowSub >>> gValues [keyName]) )
            [ gByL labelObject  (gKnowObj >>> gValues [keyName]),
              gByL labelProps   (gValueMap KeysNil)
            ]
  where
    gKnowSub :: Walk Transform EKnows VPerson
    gKnowSub = gOutV
    gKnowObj :: Walk Transform EKnows VPerson
    gKnowObj = gInV

specRead2 :: Spec
specRead2 = specify "property readers2" $ do
  toGremlin (source "g" & sE [] &. knowsInfo)
    `shouldBe`
    ( "g.E().project(\"sub\",\"obj\",\"props\")"
      <> ".by(__.outV().values(\"name\")).by(__.inV().values(\"name\")).by(__.valueMap())"
    )
```

`gProject` takes one or more pairs of label and sub-traversal. Its result is a `PMap` where the key is the label and the value is the result of the sub-traversal. If you use `gValueMap` in a sub-traversal, its result `PMap` is nested.

To parse the result of `gProject`, you can use the labels and keys defined above.

```haskell graph_io
parseKnows :: PMap Single GValue -> Either PMapLookupException Knows
parseKnows pm =
  Knows
  <$> (lookupAs labelSubject pm)
  <*> (lookupAs labelObject pm)
  <*> (lookupAs keyWeight =<< lookupAs labelProps pm)
```

### Embed property data types

In the above examples, you cannot use property data types (`Person` and `Knows`) directly in greskell expressions. Instead, you first have to read out a `PMap` from the Gremlin Server, and then parse it into `Person` or `Knows`. Often it'd be more type-safe and semantic to read `Person` and `Knows` directly from the Gremlin Server.

To embed your property data types directly into greskell, you have to define `FromGraphSON` instance for them. That's acutally so easy, because we already define parsers for them.

```haskell graph_io
instance FromGraphSON Person where
  parseGraphSON gv = (pMapToFail . parsePerson) =<< parseGraphSON gv
```

In the above, `gv` is first parsed into a `PMap`, which is then parsed by `parsePerson`. `pMapToFail` just converts `Either` into `Parser`.

To make it type-safe, you should define a dedicated traversal to get a `Person` object.

```haskell graph_io
getPerson :: Walk Transform VPerson Person
getPerson = unsafeCastEnd personProps
```

`unsafeCastEnd` function converts the end type of the walk from `PMap` to `Person`. We know that `Person` is parsed from the `PMap`, so we can tolerate this unsafe cast here.

The same goes for `Knows`, too.

```haskell graph_io
instance FromGraphSON Knows where
  parseGraphSON gv = (pMapToFail . parseKnows) =<< parseGraphSON gv

getKnows :: Walk Transform EKnows Knows
getKnows = unsafeCastEnd knowsInfo
```

Now, by putting them all together, we can write and read data to/from the graph database like the following.

```haskell graph_io
addKnows :: Name -> Double -> Binder (Walk SideEffect VPerson EKnows)
addKnows target_name weight = do
  vtarget <- newBind target_name
  vweight <- newBind weight
  return $
    gAddE "knows" (gTo (gV [] >>> gHas2 keyName vtarget)) >>> gProperty keyWeight vweight

specIO :: Spec
specIO = specify "write and read graph properties" $ do
  -- Run this test only when we get host and port from the environment variables
  (host, port) <- needEnvHostPort Want "GRESKELL_TEST_README"
  
  bracket (connect host port) close $ \client -> do
    -- Clear graph.
    drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing

    -- Add and get a Person vertex.
    let input_p1 = Person "josh" 32 (Just "marko")
    drainResults =<< (submitPair client $ runBinder $ addPerson input_p1)
    got_p1 <- fmap toList $ slurpResults =<<
              submit client (getPerson $. sV [] $ source "g") Nothing
    got_p1 `shouldBe` [input_p1]

    -- Add another Person, add a Knows edge and get it.
    let input_p2 = Person "marko" 29 Nothing
        expected_k = Knows "marko" "josh" 1.0
    got_k <- fmap toList $ slurpResults =<<
             ( submitPair client $ runBinder $
               liftWalk getKnows <$.> addKnows "josh" 1.0 <*.> addPerson input_p2
             )
    got_k `shouldBe` [expected_k]
```

## Todo

- Complete graph traversal steps API.


## Author

Toshio Ito <debug.ito@gmail.com>
