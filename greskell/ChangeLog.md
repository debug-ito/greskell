# Revision history for greskell

## 1.0.1.0  -- ???

### GTraversal module

* Add the following functions.
  * `gRepeat`
  * `gTimes`
  * `gUntilHead`
  * `gUntilTail`
  * `gEmitHead`
  * `gEmitTail`
  * `gEmitAlwaysHead`
  * `gEmitAlwaysTail`
  * `gLoops`
  * `gIs`
  * `gIs'`
  * `gIsP`
  * `gIsP'`
  * `gCyclicPath`
  * `gCyclicPath'`
  * `gSimplePath`
  * `gSimplePath'`
  * `gUnion`
  * `gChoose3`
  * `gConstant`
  * `gLocal`
  * `gBarrier`
* Add the follwing types.
  * `RepeatUntil`
  * `RepeatEmit`
  * `RepeatPos`
  * `RepeatLabel`


## 1.0.0.1  -- 2019-12-30

* Confirm test with `base-4.13.0.0`

## 1.0.0.0  -- 2019-12-27

* Add some test cases to server-behavior-test
* Add PMap and NonEmptyLike modules.

### Data.Greskell module

* Export PMap module.

### Graph module - Element class and ElementID

* [BREAKING CHANGE] Now `ElementID` is a newtype for `GValue`. It was
  an associated type family in `Element` class. However, I think there
  was no point to make it polymorphic.
* [BREAKING CHANGE] Remove `EdgeVertexID` type family from `Edge` class.
* [BREAKING CHANGE] Add `ElementData` class, and set it as a super class of `Element`.
* [BREAKING CHANGE] Type of the ID field for `AVertex`, `AEdge` and
  `AVertexProperty` is now `ElementID`, not `GValue`.
* Add `unsafeCastElementID` function.
* Add `ElementPropertyContainer` type family to `Element` class. It's
  necessary for `gValueMap` traversal.

### Graph module - "reference" model for graph element types

Now the graph element types don't contain properties. They only have
their ID and label (and the value if it's a VertexProperty). This is
because some graph implementation of TinkerPop don't return properties
when it returns graph elements. See #6.

* [BREAKING CHANGE] Remove `avProperties` field from `AVertex`.
* [BREAKING CHANGE] Remove `aeInVLabel`, `aeOutVLabel`, `aeInV`,
  `aeOutV` and `aeProperties` fields from `AEdge`.
* [BREAKING CHANGE] Remove `avpProperties` field from `AVertexProperty`.
* [BREAKING CHANGE] Move `PropertyMap` class, `FromGraphSONWithKey`
  class, `PropertyMapSingle` type, `PropertyMapList` types and related
  functions to `Graph.PropertyMap` module. They are now all
  deprecated. Use `PMap` module instead.

### Graph module - Key for element property

* [BREAKING CHANGE] Internal of `Key` is modified from `Greskell Text`
  to `Text`, so that it can be an instance of `PMapKey` class.
* [BREAKING CHANGE] Add `KeyNoValue` data constructor for `KeyValue`
  type.
* Add `unsafeCastKey` function.
* Add `Keys` type and related functions.

### GTraversal module

* [BREAKING CHANGE] Change the signature of the following functions
  because now `ElementID` is a newtype, not a type alias.
    * `sV'`
    * `gV'`
    * `sE'`
* Add `gValueMap`, `gProject` and `gByL` functions.
* Add `LabeledByProjection` type.
* Add `unsafeCastStart` and `unsafeCastEnd` functions to `ToGTraversal` class.

### Extra module

* [BREAKING CHANGE] Remove `writeAllProperties` because `PropertyMap`
  is deprecated.
* Add `writeKeyValues` function.
* Add `writePMapProperties` function.
* Add `(<=:>)` and `(<=?>)` operators.
* Re-export property reader functions such as `lookupAs`.

### AsLabel module

* Now `SelectedMap` is a specialized type alias for `PMap`.
* [BREAKING CHANGE] `lookup`, `lookupM`, `lookupAs` and `lookupAsM`
  are now re-exports from `PMap` module, whose signature is more
  polymorphic than the original ones and have a different exception
  type.
* [BREAKING CHANGE] `AsLookupException` is removed. Use
  `PMapLookupException` instead.
* Make `AsLabel` an instance of `PMapKey`.
* Add `IsString` instance to `AsLabel`.


## 0.2.3.1  -- 2019-10-02

* Confirm test with `semigroups-0.19.1`.

## 0.2.3.0  -- 2019-01-01

* Add Extra module.

## 0.2.2.0  -- 2018-11-23

* Add new `AsLabel` module.

### GTraversal module

* Add `gAs`, `gSelect1`, `gSelectN`, `gSelectBy1`, `gSelectByN`,
  `gOutV`, `gOutV'`, `gInV`, `gInV'` functions.

### Binder module

* Add `newAsLabel` function.



## 0.2.1.1  -- 2018-10-03

* Confirm test with `base-4.12.0.0`


## 0.2.1.0  -- 2018-08-17

### GTraversal module

* Add `(<$.>)`, `(<*.>)`, `gLimit`, `gTail`, `gSkip`, `gId`, `gLabel`
  functions.


## 0.2.0.3  -- 2018-07-24

* Confirmed test with `doctest-discover-0.2.0.0`.


## 0.2.0.2  -- 2018-06-24

* Confirmed test with `doctest-0.16.0`.


## 0.2.0.1  -- 2018-06-24

* .cabal: Now `hint-test` flag has `manual: True` setting. The
  hint-test may be sometimes unstable, so the user should be able to
  disable it in a stable way.
  See https://github.com/commercialhaskell/stackage/pull/3758


## 0.2.0.0  -- 2018-06-21

* Confirmed test with `aeson-1.4.0.0` and `hint-0.8.0`.

### Graph module

* **BREAKING CHANGE**: Now `AVertex`, `AEdge`, `AVertexProperty` are
  based on `GValue` type, instead of `GraphSON Value`. This is for
  taking care of possibly nested GraphSON-encoded values in a generic
  way.
* **BREAKING CHANGE**: `parseOneValue`, `parseListValues`,
  `parseNonEmptyValues` functions now require `FromGraphSON`
  constraint, instead of `FromJSON`, because property types of
  `AVertex` etc have changed.
* Add `Cardinality` type and its enum instances.
* Add `KeyValue` type and `(=:)` operator.

* (internal change): `PropertyMapGeneric` is now based on
  `HashMap.Strict`.

### The top Data.Greskell module

* Re-export `GMap` and `AsIterator` modules.

### GTraversal module

* Add `gV`, `gV'`, `gAddE`, `gAddE'`, `gFrom`, `gTo`, `gProperty`,
  `gPropertyV`, `sAddV`, `sAddV'`.


## 0.1.1.0  -- 2018-04-08

* Add Semigroup instance to PropertyMapSingle and PropertyMapList.
* Confirmed test with base-4.11 (with hint-test and server-test disabled)


## 0.1.0.0  -- 2018-03-12

* First version. Released on an unsuspecting world.
