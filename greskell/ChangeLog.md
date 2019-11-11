# Revision history for greskell

## 1.0.0.0

* Add some test cases to server-behavior-test
* Add PMap and NonEmptyLike modules.

### Data.Greskell module

* Export PMap module.

### Graph module

* Add `unsafeCastKey` function.
* [BREAKING CHANGE] Remove `ElementID` type family from `Element` class.
* Add `ElementPropertyContainer` type family to `Element` class.
* [BREAKING CHANGE] Add `ElementData` class, and set it as a super class of `Element`.
* [BREAKING CHANGE] Add `ElementID` as a newtype for `GValue`.
* Add `unsafeCastElementID` function.
* [BREAKING CHANGE] Remove `Vertex` and `Edge` classes.
* [BREAKING CHANGE] `PropertyMap` class, `FromGraphSONWithKey` class,
  `PropertyMapSingle` type, `PropertyMapList` types and related
  functions are moved to `Graph.PropertyMap` module and are now
  deprected. Use `PMap` module instead.
* [BREAKING CHANGE] Now `tKey` and `tValue` are monomorphic for `AVertexProperty`.
* [BREAKING CHANGE] Remove `avProperties` field from `AVertex`.
* [BREAKING CHANGE] Remove `aeInVLabel`, `aeOutVLabel`, `aeInV`,
  `aeOutV` and `aeProperties` fields from `AEdge`.
* [BREAKING CHANGE] Remove `avpProperties` field from `AVertexProperty`.
* [BREAKING CHANGE] Type of the ID field for `AVertex`, `AEdge` and
  `AVertexProperty` is now `ElementID`, not `GValue`.
* [BREAKING CHANGE] Internal of `Key` is modified from `Greskell Text`
  to `Text`, so that it can be an instance of `PMapKey` class.
* Add `Keys` type and related functions.

### GTraversal module

* [BREAKING CHANGE] Change the signature of the following
  functions. Because `Vertex`, `Edge` and `PropertyMap` classes are
  gone, those functions are now all monomorphic with `AVertex`,
  `AEdge` and `AVertexProperty`.

    * `sV`, `sV'`
    * `sE`, `sE'`
    * `sAddV`
    * `gHasKey`, `gHasKey'`, `gHasKeyP`, `gHasKeyP'`
    * `gHasValue`, `gHasValue'`, `gHasValueP`, `gHasValueP'`
    * `gV`, `gV'`
    * `gOut`, `gOut'`
    * `gOutE`, `gOutE'`
    * `gOutV`, `gOutV'`
    * `gIn`, `gIn'`
    * `gInE`, `gInE'`
    * `gInV`, `gInV'`
    * `gAddV`
    * `gPropertyV`
    * `gFrom`
    * `gTo`
    * `gAddE`

* Add `gValueMap`, `gProject` and `gByL` functions.
* Add `LabeledByProjection` type.

### Extra module

* [BREAKING CHANGE] Remove `writeAllProperties` because `PropertyMap`
  is deprecated.
* Add `writePropertyKeyValues'` function.

### AsLabel module

* Now `SelectedMap` is generalized by `PMap`.
* Make `AsLabel` an instance of `PMapKey`.
* [BREAKING CHANGE] `lookup`, `lookupM`, `lookupAs` and `lookupAsM`
  are now re-exports from `PMap` module, whose signature is more
  polymorphic than the original ones and have different an exception
  type.
* [BREAKING CHANGE] `AsLookupException` is removed. Use
  `PMapLookupException` instead.
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
