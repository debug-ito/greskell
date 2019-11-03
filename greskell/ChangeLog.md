# Revision history for greskell

## 1.0.0.0

* Add some test cases to server-behavior-test

### Graph module

* Add `unsafeCastKey` function.
* [BREAKING CHANGE] Remove `ElementID` type family from `Element` class.
* [BREAKING CHANGE] Add `ElementID` as a newtype for `GValue`.
* [BREAKING CHANGE] Add `elementId` and `elementLabel` functions to `Element` class.
* [BREAKING CHANGE] Remove `Vertex`, `Edge`, `PropertyMap` and
  `FromGraphSONWithKey` classes.
* [BREAKING CHANGE] Remove `PropertyMapSingle` and `PropertyMapList`
  types. Remove functions about those types.
* [BREAKING CHANGE] Now `tKey` and `tValue` are monomorphic for `AVertexProperty`.
* [BREAKING CHANGE] Remove `avProperties` field from `AVertex`.
* [BREAKING CHANGE] Remove `aeInVLabel`, `aeOutVLabel`, `aeInV`,
  `aeOutV` and `aeProperties` fields from `AEdge`.
* [BREAKING CHANGE] Remove `avpProperties` field from `AVertexProperty`.


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
