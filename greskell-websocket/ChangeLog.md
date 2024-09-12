# Revision history for greskell-websocket

## 1.0.0.2  -- 2024-09-12

* Update dependency version bounds with cabal-plan-bounds.
  This adds support for new packages, while drops support for old ones.

## 1.0.0.1  -- 2022-11-24

* Confirm test with ghc-9.2.5, vector-0.13.0.0 and aeson-2.1.0.0.
* Remove doctests. This is because it's so difficult to maintain doctests with recent GHCs and cabals.
  * doctests have been moved to `examples` function defined in some modules.
* Use stylish-haskell to format codes.

## 1.0.0.0  -- 2021-12-28

* **BREAKING CHANGE**: Now greskell-websocket uses `aeson-2`.
  As a result, now implementation of `Data.Aeson.Object` has been changed from `HashMap` to `KeyMap`.
  Some types in greskell-websocket (e.g. `ResponseResult`) directly uses the `Object` type.

* Confirm test with `aeson-2.0.2.0`, `hashtables-1.3`, `greskell-core-1.0`.

## 0.1.2.6  -- 2021-11-08

* Confirm test with `base-4.15.0.0`

## 0.1.2.5  -- 2020-12-30

* Confirm test with `base64-bytestring-1.2.0.0`

## 0.1.2.4  -- 2020-06-21

* Confirm test with `base-4.14.0.0`.

## 0.1.2.3  -- 2020-05-30

* Support `aeson-1.5.0.0`.

## 0.1.2.2  -- 2020-05-04

* Confirm test with `base64-bytestring-1.1.0.0`.

## 0.1.2.1  -- 2019-12-30

* Confirm test with `base-4.13.0.0`

## 0.1.2.0  -- 2019-12-27

* Add `submitPair` function to Client module.

## 0.1.1.2  -- 2018-10-03

* Confirm test with `base-4.12.0.0`


## 0.1.1.1  -- 2018-09-23

* Confirm test with `stm-2.5.0.0`.


## 0.1.1.0  -- 2018-08-17

### Client module

* Add `drainResults` function.

### Connection module

* Add `drainResponses` function.


## 0.1.0.0  -- 2018-06-21

* First version. Released on an unsuspecting world.
