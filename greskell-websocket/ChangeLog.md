# Revision history for greskell-websocket

## 1.0.0.0  -- ?

* **BREAKING CHANGE**: Now greskell-websocket uses `aeson-2`.
  As a result, now implemenation of `Data.Aeson.Object` has been changed from `HashMap` to `KeyMap`.
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
