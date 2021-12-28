# Revision history for greskell-core

## 1.0.0.0  -- ?

* **BREAKING CHANGE**:  `GObject` variant of `GValueBody` is now based on `KeyMap` from `aeson-2.0`.
  Before, it was based on `HashMap Text`.
  As a result, signature of the following functions has been changed.
  * `parseToGMap`
  * `parseToGMapEntry`
  * `(.:)`
* **BREAKING CHANGE**: Remove `FromGraphSON` instance for `Data.Semigroup.Option`, which is deprecated.
* Add `FromGraphSON` instance to `KeyMap` and `Key` from `aeson-2.0`.
* Confirm test with `aeson-2.0.2.0`, `semigroups-0.20` and `hashable-1.4.0.1`, `doctest-0.19.0`, `doctest-0.20.0`.


## 0.1.3.7  -- 2021-11-08

* Confirm test with `base-4.15.0.0`

## 0.1.3.6  -- 2021-02-11

* Confirm test with `doctest-0.18`.

## 0.1.3.5  -- 2020-06-21

* Confirm test with `base-4.14.0.0`

## 0.1.3.4  -- 2020-06-06

* Support `doctest-0.17`.

## 0.1.3.3  -- 2020-05-30

* Support `aeson-1.5.0.0`.

## 0.1.3.2  -- 2020-03-29

* Confirm test with `QuickCheck-2.14`.

## 0.1.3.1  -- 2019-12-30

* Confirm test with `base-4.13.0.0`

## 0.1.3.0  -- 2019-12-27

### GraphSON module

* Add `FromGraphSON` instances to the following wrapper types.
    * `Identity` functor
    * `NonEmpty` list
    * From `Data.Semigroup`
        * `Min`
        * `Max`
        * `First`
        * `Last`
        * `WrappedMonoid`
        * `Dual`
        * `Option`
    * From `Data.Monoid`
        * `First`
        * `Last`
        * `Sum`
        * `Product`
        * `All`
        * `Any`

### AsIterator module

* Add `AsIterator` instance to `NonEmpty`.

## 0.1.2.7  -- 2019-10-02

* Confirm test with `hashable-1.3.0.0` and `semigroups-0.19.1`.

## 0.1.2.6  -- 2019-06-10

* Adapt test spec to `aeson-1.4.3.0` (#1).


## 0.1.2.5  -- 2019-03-31

* Confirm test with `QuickCheck-2.13.1`.

## 0.1.2.4  -- 2018-10-03

* Confirm test with `base-4.12.0.0` and `containers-0.6.0.1`


## 0.1.2.3  -- 2018-09-05

* Confirmed test with `QuickCheck-2.12` and `hspec-2.5.6`.


## 0.1.2.2  -- 2018-07-24

* Confirmed test with `doctest-discover-0.2.0.0`.


## 0.1.2.1  -- 2018-06-24

* Confirmed test with `doctest-0.16.0`.


## 0.1.2.0  -- 2018-06-21

* Add `GMap` module.
* Add `AsIterator` module.
* Add `GraphSON.GValue` module.
* Confirmed test with `aeson-1.4.0.0`.

### GraphSON module

* Change behavior of `instance FromJSON GraphSON`. Now {"@type": null}
  goes to failure. Before, "@type":null fell back to direct (bare)
  parsing. If it finds "@type" key, I think it should expect that the
  JSON object is a GraphSON wrapper. It's more or less a bug fix, so
  it doesn't bump major version.
* Add `Generic` and `Hashable` instances to `GraphSON`.
* Add `GValue` and `GValueBody` types and related functions.
* Add `FromGraphSON` class and related functions.
* Add `instance GraphSONTyped Either`.
* Add `instance GraphSONTyped` to types in `containers` package.
* Re-export Aeson's `Parser` type for convenience.

### Greskell module

* Add `valueInt`, `gvalue`, `gvalueInt` functions.


## 0.1.1.0  -- 2018-04-08

* Add Semigroup instance to Greskell.
* Confirmed test with base-4.11.


## 0.1.0.0  -- 2018-03-12

* First version. Released on an unsuspecting world.
