# Revision history for greskell-core

## 0.1.2.0  -- (draft)

* Add `GMap` module.
* Add `AsIterator` module.
* Add `GraphSON.GValue` module.

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
