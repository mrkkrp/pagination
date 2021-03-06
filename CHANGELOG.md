## Pagination 0.2.2

* Works with 9.0.1. Dropped support for GHC 8.6 and older.

## Pagination 0.2.1

* Fix test suite failure with `QuickCheck-2.10`.

## Pagination 0.2.0

* Drop the `Applicative` instance of `Paginated` as it may lead to confusing
  results in certain cases.

* Improved documentation and metadata.

## Pagination 0.1.1

* Relax constraint of `paginate`. We only need `Functor` here, not `Monad`.

## Pagination 0.1.0

* Initial release.
