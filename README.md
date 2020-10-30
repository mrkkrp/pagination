# Pagination

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/pagination.svg?style=flat)](https://hackage.haskell.org/package/pagination)
[![Stackage Nightly](http://stackage.org/package/pagination/badge/nightly)](http://stackage.org/nightly/package/pagination)
[![Stackage LTS](http://stackage.org/package/pagination/badge/lts)](http://stackage.org/lts/package/pagination)
![CI](https://github.com/mrkkrp/pagination/workflows/CI/badge.svg?branch=master)

The package implements pagination boilerplate in a framework-agnostic way.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/pagination/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.

# Short example

With the following dependencies:
```
dependencies:
- base
- pagination
- exceptions
- mtl
```

```
module Main where

import Data.Pagination
import Control.Monad.Identity
import Control.Monad.Catch

data MyPage = MyPage Int deriving Show

example :: MonadThrow m => m Pagination
example = do
  mkPagination 10 1

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  let vals = [0..100]
  print vals

  a <- example
  print $ a
  print . pageSize $ a
  print . pageIndex $ a
  putStrLn "----------------------------"
  let paginated = paginate a 100 (\a b -> Identity [MyPage a, MyPage b])
  print $ paginated
  print $ runIdentity paginated
  let (MyPage start:MyPage length:[]) = paginatedItems (runIdentity paginated)
  print $ slice start (start+length-1) vals
```

Outputs:
```
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
Pagination 10 1
10
1
----------------------------
Identity (Paginated {pgItems = [MyPage 0,MyPage 10], pgPagination = Pagination 10 1, pgPagesTotal = 10, pgItemsTotal = 100})
Paginated {pgItems = [MyPage 0,MyPage 10], pgPagination = Pagination 10 1, pgPagesTotal = 10, pgItemsTotal = 100}
[0,1,2,3,4,5,6,7,8,9]
```
