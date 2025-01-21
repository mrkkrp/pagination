{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Data.Pagination
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Framework-agnostic pagination boilerplate.
module Data.Pagination
  ( -- * Pagination settings
    Pagination,
    mkPagination,
    pageSize,
    pageIndex,

    -- * Paginated data
    Paginated,
    paginate,
    paginatedItems,
    paginatedPagination,
    paginatedPagesTotal,
    paginatedItemsTotal,
    hasOtherPages,
    pageRange,
    hasPrevPage,
    hasNextPage,
    backwardEllip,
    forwardEllip,

    -- * Exceptions
    PaginationException (..),
  )
where

import Control.DeepSeq
import Control.Monad.Catch
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import GHC.Generics
import Numeric.Natural

----------------------------------------------------------------------------
-- Pagination settings

-- | Pagination settings.
data Pagination = Pagination Natural Natural
  deriving (Eq, Show, Data, Generic)

instance NFData Pagination

-- | Create a 'Pagination' value. May throw 'PaginationException'.
mkPagination ::
  (MonadThrow m) =>
  -- | Page size
  Natural ->
  -- | Page index
  Natural ->
  -- | Pagination settings
  m Pagination
mkPagination size index
  | size == 0 = throwM ZeroPageSize
  | index == 0 = throwM ZeroPageIndex
  | otherwise = return (Pagination size index)

-- | Get the page size (the maximum number of items on a page) from a
-- 'Pagination'.
pageSize :: Pagination -> Natural
pageSize (Pagination size _) = size

-- | Get the page index from a 'Pagination'.
pageIndex :: Pagination -> Natural
pageIndex (Pagination _ index) = index

----------------------------------------------------------------------------
-- Paginated data

-- | Data in the paginated form.
data Paginated a = Paginated
  { pgItems :: [a],
    pgPagination :: Pagination,
    pgPagesTotal :: Natural,
    pgItemsTotal :: Natural
  }
  deriving (Eq, Show, Data, Generic, Functor)

instance (NFData a) => NFData (Paginated a)

instance Foldable Paginated where
  foldr f x = foldr f x . pgItems

instance Traversable Paginated where
  traverse f p =
    let g p' xs = p' {pgItems = xs}
     in g p <$> traverse f (pgItems p)

-- | Create paginated data.
paginate ::
  (Functor m, Integral n) =>
  -- | Pagination options
  Pagination ->
  -- | Total number of items
  Natural ->
  -- | The element producing callback. The function takes arguments:
  -- offset and limit.
  (n -> n -> m [a]) ->
  -- | The paginated data
  m (Paginated a)
paginate (Pagination size index') totalItems f =
  r <$> f (fromIntegral offset) (fromIntegral size)
  where
    r xs =
      Paginated
        { pgItems = xs,
          pgPagination = Pagination size index,
          pgPagesTotal = totalPages,
          pgItemsTotal = totalItems
        }
    (whole, rems) = totalItems `quotRem` size
    totalPages = max 1 (whole + if rems == 0 then 0 else 1)
    index = min index' totalPages
    offset = (index - 1) * size

-- | Get the items for current page.
paginatedItems :: Paginated a -> [a]
paginatedItems = pgItems

-- | Get 'Pagination' parameters that were used to create this paginated
-- result.
paginatedPagination :: Paginated a -> Pagination
paginatedPagination = pgPagination

-- | Get the total number of pages in this collection.
paginatedPagesTotal :: Paginated a -> Natural
paginatedPagesTotal = pgPagesTotal

-- | Get the total number of items in this collection.
paginatedItemsTotal :: Paginated a -> Natural
paginatedItemsTotal = pgItemsTotal

-- | Test whether there are other pages.
hasOtherPages :: Paginated a -> Bool
hasOtherPages Paginated {..} = pgPagesTotal > 1

-- | Is there previous page?
hasPrevPage :: Paginated a -> Bool
hasPrevPage Paginated {..} = pageIndex pgPagination > 1

-- | Is there next page?
hasNextPage :: Paginated a -> Bool
hasNextPage Paginated {..} = pageIndex pgPagination < pgPagesTotal

-- | Get the range of pages to show before and after the current page. This
-- does not necessarily include the first and the last pages (they are
-- supposed to be shown in all cases). Result of the function is always
-- sorted.
pageRange ::
  -- | Paginated data
  Paginated a ->
  -- | Number of pages to show before and after
  Natural ->
  -- | Page range
  NonEmpty Natural
pageRange Paginated {..} 0 = NE.fromList [pageIndex pgPagination]
pageRange Paginated {..} n =
  let len = min pgPagesTotal (n * 2 + 1)
      index = pageIndex pgPagination
      shift
        | index <= n = 0
        | index >= pgPagesTotal - n = pgPagesTotal - len
        | otherwise = index - n - 1
   in (+ shift) <$> NE.fromList [1 .. len]

-- | Backward ellipsis appears when page range (pages around current page to
-- jump to) has gap between its beginning and the first page.
backwardEllip ::
  -- | Paginated data
  Paginated a ->
  -- | Number of pages to show before and after
  Natural ->
  Bool
backwardEllip p n = NE.head (pageRange p n) > 2

-- | Forward ellipsis appears when page range (pages around current page to
-- jump to) has gap between its end and the last page.
forwardEllip ::
  -- | Paginated data
  Paginated a ->
  -- | Number of pages to show before and after
  Natural ->
  -- | Do we have forward ellipsis?
  Bool
forwardEllip p@Paginated {..} n = NE.last (pageRange p n) < pred pgPagesTotal

----------------------------------------------------------------------------
-- Exceptions

-- | Exception indicating various problems when working with paginated data.
data PaginationException
  = -- | Page size (number of items per page) was zero
    ZeroPageSize
  | -- | Page index was zero (they start from one)
    ZeroPageIndex
  deriving (Eq, Show, Data, Generic)

instance NFData PaginationException

instance Exception PaginationException
