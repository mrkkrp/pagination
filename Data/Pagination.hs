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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Data.Pagination
  ( -- * Pagination settings
    Pagination
  , mkPagination
  , pageSize
  , pageIndex
    -- * Paginated data
  , Paginated
  , paginate
  , paginatedItems
  , paginatedPagination
  , paginatedPagesTotal
  , paginatedItemsTotal
  , hasOtherPages
  , pageRange
  , hasPrevPage
  , hasNextPage
  , backwardEllip
  , forwardEllip
    -- * Exceptions
  , PaginationException (..) )
where

import Control.DeepSeq
import Control.Monad.Catch
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import GHC.Generics
import Numeric.Natural
import qualified Data.List.NonEmpty as NE

----------------------------------------------------------------------------
-- Pagination settings

-- | Settings that are required to organize data in paginated form.

data Pagination = Pagination Natural Natural
  deriving (Eq, Show, Data, Typeable, Generic)

instance NFData Pagination

-- | Create a 'Pagination' value. May throw 'PaginationException'.

mkPagination :: MonadThrow m
  => Natural           -- ^ Page size
  -> Natural           -- ^ Page index
  -> m Pagination      -- ^ The pagination settings
mkPagination size index
  | size  == 0 = throwM ZeroPageSize
  | index == 0 = throwM ZeroPageIndex
  | otherwise  = return (Pagination size index)

-- | Get page size (maximum number of items on a page) from a 'Pagination'.

pageSize :: Pagination -> Natural
pageSize (Pagination size _) = size

-- | Get page index from a 'Pagination'.

pageIndex :: Pagination -> Natural
pageIndex (Pagination _ index) = index

----------------------------------------------------------------------------
-- Paginated data

-- | Data in the paginated form.

data Paginated a = Paginated
  { pgItems      :: [a]
  , pgPagination :: Pagination
  , pgPagesTotal :: Natural
  , pgItemsTotal :: Natural
  } deriving (Eq, Show, Data, Typeable, Generic, Functor)

instance NFData a => NFData (Paginated a)

instance Foldable Paginated where
  foldr f x = foldr f x . pgItems

instance Traversable Paginated where
  traverse f p =
    let g p' xs = p' { pgItems = xs }
    in g p <$> traverse f (pgItems p)

-- | Create paginated data.

paginate :: (Functor m, Integral n)
  => Pagination        -- ^ Pagination options
  -> Natural           -- ^ Total number of items
  -> (n -> n -> m [a])
     -- ^ The element producing callback. The function takes arguments:
     -- offset and limit.
  -> m (Paginated a)   -- ^ The paginated data
paginate (Pagination size index') totalItems f =
  r <$> f (fromIntegral offset) (fromIntegral size)
  where
    r xs = Paginated
      { pgItems      = xs
      , pgPagination = Pagination size index
      , pgPagesTotal = totalPages
      , pgItemsTotal = totalItems }
    (whole, rems) = totalItems `quotRem` size
    totalPages    = max 1 (whole + if rems == 0 then 0 else 1)
    index         = min index' totalPages
    offset        = (index - 1) * size

-- | Get subset of items for current page.

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

-- | Get range of pages to show before and after the current page. This does
-- not necessarily include the first and the last pages (they are supposed
-- to be shown in all cases). Result of the function is always sorted.

pageRange
  :: Paginated a       -- ^ Paginated data
  -> Natural           -- ^ Number of pages to show before and after
  -> NonEmpty Natural  -- ^ Page range
pageRange Paginated {..} 0 = NE.fromList [pageIndex pgPagination]
pageRange Paginated {..} n =
  let len   = min pgPagesTotal (n * 2 + 1)
      index = pageIndex pgPagination
      shift | index <= n                = 0
            | index >= pgPagesTotal - n = pgPagesTotal - len
            | otherwise                 = index - n - 1
  in (+ shift) <$> NE.fromList [1..len]

-- | Backward ellipsis appears when page range (pages around current page to
-- jump to) has gap between its beginning and the first page.

backwardEllip
  :: Paginated a       -- ^ Paginated data
  -> Natural           -- ^ Number of pages to show before and after
  -> Bool
backwardEllip p n = NE.head (pageRange p n) > 2

-- | Forward ellipsis appears when page range (pages around current page to
-- jump to) has gap between its end and the last page.

forwardEllip
  :: Paginated a       -- ^ Paginated data
  -> Natural           -- ^ Number of pages to show before and after
  -> Bool              -- ^ Do we have forward ellipsis?
forwardEllip p@Paginated {..} n = NE.last (pageRange p n) < pred pgPagesTotal

----------------------------------------------------------------------------
-- Exceptions

-- | Exception indicating various problems when working with paginated data.

data PaginationException
  = ZeroPageSize  -- ^ Page size (number of items per page) was zero
  | ZeroPageIndex -- ^ Page index was zero (they start from one)
  deriving (Eq, Show, Data, Typeable, Generic)

instance NFData PaginationException
instance Exception PaginationException
