--
-- Tests for the ‘pagination’ package.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE RankNTypes           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad
import Control.Monad.Catch (MonadThrow (..), fromException)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Pagination
import Numeric.Natural
import Test.Hspec
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mkPagination" $ do
    context "when page size is zero" $
      it "throws ZeroPageSize exception" $
        property $ \index ->
          asEither (mkPagination 0 index) === Left ZeroPageSize
    context "when page index in zero" $
      it "throws ZeroPageIndex exception" $
        property $ \size ->
          size > 0 ==> asEither (mkPagination size 0) === Left ZeroPageIndex
    context "when page size and page index are positive" $
      it "we get the Pagination value" $
        property $ \size index ->
          (size > 0 && index > 0) ==> do
            p <- mkPagination size index
            pageSize  p `shouldBe` size
            pageIndex p `shouldBe` index
  describe "Functor instance of Paginated" $
    it "works" $
      property $ \r ->
        let f :: Int -> Int
            f = (+ 1)
        in paginatedItems (f <$> r) === (f <$> paginatedItems r)
  describe "Applicative instance of Paginated" $ do
    it "constructs the right pure Paginated value" $ do
      p <- mkPagination 1 1
      r <- paginate p 1 ((\_ _ -> return [1]) :: Int -> Int -> IO [Int])
      pure (1 :: Int) `shouldBe` r
    it "the (<*>) operator works like with lists" $
      property $ \r0 r1 ->
        let f :: Int -> Int -> Int
            f = (*)
        in paginatedItems (f <$> r0 <*> r1) ===
             (f <$> paginatedItems r0 <*> paginatedItems r1)
  describe "Foldable instance of Paginated" $
    it "foldr works like with lists" $
      property $ \p n ->
        let f :: Foldable f => f Int -> Int
            f = foldr (+) n
        in f p === f (paginatedItems p)
  describe "Traversable instance of Paginated" $
    it "traverse works like with lists" $
      property $ \p ->
        (paginatedItems <$> traverse Just (p :: Paginated Int))
          === Just (paginatedItems p)
  describe "paginate" $
    context "when total number of items is zero" $
      it "produces an empty pagination" $
        property $ \p n -> do
          r <- paginate p 0 $ \offset limit -> do
                 offset `shouldBe` 0
                 limit  `shouldBe` pageSize p
                 return []
          paginatedItems      r `shouldBe` ([] :: [Int])
          (pageSize . paginatedPagination) r `shouldBe` pageSize p
          (pageIndex . paginatedPagination) r `shouldBe` 1
          paginatedPagesTotal r `shouldBe` 1
          paginatedItemsTotal r `shouldBe` 0
          pageRange         r n `shouldBe` 1 :| []
          hasOtherPages       r `shouldBe` False
          hasPrevPage         r `shouldBe` False
          hasNextPage         r `shouldBe` False
          backwardEllip     r n `shouldBe` False
          forwardEllip      r n `shouldBe` False
  describe "paginatedItems" $
    it "number of actual items is less than or equal to page size" $
      property $ \r ->
        let size = pageSize (paginatedPagination (r :: Paginated Int))
        in (fromIntegral . length . paginatedItems) r `shouldSatisfy` (<= size)
  describe "paginatedPagination" $
    it "returns original pagination correcting index if necessary" $
      property $ \p n -> do
        r <- paginate p n $ \offset limit -> do
               let totalPages = ptotal n (pageSize p)
               offset `shouldBe`
                 min ((pageIndex p - 1) * pageSize p)
                     ((totalPages - 1) * pageSize p)
               limit  `shouldBe` pageSize p
               return (replicate (plen n offset limit) (0 :: Int))
        pageSize (paginatedPagination r)  `shouldBe` pageSize p
        pageIndex (paginatedPagination r) `shouldSatisfy` (<= pageIndex p)
  describe "paginatedPagesTotal" $
    it "returns correct number of total pages" $
      property $ \r ->
        let itemsTotal = paginatedItemsTotal (r :: Paginated Int)
            psize      = pageSize (paginatedPagination r)
        in paginatedPagesTotal r `shouldBe` ptotal itemsTotal psize
  describe "paginatedItemsTotal" $
    it "returns the same number of items as it was specified for paginate" $
      property $ \p n -> do
        r <- paginate p n ((\_ _ -> return []) :: Int -> Int -> IO [Int])
        paginatedItemsTotal r `shouldBe` n
  describe "hasOtherPages" $
    it "correctly detects whether we the collection has other pages" $
      property $ \r ->
        hasOtherPages (r :: Paginated Int) `shouldBe` paginatedPagesTotal r > 1
  describe "hasPrevPage" $
    it "correctly detect whether paginated data has previous page" $
      property $ \r ->
        hasPrevPage (r :: Paginated Int) ===
          (pageIndex (paginatedPagination r) /= 1)
  describe "hasNextPage" $
    it "correctly detect whether paginated data has next page" $
      property $ \r ->
        hasNextPage (r :: Paginated Int) ===
          (pageIndex (paginatedPagination r) /= paginatedPagesTotal r)
  describe "pageRange" $
    it "correctly performs generation of page ranges" $
      forM_ [1..10] $ \n -> do
        p <- mkPagination 10 n
        r <- paginate p 95 (\_ limit -> return [1..limit])
        let x = NE.toList (pageRange (r :: Paginated Int) 2)
        x `shouldBe` case n of
          1 -> [1..5]
          2 -> [1..5]
          3 -> [1..5]
          4 -> [2..6]
          5 -> [3..7]
          6 -> [4..8]
          7 -> [5..9]
          8 -> [6..10]
          9 -> [6..10]
          _ -> [6..10]
  describe "backwardEllip" $
    it "correctly detects when there is a backward ellipsis" $
      property $ \r n ->
        backwardEllip (r :: Paginated Int) n ===
          (NE.head (pageRange r n) > 2)
  describe "forwardEllip" $
    it "correctly detects when there is a forward ellipsis" $
      property $ \r n ->
        forwardEllip (r :: Paginated Int) n ===
          (NE.last (pageRange r n) < paginatedPagesTotal r - 1)

----------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary Pagination where
  arbitrary = do
    size  <- p
    index <- p
    (return . fromJust) (mkPagination size index)
    where p = arbitrary `suchThat` (> 0)

instance Arbitrary a => Arbitrary (Paginated a) where
  arbitrary = do
    pagination <- arbitrary
    total      <- arbitrary
    let f offset limit = vector (plen total offset limit)
    paginate pagination total f

----------------------------------------------------------------------------
-- Helpers

-- | Run computation inside 'MonadThrow' and return result as an 'Either'.

asEither :: (forall m. MonadThrow m => m a) -> Either PaginationException a
asEither = either (Left . fromJust . fromException) Right

-- | Calculate number of items in paginated selection given total number of
-- items, offset, and limit.

plen :: Integral n
  => Natural           -- ^ Total items
  -> Natural           -- ^ Offset
  -> Natural           -- ^ Limit
  -> n
plen total offset limit = fromIntegral (min (total - offset) limit)

-- | Calculate total number of pages given total number of items, and page
-- size.

ptotal :: Integral n
  => Natural           -- ^ Total items
  -> Natural           -- ^ Page size
  -> n
ptotal total size = fromIntegral $
  let (whole, rems) = total `quotRem` size
  in max 1 (whole + if rems == 0 then 0 else 1)
