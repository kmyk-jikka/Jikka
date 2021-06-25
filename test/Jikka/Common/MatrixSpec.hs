module Jikka.Common.MatrixSpec
  ( spec,
  )
where

import qualified Data.Vector as V
import Jikka.Common.Matrix
import Test.Hspec

makeMatrix'' :: [[Integer]] -> Matrix Integer
makeMatrix'' = makeMatrix' . V.fromList . map V.fromList

spec :: Spec
spec = do
  describe "matcheck" $ do
    it "works" $ do
      let f = V.fromList $ map V.fromList [[1, 2, 3], [3, 4, 5]]
      let expected = True
      matcheck f `shouldBe` expected
    it "works'" $ do
      let f = V.fromList $ map V.fromList [[1, 2, 3], [3, 4]]
      let expected = False
      matcheck f `shouldBe` expected
  describe "matap" $ do
    it "works" $ do
      let f = makeMatrix'' [[1, 2, 3], [3, 4, 5]]
      let x = V.fromList [1, 2, 3]
      let y = V.fromList [14, 26]
      matap f x `shouldBe` y

  describe "matadd" $ do
    it "works" $ do
      let f = makeMatrix'' [[1, 2, 3], [3, 4, 5]]
      let g = makeMatrix'' [[7, 7, 7], [6, 5, 4]]
      let h = makeMatrix'' [[8, 9, 10], [9, 9, 9]]
      matadd f g `shouldBe` h

  describe "matmul" $ do
    it "works" $ do
      let f = makeMatrix'' [[1, 2, 3], [3, 4, 5]]
      let g = makeMatrix'' [[1, 2], [3, 4], [5, 6]]
      let h = makeMatrix'' [[22, 28], [40, 52]]
      matmul f g `shouldBe` h

  describe "matscalar" $ do
    it "works" $ do
      let k = 3
      let f = makeMatrix'' [[1, 2, 3], [3, 4, 5]]
      let g = makeMatrix'' [[3, 6, 9], [9, 12, 15]]
      matscalar k f `shouldBe` g

  describe "matpow" $ do
    it "works" $ do
      let f = makeMatrix'' [[1, 1], [1, 0]]
      let k = 10
      let g = makeMatrix'' [[89, 55], [55, 34]]
      matpow f k `shouldBe` g
