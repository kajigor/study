module Test.Chapter3.HeapProperty where 

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Chapter3.Heap 

fromList :: Ord a => (a -> LeftBiasedHeap a -> LeftBiasedHeap a) -> [a] -> LeftBiasedHeap a
fromList f xs = foldl (\h x -> f x h) empty xs 

testInsert insert xs = do 
  let heap = fromList insert xs 
  assert (isHeap heap)
  assert (isLeftist heap) 

hprop_insert :: Property
hprop_insert =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    testInsert insert xs 

hprop_insert' :: Property
hprop_insert' =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    testInsert insert' xs 
