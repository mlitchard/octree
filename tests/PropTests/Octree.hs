{-# LANGUAGE ScopedTypeVariables #-}
module PropTests.Octree 
  ( propOctInternal
  , propOctExposed)
  where

import Data.Octree.Internal
import Data.Octree() -- test that interface module is not broken
import Prelude hiding(lookup)
import Data.List(sort, sortBy)

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck.Arbitrary

import Data.Vector.Class
import Control.Arrow(second)

import PropTests.OctreeTests.Internal
import PropTests.OctreeTests.Exposed

propOctInternal :: Spec
propOctInternal = do
  describe "Tests for internal helper functions" $ do
    prop "depth" $ prop_depth 
    prop "prop_cmp1" $ property $ prop_cmp1
    prop "prop_cmp2" $ property $ prop_cmp2
    prop "prop_stepDescription" $ property $ prop_stepDescription
    prop "prop_octantDistanceNoGreaterThanInterpointDistance0" $
       property $ prop_octantDistanceNoGreaterThanInterpointDistance0
    prop "prop_octantDistanceNoGreaterThanInterpointDistance" $
       property $ prop_octantDistanceNoGreaterThanInterpointDistance
    prop "prop_octantDistanceNoGreaterThanInterpointDistanceZero" $
       property $ prop_octantDistanceNoGreaterThanInterpointDistanceZero
    prop "prop_octantDistanceNoGreaterThanCentroidDistance" $
       property $ prop_octantDistanceNoGreaterThanCentroidDistance
    prop "prop_pickClosest" $
       property (prop_pickClosest :: [(Vector3, Int)] -> Vector3 -> Bool)

propOctExposed :: Spec
propOctExposed = do
  describe "Tests for exposed functions" $ do
    prop "prop_lookup" $ prop_lookup 
    prop "prop_fromToList" $ prop_fromToList 
    prop "prop_insertionPreserved" $ prop_insertionPreserved 
    prop "prop_nearest" $ prop_nearest 
    prop "prop_naiveWithinRange" $ prop_naiveWithinRange 
    prop "prop_fmap1" $ prop_fmap1
    prop "prop_fmap2" $ prop_fmap2
    prop "prop_fmap3" $ prop_fmap3
    prop "prop_depth_empty" $ prop_depth_empty
    prop "prop_depth_upper_bound" $ prop_depth_upper_bound
    prop "prop_size" $ prop_size 
