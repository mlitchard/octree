{- |
   Module     : PropTests.OctreeTests.Exposed
   Copyright  : Copyright (c) 2016 Michal J. Gajda
   License    : see LICENSE file
 
   Maintainer : Michael Litchard, Michal J. Gajda
   Stability  : experimental
   Portability: not portable
                            
   This module provides the property tests for exposed octree functions.
-}


{-# LANGUAGE ScopedTypeVariables #-}
module PropTests.OctreeTests.Exposed
  ( prop_lookup
  , prop_fromToList
  , prop_insertionPreserved
  , prop_nearest
  , prop_naiveWithinRange
  , prop_fmap1
  , prop_fmap2
  , prop_fmap3
  , prop_depth_empty
  , prop_depth_upper_bound
  , prop_size 
  ) where

import Prelude hiding(lookup)
import Data.List(sort, sortBy)

import Data.Vector.Class
import Control.Arrow(second)

import Data.Octree.Internal
import Data.Octree() -- test that interface module is not broken
import PropTests.Common

prop_lookup :: [(Vector3, Int)] -> Bool
prop_lookup l = all isIn l
  where ot = fromList l
        isIn x = lookup ot (fst x) == Just x

prop_fromToList :: [(Vector3, Int)] -> Bool
prop_fromToList l = sort l == (sort . toList . fromList $ l)

prop_insertionPreserved :: [(Vector3, Int)] -> Bool
prop_insertionPreserved l = 
  sort l == (sort . toList . foldr insert (Leaf []) $ l)

prop_nearest :: [(Vector3, Int)] -> Vector3 -> Bool
prop_nearest l pt = 
  nearest (fromList l) pt == naiveNearest pt l


prop_naiveWithinRange r l pt = naiveWithinRange r pt l == (sort . map fst . (\o -> withinRange o r pt) . fromList . tuplify pt $ l)

tuplify pt = map (\a -> (a, dist pt a))

compareDistance pt (a,_) (b,_) = compare (dist pt a) (dist pt b)

naiveNearest pt [] = Nothing
naiveNearest pt l  = Just $ head byDist
  where byDist = sortBy (compareDistance pt) l

naiveWithinRange r pt = sort . filter withinRange
  where
    withinRange p = dist pt p <= r

-- unfortunately there is no Arbitrary for (a -> b)
-- since generic properties are quite common, I wonder how to force Quickcheck to default something reasonable?
prop_fmap1,prop_fmap2 :: [(Vector3, Int)] -> Bool
prop_fmap1 = genericProperty_fmap (+1)
prop_fmap2 = genericProperty_fmap (*2)
prop_fmap3 = genericProperty_fmap (show :: Int -> String)

genericProperty_fmap f l = (sort . map (Control.Arrow.second f) $ l) == (sort . toList . fmap f . fromList $ l)

prop_depth_empty = depth (Leaf []) == 0

prop_depth_upper_bound :: [(Vector3, Int)] -> Bool
prop_depth_upper_bound l = 
  depth ot <= max 0 (ceiling . logBase 2 . realToFrac $ size) -- worst splitting ratio possible when we take midpoint (and inputs are colinear)
  where ot   = fromList l
        size = length l

prop_size :: [(Vector3, Int)] -> Bool
prop_size l = size (fromList l) == length l
