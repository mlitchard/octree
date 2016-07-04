{- |
  Module     : Data.Octree.BoundingBox.BoundingBox
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : BSD3

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
              
  This module provides a way to use bounding boxes with Octrees.
                  
-}

module Data.Octree.BoundingBox.BoundingBox
  ( BBoxConfig (..)
  , traverseOctreeBB
  , defBBoxConfig
  ) where

import Data.Maybe (mapMaybe)
import Data.BoundingBox.B3 (BBox3)

import Data.Octree.Internal (Octree (..), allOctants)
import Data.Octree.BoundingBox.Internal

-- | BBoxConfig - The functions traverseOctreeBB needs
data BBoxConfig x y a = BBoxConfig {
-- | A function to recurse down the Octree
  select   :: (BBox3 -> x -> Maybe x),
-- | A function to pre-condition the leaves
  procLeaf :: (BBox3 -> x -> [LeafValue a] -> y),
-- | A function to recurse back up the tree
  combine  :: (x -> [y] -> y)
}

-- | defBBoxConfig - default BBoxConfig
defBBoxConfig :: BBoxConfig DefInput DefOutput DefNodeValue
defBBoxConfig = BBoxConfig {
  select   = filterNodes ,
  procLeaf = points,
  combine  = result
}

-- | traverseOctreeBB - Generalized Octree traversal function based on BBox3
-- Base case processes leaves 
traverseOctreeBB :: BBoxConfig x y a -> BBox3 -> Octree a -> x -> y
traverseOctreeBB bbc bbx (Leaf leaf_vals) input = 
   procLeaf' bbx input leaf_vals
  where
    procLeaf' = procLeaf bbc   

-- | General framework to traverse an Octree in terms of a BBox3
traverseOctreeBB 
  bbc bbx (Node split' nwu' nwd' neu' ned' swu' swd' seu' sed') x = 
  let res = mapMaybe traverseOctreeBB' octList
  in combine' x res            
  where
    combine' = combine bbc 
    select'  = select bbc

    traverseOctreeBB' (subbox, subtree) =
      case (select' subbox x) of
        Just x' -> Just (traverseOctreeBB bbc subbox subtree x)
        Nothing -> Nothing 

    octList = zip boxList children
    boxList = map (newBBox3 bbx split') allOctants 
    children = [swd',sed',nwd',ned',swu',seu',nwu',neu']
