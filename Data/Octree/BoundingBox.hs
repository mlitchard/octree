{- |
  Module     : Data.Octree.BoundingBox.BoundingBox
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : MIT

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
              
  This module provides a way to use bounding boxes with Octrees.
                  
-}

module Data.Octree.BoundingBox
  ( BBoxConfig (..)
  , traverseOctreeBB
  ) where

import Data.List
import Safe
import Data.Maybe
import Data.BoundingBox.B3 
import qualified Data.BoundingBox.Range as R
import Data.Traversable
import Data.Vector.V3
import Data.Vector.Class

import Data.Octree.Internal

data BBoxConfig x y a = BBoxConfig {
-- | A function to recurse down the Octree
  select   :: (BBox3 -> x -> Maybe x),
-- | A function to pre-condition the leaves
  procLeaf :: (BBox3 -> x -> [LeafValue a] -> y),
-- | A function to recurse back up the tree
  combine  :: (x -> [y] -> y)
}

type DefInput       =  Vector3
type LeafValue a    = (Vector3, a)
type DefOutput a     = (BBox3, [LeafValue (DefNodeValue a)])
type DefNodeValue a = (a -> a)

defBBoxConfig :: BBoxConfig DefInput (DefOutput a) (DefNodeValue a)
defBBoxConfig = BBoxConfig {
  select   = filterNodes ,
  procLeaf = points,
  combine  = result
}

filterNodes :: BBox3 -> DefInput -> Maybe (DefInput)
filterNodes bbox x =
  case (within_bounds x bbox) of
    True  -> Just x
    False -> Nothing 

points :: BBox3 -> DefInput -> [LeafValue (DefNodeValue a)] -> DefOutput a
points box x leaf = (box, leaf)
   
-- | result reduces the list of all BBoxes containing the point to
-- the terminal BBox
result :: x -> [DefOutput a] -> DefOutput a
result _ (x:xs) =
  foldl' findTerminal x xs
  where
    findTerminal :: DefOutput a -> DefOutput a -> DefOutput a
    findTerminal bbox1@(bbox1',_) bbox2@(bbox2',_)
      | (inclusive bbox1' bbox2') == True = bbox1
      | otherwise                         = bbox2
      
   
-- | Supplied boolean test for result function
-- | Returns True if Box b1 is contained within Box b2
inclusive :: BBox3 -> BBox3 -> Bool
inclusive b1 b2 =
  case (isect b1 b2) of
    Just b3 -> b1 == b3
    Nothing -> False
      
traverseOctreeBB :: BBoxConfig x y a -> BBox3 -> Octree a -> x -> y
traverseOctreeBB bbc bbx (Leaf leaf_vals) input = 
   procLeaf' bbx input leaf_vals
  where
    procLeaf' = procLeaf bbc   

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
    boxList = [swdBox, sedBox, nwdBox, nedBox, swuBox, seuBox, nwuBox, neuBox]
    children = [swd',sed',nwd',ned',swu',seu',nwu',neu']
    swdBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (minX bbx) (minY bbx) (minZ bbx)
        neuCorner = Vector3 (v3x split') (v3y split') (v3z split')
    sedBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (v3x split') (minY bbx) (minZ bbx)
        neuCorner = Vector3 (maxX bbx) (v3y split') (v3z split')
    nwdBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (minX bbx) (v3y split') (minZ bbx)
        neuCorner = Vector3 (v3x split') (maxY bbx) (v3z split')
    nedBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (v3x split') (v3y split') (minZ bbx)
        neuCorner = Vector3 (maxX bbx) (maxY bbx) (v3z split')
    swuBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (minX bbx) (minY bbx) (v3z split')
        neuCorner = Vector3 (v3x split') (v3y split') (maxZ bbx)
    seuBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (v3x split') (minY bbx) (v3z split')
        neuCorner = Vector3 (maxX bbx) (v3y split') (maxZ bbx)
    nwuBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (minX bbx) (v3y split') (v3z split')
        neuCorner = Vector3 (v3x split') (maxY bbx) (maxZ bbx)
    neuBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (v3x split') (v3y split') (v3z split')
        neuCorner = Vector3 (maxX bbx) (maxY bbx) (maxZ bbx)
