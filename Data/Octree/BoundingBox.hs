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
import Data.BoundingBox.B3 hiding (within_bounds,min_point)
import Data.BoundingBox.Range hiding (bound_corners)
import Data.Traversable
import Data.Vector.V3
import Data.Vector.Class

import Data.Octree.Internal

data BBoxConfig x y a = BBoxConfig {
-- | A function to recurse down the Octree
  select  :: (BBox3 -> x -> x),
-- | A function to pre-condition the leaves
  leaf    :: (BBox3 -> x -> (Vector3, a) -> y),
-- | A function to recurse back up the tree
  combine :: (x -> [y] -> y) 

}
traverseOctreeBB :: BBoxConfig x y a -> BBox3 -> Octree a -> x -> y
traverseOctreeBB bbc bbx (Leaf objects) input =
  map (leaf' bbx input objects 
  where
    leaf' = leaf bbc   
traverseOctreeBB 
  bbc bbx (Node split' nwu' nwd' neu' ned' swu' swd' seu' sed') x = undefined
--  where
--    swdBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (minX bbx) (minY bbx) (minZ bbx)
--        neuCorner = Vector3 (v3x split') (v3y split') (v3z split')
--    sedBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (v3x split') (minY bbx) (minZ bbx)
--        neuCorner = Vector3 (maxX bbx) (v3y split') (v3z split')
--    nwdBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (minX bbx) (v3y split') (minZ bbx)
--        neuCorner = Vector3 (v3x split') (maxY bbx) (v3z split')
--    nedBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (v3x split') (v3y split') (minZ bbx)
--        neuCorner = Vector3 (maxX bbx) (maxY bbx) (v3z split')
--    swuBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (minX bbx) (minY bbx) (v3z split')
--        neuCorner = Vector3 (v3x split') (v3y split') (maxZ bbx)
--    seuBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (v3x split') (minY bbx) (v3z split')
--        neuCorner = Vector3 (maxX bbx) (v3y split') (maxZ bbx)
--    nwuBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (minX bbx) (v3y split') (v3z split')
--        neuCorner = Vector3 (v3x split') (maxY bbx) (maxZ bbx)
--    neuBox = bound_corners swdCorner neuCorner
--      where
--        swdCorner = Vector3 (v3x split') (v3y split') (v3z split')
--        neuCorner = Vector3 (maxX bbx) (maxY bbx) (maxZ bbx)


      
  
