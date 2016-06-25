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
import Data.BoundingBox.B3 
import qualified Data.BoundingBox.Range as R
import Data.Traversable
import Data.Vector.V3
import Data.Vector.Class

import Data.Octree.Internal

data BBoxConfig x y a = BBoxConfig {
-- | A function to recurse down the Octree
  select  :: (BBox3 -> x -> Maybe x),
-- | A function to pre-condition the leaves
  leaf    :: (BBox3 -> x -> (Vector3, a) -> y),
-- | A function to recurse back up the tree
  combine :: (x -> [y] -> y) 

}

type DefInput       =  Vector3
type LeafValue a    = (Vector3, a)
type DefOutput      = (BBox3, Vector3)
type DefNodeValue a = (a -> a)

defBBoxConfig :: BBoxConfig DefInput DefOutput (DefNodeValue a)
defBBoxConfig = BBoxConfig {
  select  = filterNodes ,
  leaf    = points,
  combine = result
}

filterNodes :: BBox3 -> DefInput -> Maybe (DefInput)
filterNodes bbox x =
  case (within_bounds x bbox) of
    True  -> Just x
    False -> Nothing 

points :: BBox3 -> DefInput -> LeafValue (a -> a) -> DefOutput
points box _ (point,_) = (box,point)
   

result :: DefInput -> [DefOutput] -> DefOutput
result _ out =
  let dedup = nub out
   
-- | Supplied boolean test for result function
-- | Returns True if Box a is contained within Box b
inclusive :: DefOutput -> DefOutput -> Bool
inclusive (b1,_) (b2,_) =
  case (isect b1 b2) of
    Just b3 -> b1 == b3
    Nothing -> False
      
traverseOctreeBB :: BBoxConfig x y a -> BBox3 -> Octree a -> x -> y
traverseOctreeBB bbc bbx (Leaf objects) input = undefined
--  map (leaf' bbx input objects 
  where
    leaf' = leaf bbc   
traverseOctreeBB 
  bbc bbx (Node split' nwu' nwd' neu' ned' swu' swd' seu' sed') x = undefined
--  let res = map 
--    case (select' ) of
       
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


      
  
