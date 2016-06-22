{- |
  Module     : Data.Octree.MBB
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : MIT

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
 
  This module provides a minimal bounding box.
 
-}

module Data.Octree.BoundingBox

(
    isBBox,
    inBBox,
    boxed,
    explicateBBox,
) where

import Data.List
import Data.BoundingBox.B3 hiding (within_bounds,min_point)
import Data.BoundingBox.Range hiding (bound_corners) 
import Data.Traversable
import Data.Octree.Internal
import Data.Vector.V3 
import Data.Vector.Class

data OutOfBounds = High | Low deriving Show
                   
-- | the property, that a 'MBB' must hold
isBBox :: BBox3 -> Bool
isBBox (BBox3 minX minY minZ maxX maxY maxZ) = 
  (minX < maxX) && (minY < maxY) && (minZ < maxZ)
--      
inBBox :: BBox3 -> Vector3 ->  Bool
inBBox (BBox3 minX minY minZ maxX maxY maxZ) vect1 =
  (minX <= x && minY <= y && minZ <= z) &&
  (maxX >= x && maxY >= y && maxZ >= z)
  where
    x = v3z vect1
    y = v3y vect1
    z = v3x vect1

-- | Checks to see if two points are contained inside the same MBB
boxed :: BBox3 -> Vector3 -> Vector3 -> Bool
boxed bbox3 vect1 vect2 =
  inBBox bbox3 vect1 && inBBox bbox3 vect2

-- | Makes explicit the implicit bounding boxes of each Node
-- Initial input is root bounding box, Octree pair.
explicateBBox :: (BBox3, Octree a) -> [BBox3]
explicateBBox (mbb, (Leaf _)) = [mbb]
explicateBBox (mbb, (Node { split = split',
                             nwu   = nwu',
                             nwd   = nwd',
                             neu   = neu',
                             ned   = ned',
                             swu   = swu',
                             swd   = swd',
                             seu   = seu',
                             sed   = sed'
                 })) =
   mbb:concatMap explicateBBox octList 
  where 
    octList = zip boxList children
    boxList = [swdBox, sedBox, nwdBox, nedBox, swuBox, seuBox, nwuBox, neuBox]
    children = [swd',sed',nwd',ned',swu',seu',nwu',neu']
    swdBox = bound_corners swdCorner neuCorner 
      where
        swdCorner = Vector3 (minX mbb) (minY mbb) (minZ mbb)
        neuCorner = Vector3 (v3x split') (v3y split') (v3z split')
    sedBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (v3x split') (minY mbb) (minZ mbb)
        neuCorner = Vector3 (maxX mbb) (v3y split') (v3z split')
    nwdBox = bound_corners swdCorner neuCorner 
      where
        swdCorner = Vector3 (minX mbb) (v3y split') (minZ mbb)
        neuCorner = Vector3 (v3x split') (maxY mbb) (v3z split')
    nedBox = bound_corners swdCorner neuCorner 
      where
        swdCorner = Vector3 (v3x split') (v3y split') (minZ mbb)    
        neuCorner = Vector3 (maxX mbb) (maxY mbb) (v3z split')
    swuBox = bound_corners swdCorner neuCorner 
      where
        swdCorner = Vector3 (minX mbb) (minY mbb) (v3z split')
        neuCorner = Vector3 (v3x split') (v3y split') (maxZ mbb)
    seuBox = bound_corners swdCorner neuCorner 
      where
        swdCorner = Vector3 (v3x split') (minY mbb) (v3z split')
        neuCorner = Vector3 (maxX mbb) (v3y split') (maxZ mbb)
    nwuBox = bound_corners swdCorner neuCorner 
      where
        swdCorner = Vector3 (minX mbb) (v3y split') (v3z split')
        neuCorner = Vector3 (v3x split') (maxY mbb) (maxZ mbb)
    neuBox = bound_corners swdCorner neuCorner
      where
        swdCorner = Vector3 (v3x split') (v3y split') (v3z split')
        neuCorner = Vector3 (maxX mbb) (maxY mbb) (maxZ mbb)

boundedPoints :: (BBox3, Octree a) -> [Vector3]
boundedPoints (_, (Leaf vects)) = map fst vects
boundedPoints (mbb, (Node { split = split',
                             nwu   = nwu',
                             nwd   = nwd',
                             neu   = neu',
                             ned   = ned',
                             swu   = swu',
                             swd   = swd',
                             seu   = seu',
                             sed   = sed'
                 })) = 
  let x_west  = [NWD,NWU,SWD,SWU]
      x_east  = [NED,NEU,SED,SEU]
      y_north = [NWU,NWD,NEU,NED]
      y_south = [SWU,SWD,SEU,SED]
      z_up    = [NWU,SWU,NEU,SEU]
      z_down  = [NWD,SWD,NED,SED]
      tagged_nodes = zip allOctants children
      children     = [swd',sed',nwd',ned',swu',seu',nwu',neu']
      sDir  = (xfilter . yfilter . zfilter) allOctants
      nodes = map (toNode tagged_nodes) sDir
  in concatMap boundedPoints $ map ((,) mbb) nodes
  where
    xfilter, yfilter, zfilter :: [ODir] -> [ODir]
    xfilter = undefined
    yfilter = undefined
    zfilter = undefined
    toNode :: [(ODir,Octree a)] -> ODir -> [Octree a]
    toNode = undefined
    rangeRelationX, rangeRelationY, rangeRelationZ :: Maybe OutOfBounds
    rangeRelationX = 
      let rx         = rangeX mbb
          splitx     = v3x split'
          min_point' = min_point rx
      in case (within_bounds splitx rx) of
           True  -> Nothing
           False -> 
             case (splitx < min_point') of
               True ->  Just Low
               False -> Just High
    rangeRelationY =
      let ry         = rangeY mbb
          splity     = v3y split'
          min_point' = min_point ry
      in case (within_bounds splity ry) of
           True  -> Nothing
           False -> 
             case (splity < min_point') of
               True  -> Just Low
               False -> Just High
    rangeRelationZ =
      let rz = rangeZ mbb 
          splitz = v3z split'
          min_point' = min_point rz
      in case (within_bounds splitz rz) of
           True  -> Nothing
           False -> 
             case (splitz < min_point') of
               True  -> Just Low
               False -> Just High
       

      

    taggedChildren = zip allOctants [swd,sed,nwd,ned,swu,seu,nwu,neu]




