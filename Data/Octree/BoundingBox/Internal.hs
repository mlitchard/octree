{- |
  Module     : Data.Octree.BoundingBox.Internal
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : MIT

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
  
  This module provides functions etc, for Data.Octree.BoundingBox.BoundingBox

  -- | DefInput, LeafValue a, DefOutput a, DefNodeValue a 
         Default types for BBoxConfig

  -- | filterNodes, points, result
         Default functions for BBoxConfig

  -- | inclusive
         boolean check for one BBox3 being inclusive of another
-}

module Data.Octree.BoundingBox.Internal
  ( filterNodes
  , points
  , result
  , DefInput
  , LeafValue 
  , DefOutput 
  , DefNodeValue
  , newBBox3 
  ) where

import Data.Vector.V3
import Data.BoundingBox.Range hiding (within_bounds,isect,bound_corners)
import Data.BoundingBox.B3 

import Data.Octree.Internal
import Data.List

type DefInput       =  Vector3
type Split          = Vector3
type LeafValue a    = (Vector3, a)
type DefOutput      = (BBox3, [LeafValue DefNodeValue ])
type DefNodeValue   = Int

-- |  newBBox3 creates a smaller BBox3
--    Given Node name, previous BBox3, and the split
newBBox3 :: BBox3 -> Vector3 -> ODir -> BBox3
newBBox3 bbx split' SWD =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (minX bbx) (minY bbx) (minZ bbx)
      neuCorner = Vector3 (v3x split') (v3y split') (v3z split')

newBBox3 bbx split' SED =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (v3x split') (minY bbx) (minZ bbx)
      neuCorner = Vector3 (maxX bbx) (v3y split') (v3z split')

newBBox3 bbx split' NWD =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (minX bbx) (v3y split') (minZ bbx)
      neuCorner = Vector3 (v3x split') (maxY bbx) (v3z split')

newBBox3 bbx split' NED =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (v3x split') (v3y split') (minZ bbx)
      neuCorner = Vector3 (maxX bbx) (maxY bbx) (v3z split')

newBBox3 bbx split' SWU =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (minX bbx) (minY bbx) (v3z split')
      neuCorner = Vector3 (v3x split') (v3y split') (maxZ bbx)

newBBox3 bbx split' SEU =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (v3x split') (minY bbx) (v3z split')
      neuCorner = Vector3 (maxX bbx) (v3y split') (maxZ bbx)

newBBox3 bbx split' NWU =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (minX bbx) (v3y split') (v3z split')
      neuCorner = Vector3 (v3x split') (maxY bbx) (maxZ bbx)

newBBox3 bbx split' NEU =
  bound_corners swdCorner neuCorner
    where
      swdCorner = Vector3 (v3x split') (v3y split') (v3z split')
      neuCorner = Vector3 (maxX bbx) (maxY bbx) (maxZ bbx)

-- | filterNodes is default function for BBoxConfig 
--   used to recurse down octree identifying which BBox3s contain DefInput
filterNodes :: BBox3 -> DefInput -> Maybe DefInput
filterNodes bbox x =
  case (within_bounds x bbox) of
    True  -> Just x
    False -> Nothing

-- | points is default function for BBoxConfig
-- pre-processes Leaf
points :: BBox3 -> DefInput -> [LeafValue DefNodeValue ] -> DefOutput
points box _ leaf = (box, leaf)

-- | result reduces the list of all BBoxes containing the point to
--   the terminal BBox
result :: DefInput -> [DefOutput ] -> DefOutput 
result _ (x:xs) =
  foldl' findTerminal x xs
    where
      findTerminal :: DefOutput -> DefOutput -> DefOutput
      findTerminal bbox1@(bbox1',_) bbox2@(bbox2',_)
        | (inclusive bbox1' bbox2') == True = bbox1
        | otherwise                         = bbox2

-- | Supplied boolean test for result function
--   Returns True if Box b1 is contained within Box b2
inclusive :: BBox3 -> BBox3 -> Bool
inclusive b1 b2 =
  case (isect b1 b2) of
    Just b3 -> b1 == b3
    Nothing -> False
