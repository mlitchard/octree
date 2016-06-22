{- |
  Module     : Data.Octree.BoundingBox.Internal
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : MIT

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
  
  This module provides functions for Data.Octree.BoundingBox.BoundingBox

-}

module Data.Octree.BoundingBox.Internal
  (
    xfilter,
    yfilter,
    zfilter
  ) where

import Data.Vector.V3
import Data.BoundingBox.Range hiding (bound_corners)
import Data.BoundingBox.B3 hiding (within_bounds,min_point)

import Data.Octree.Internal

data OutOfBounds = High | Low deriving Show

xfilter, yfilter, zfilter :: BBox3 -> Vector3 -> [ODir] -> [ODir]

xfilter mbb split octants =
  case (rangeRelationX mbb split) of
    Nothing   -> octants
    Just High -> filter (flip notElem x_east) octants
    Just Low  -> filter (flip notElem x_west) octants
  where
    x_west  = [NWD,NWU,SWD,SWU]
    x_east  = [NED,NEU,SED,SEU]

yfilter mbb split octants =
  case (rangeRelationY mbb split) of
    Nothing   -> octants
    Just High -> filter (flip notElem y_south) octants
    Just Low  -> filter (flip notElem y_north) octants
  where
    y_north = [NWU,NWD,NEU,NED]
    y_south = [SWU,SWD,SEU,SED]
 
zfilter mbb split octants =
  case (rangeRelationZ mbb split) of
    Nothing   -> octants
    Just High -> filter (flip notElem z_down) octants
    Just Low  -> filter (flip notElem z_up) octants
  where
    z_up    = [NWU,SWU,NEU,SEU]
    z_down  = [NWD,SWD,NED,SED]
 
rangeRelationX, rangeRelationY, rangeRelationZ :: BBox3   -> 
                                                  Vector3 -> 
                                                  Maybe OutOfBounds
   
rangeRelationX mbb split =
  let rx     = rangeX mbb
      splitx = v3x split
      min_point' = min_point rx
  in case (within_bounds splitx rx) of
       True  -> Nothing
       False ->
         case (min_point' < splitx) of
           True ->  Just Low
           False -> Just High   

rangeRelationY mbb split =
  let ry         = rangeY mbb
      splity     = v3y split
      min_point' = min_point ry
  in case (within_bounds splity ry) of
       True  -> Nothing
       False ->
         case (min_point' < splity) of
           True  -> Just Low
           False -> Just High 

rangeRelationZ mbb split =
  let rz = rangeZ mbb
      splitz = v3z split
      min_point' = min_point rz
  in case (within_bounds splitz rz) of
       True  -> Nothing
       False ->
         case (min_point' < splitz) of
           True  -> Just Low
           False -> Just High 
