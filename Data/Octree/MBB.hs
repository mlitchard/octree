{- |
  Module     : Data.Octree.MBB
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : MIT

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
 
  This module provides a minimal bounding box.
 
-}

module Data.Octree.MBB

(
    MBB (..),
    mbb,
    volume,
    containsMBB,
    unionMBB,
    unionsMBB,
    intersectMBB,
    isValidMBB,
) where

import Data.Octree.Internal
import Data.Vector.V3 
import Data.Vector.Class

-- | Minimal Bounding Box (Cuboid)
data MBB = MBB { get_min :: ! Vector3, get_max :: ! Vector3 } deriving Eq

-- | created a minimal bounding box (or a rectangular cubeoid)
-- get_min must be smaller, than get_max. This is unchecked.
mbb :: Vector3 -- ^ minimum values
    -> Vector3 -- ^ maximum values
    -> MBB  
mbb = MBB

-- | the property, that a 'MBB' must hold
isValidMBB :: MBB -> Bool
isValidMBB (MBB minimums maximums) = 
  (minx < maxx) && (miny < maxy) && (minz < maxz)
  where
    minx = v3x minimums
    miny = v3y minimums
    minz = v3z minimums
    maxx = v3x maximums
    maxy = v3y maximums
    maxz = v3z maximums

-- | internal only.
unionsMBB :: [MBB] -> MBB
unionsMBB [] = error "unionsMBB': []"
unionsMBB xs = foldr1 unionMBB xs

-- | unifies two MBBs into one
unionMBB :: MBB -> MBB -> MBB
unionMBB (MBB minimums maximums) (MBB minimums' maximums') =
  MBB newMinVector newMaxVector
  where
    newMinVector = Vector3 (min minx minx') (min miny miny') (min minz minz') 
    newMaxVector = Vector3 (max maxx maxx') (max maxy maxy') (max maxz maxz')
    minx  = v3x minimums
    miny  = v3y minimums
    minz  = v3z minimums
    maxx  = v3x maximums
    maxy  = v3y maximums
    maxz  = v3z maximums
    minx' = v3x minimums'
    miny' = v3y minimums'
    minz' = v3z minimums'
    maxx' = v3x maximums'
    maxy' = v3y maximums'
    maxz' = v3z maximums'

-- | Calculates Volume
volume :: MBB -> Scalar
volume (MBB minimums maximums) =
  vmag (vpromote (u `vdot` (w `vcross` v) ) :: Vector3)
  where
  u = Vector3 maxx miny minz
  w = Vector3 minx maxy minz
  v = Vector3 minx miny maxz
  minx  = v3x minimums
  miny  = v3y minimums
  minz  = v3z minimums
  maxx  = v3x maximums
  maxy  = v3y maximums
  maxz  = v3z maximums

-- | returns True when the first mbb contains the second mbb
containsMBB :: MBB -> MBB -> Bool
containsMBB (MBB minimums maximums) (MBB minimums' maximums') = 
  (minx <= minx' && miny <= miny' && minz <= minz') &&
  (maxx >= maxx' && maxy >= maxy' && maxz >= maxz')
  where
    minx  = v3x minimums
    miny  = v3y minimums
    minz  = v3z minimums
    maxx  = v3x maximums
    maxy  = v3y maximums
    maxz  = v3z maximums
    minx' = v3x minimums'
    miny' = v3y minimums'
    minz' = v3z minimums'
    maxx' = v3x maximums'
    maxy' = v3y maximums'
    maxz' = v3z maximums'

intersectMBB :: MBB -> MBB -> Maybe MBB
intersectMBB (MBB minimums maximums) (MBB minimums' maximums') 
  | maxx'' <= minx'' && maxy'' <= miny'' && maxz'' <= minz'' = Just $ MBB minVect maxVect
  | otherwise                                                = Nothing
  where
    maxx'' = max maxx maxx'
    maxy'' = max maxy maxy'
    maxz'' = max maxz maxz'
    minx'' = min minx minx'
    miny'' = min miny miny'
    minz'' = min minz minz'
    maxx  = v3x maximums
    maxy  = v3y maximums
    maxz  = v3z maximums
    maxx' = v3x maximums'
    maxy' = v3y maximums'
    maxz' = v3z maximums'
    minx  = v3x minimums
    miny  = v3y minimums
    minz  = v3z minimums
    minx' = v3x minimums'
    miny' = v3y minimums'
    minz' = v3z minimums'
    minVect = Vector3 minx'' miny'' minz''
    maxVect = Vector3 maxx'' maxy'' maxz''
    






