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
    explicateMBB',
    explicateMBB,
) where

import Data.List
import Data.Traversable
import Data.Octree.Internal
import Data.Vector.V3 
import Data.Vector.Class

-- | Minimal Bounding Box (Cuboid)
data MBB = MBB { get_min :: ! Vector3, get_max :: ! Vector3 } deriving (Eq,Show)

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
  | maxx'' <= minx'' && 
    maxy'' <= miny'' && 
    maxz'' <= minz''    = Just $ MBB minVect maxVect
  | otherwise           = Nothing
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

-- checks to see if a cube's center point is contained within an MBB    
centerInMBB :: MBB -> Vector3 ->  Bool
centerInMBB (MBB minimums maximums) vect1 =
  (minx <= x && miny <= y && minz <= z) &&
  (maxx >= x && maxy >= y && maxz >= z)
  where
    x = v3z vect1
    y = v3y vect1
    z = v3x vect1
    minx  = v3x minimums
    miny  = v3y minimums
    minz  = v3z minimums
    maxx  = v3x maximums
    maxy  = v3y maximums
    maxz  = v3z maximums

-- | Checks to see if two points are contained inside the same MBB
boxed :: MBB -> Vector3 -> Vector3 -> Bool
boxed mbb vect1 vect2 =
  centerInMBB mbb vect1 && centerInMBB mbb vect2

-- | Makes explicit the implicit bounding boxes of each Node
-- Initial input is root bounding box in a singleton list
explicateMBB :: MBB -> Octree a -> [MBB]
explicateMBB rbb (Leaf _) = [rbb]
explicateMBB rbb (Node { split = split',
                           nwu   = nwu',
                           nwd   = nwd',
                           neu   = neu',
                           ned   = ned',
                           swu   = swu',
                           swd   = swd',
                           seu   = seu',
                           sed   = sed'
                    }) =
--  let newMBB = (MBB (split') (get_max rbb))
  foldl' explicateMBB' [rbb] octList 
  where 
    octList = 
      [(NWU,nwu')
      ,(NWD,nwd')
      ,(NEU,neu')
      ,(NED,ned')
      ,(SWU,swu')
      ,(SWD,swd')
      ,(SEU,seu')
      ,(SED,sed')]

explicateMBB' :: [MBB] -> (ODir, Octree a) -> [MBB]
explicateMBB' mList@((MBB min' max'):_) (_,(Leaf _)) = terminalBoxes:mbb 
explicateMBB' mList@((MBB min' max'):_)
              (odir,(Node { split = split',
                            nwu   = nwu',
                            nwd   = nwd',
                            neu   = neu',
                            ned   = ned',
                            swu   = swu',
                            swd   = swd',
                            seu   = seu',
                            sed   = sed'
                    })) =
  let newMBB = case odir of
                 NWU -> (MBB nwuMin nwuMax) 
                 NWD -> (MBB nwdMin nwdMax)
                 NEU -> (MBB neuMin neuMax)
                 NED -> (MBB nedMin nedMax)
                 SWU -> (MBB swuMin swuMax)
                 SWD -> (MBB swdMin swdMax)
                 SEU -> (MBB seuMin seuMax)
                 SED -> (MBB sedMin sedMax)
      newML = (newMBB:mList)
  in foldl' explicateMBB' newML octList
  where
    nwuMin = (Vector3 (v3x min') (v3y split') (v3z split')) 
    nwuMax = (Vector3 (v3x split') (v3y max') (v3z max')) 
    nwdMin = (Vector3 (v3x min') (v3y split') (v3z min')) 
    nwdMax = (Vector3 (v3x split') (v3y max') (v3z split')) 
    neuMin = (Vector3 (v3x split') (v3y split') (v3z split'))
    neuMax = (Vector3 (v3x max') (v3y max') (v3z max'))
    nedMin = (Vector3 (v3x min') (v3y split') (v3z min')) 
    nedMax = (Vector3 (v3x max') (v3y max') (v3z split'))
    swuMin = (Vector3 (v3x min') (v3y min') (v3z split'))
    swuMax = (Vector3 (v3x split') (v3y split') (v3z max'))
    swdMin = (Vector3 (v3x min') (v3y min') (v3z min'))
    swdMax = (Vector3 (v3x split') (v3y split') (v3z split'))
    seuMin = (Vector3 (v3x split') (v3y min') (v3z split'))
    seuMax = (Vector3 (v3x max') (v3y split') (v3z max'))
    sedMin = (Vector3 (v3x split') (v3y min') (v3z min'))
    sedMax = (Vector3 (v3x max') (v3y split') (v3z split'))
    octList = 
      [(NWU,nwu')
      ,(NWD,nwd')
      ,(NEU,neu')
      ,(NED,ned')
      ,(SWU,swu')
      ,(SWD,swd')
      ,(SEU,seu')
      ,(SED,sed')]
    
    

   
                     
    
