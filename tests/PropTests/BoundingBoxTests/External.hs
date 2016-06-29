{- |
  Module : PropTests.BoundingBoxTests.External
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : MIT

  Maintainer : Michael Litchard, Michal J. Gajda
  Stability  : experimental
  Portability: not portable
  
  This module provides property tests for BoundingBox.hs

-}

module PropTests.BoundingBoxTests.External
  ( bbis
  , isBBox
  , listLeaves
  , octree
  , testConfig1
  ) where


import Data.List
import Data.Maybe 
import Data.Vector.Class 
import System.Random 
import System.Random.Shuffle 
import Data.BoundingBox.B3

import Data.Octree.Internal hiding (lookup)
import Data.Octree.BoundingBox.BoundingBox
import Data.Octree.BoundingBox.Internal
-- | Bounding Box of Infinite Space
--   The root Bounding Box
bbis :: BBox3
bbis =
  let infinity = (read "Infinity") :: Double
      swdCorner = Vector3 (-infinity) (-infinity) (-infinity)
      neuCorner = Vector3 (infinity) (infinity) (infinity)
  in bound_corners swdCorner neuCorner

isBBox :: BBox3 -> Bool
isBBox (BBox3 minX minY minZ maxX maxY maxZ) =
  (minX < maxX) && (minY < maxY) && (minZ < maxZ)

octree :: Integer -> IO (Octree Int)
octree bound = do
  xGen <- newStdGen
  yGen <- newStdGen
  zGen <- newStdGen
  let xPoints :: [Double]
      yPoints :: [Double]
      zPoints :: [Double]
      xPoints = map fromInteger $ shuffle' pointList (length pointList) xGen
      yPoints = map fromInteger $ shuffle' pointList (length pointList) yGen
      zPoints = map fromInteger $ shuffle' pointList (length pointList) zGen
      ub = bound `div` 2 
      lb = - (bound `div` 2) 
      pointList = [lb .. ub] 
      sizePL = length pointList
      vectors = map (\(x,y,z) -> Vector3 x y z) $ zip3 xPoints yPoints zPoints
  return $ fromList $ zip vectors $ [1 .. sizePL]

testConfig1 :: BBoxConfig DefInput [DefOutput] DefNodeValue
testConfig1 = BBoxConfig {
  select   = allPoints,
  procLeaf = allBoxesAndKeys,
  combine  = assemble
}     
allBoxesAndKeys :: BBox3                     -> 
                   DefInput                  -> 
                   [LeafValue DefNodeValue ] -> 
                   [DefOutput]
allBoxesAndKeys bbx x leaf = [(bbx, leaf)]

allPoints :: BBox3 -> DefInput -> Maybe (DefInput)
allPoints bbx x = Just x

assemble :: DefInput -> [[DefOutput]] -> [DefOutput]
assemble _ x = concat x

type AltInput = BBox3


filterBoxes :: BBox3 -> AltInput -> Maybe AltInput
filterBoxes genBox origBox =
  case (within_bounds origBox genBox) of
    True  -> Just origBox
    False -> Nothing

checkbox :: BBox3 -> AltInput -> [LeafValue DefNodeValue ] -> DefOutput
checkbox genBox _ leaf = (genBox, leaf)

boxResult :: DefInput -> [DefOutput] -> DefOutput
boxResult origBox allOuts =
  fromMaybe (bbis,[] :: [(Vector3,a)]) $ lookup origBox allOuts         
