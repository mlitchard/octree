octree/Data/Octree/BoundingBox.hs
==============================
BoundingBox.hs provides a generalized traversal function, that traverses
an `Octree a` based on a BBox3 value.

Here is an example that produces an `Octree Int` with 2 subdivisions,
and finds the bounding box of a particular `(Vector3,Int)`.

~~~ {.haskell}
module Main where

import Data.List
import Data.Maybe
import Data.Vector.Class
import System.Random
import System.Random.Shuffle
import Data.BoundingBox.B3

import Data.Octree.Internal hiding (lookup)
import Data.Octree.BoundingBox.BoundingBox
import Data.Octree.BoundingBox.Internal

main = do
  octree' <- octree 513
  let (testInput,_) = (!!) (toList octree') 256
      (bbx,_)      = traverseOctreeBB defBBoxConfig bbis octree' testInput
  putStrLn ("Bounding box of " ++ (show testInput) ++ " is " ++ (show bbx))

bbis :: BBox3
bbis =
  let infinity = (read "Infinity") :: Double
      swdCorner = Vector3 (-infinity) (-infinity) (-infinity)
      neuCorner = Vector3 (infinity) (infinity) (infinity)
  in bound_corners swdCorner neuCorner

octree :: Integer -> IO (Octree DefNodeValue)
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
~~~
