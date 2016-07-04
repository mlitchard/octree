-- |
-- Module: tests/Main.hs
-- Copyright:   2016 Michael Litchard
-- License: BSD3
-- Maintainer: <Michael Litchard> <michael@schmong.org>
-- Top-level test driver

import Test.Hspec (hspec)

import PropTests.Octree
import PropTests.BoundingBoxTests.BoundingBoxTests

main :: IO ()
main = do
  hspec propOctExposed
  hspec propValidateBoxes 
  hspec propMatchValuesToBoxes
  hspec propOctInternal
