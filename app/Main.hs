{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Diagrams.Backend.Cairo.CmdLine
import qualified GameOfLife
import qualified Seeds
import qualified BriansBrain

-- Seeds
-- =====

--gridDim = 50
--startGrid :: Univ Seeds.Cell
--startGrid = makeUniv gridDim (\y x -> Seeds.bool2cell (y `mod` 5 < 3 && x `mod` 2 == 0 && (x + y) `mod` 3 < 1))

--main = mainWith $ mkCAGif Seeds.seedsCA startGrid 500


-- Life
-- =======

-- Brians Brain
-- ============

gridDim = 20
startGrid :: Univ BriansBrain.Cell
startGrid = makeUniv gridDim (\y x -> if (y ^ 13 `mod` 1023 <= 5)
                                      then
                                        if (x ^ 17 `mod` 2047 <= 5)
                                            then BriansBrain.On
                                            else BriansBrain.Dying
                                      else
                                        BriansBrain.Off)

main = mainWith $ mkCAGif BriansBrain.briansBrainCA startGrid 100
