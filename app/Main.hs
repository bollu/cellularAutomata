{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Diagrams.Backend.Cairo.CmdLine
import qualified GameOfLife
import qualified Seeds

-- Seeds
-- =====

--gridDim = 50
--startGrid :: Univ Seeds.Cell
--startGrid = makeUniv gridDim (\y x -> Seeds.bool2cell (y `mod` 5 < 3 && x `mod` 2 == 0 && (x + y) `mod` 3 < 1))

--main = mainWith $ mkCAGif Seeds.seedsCA startGrid 500


-- Life
-- =======


gridDim = 20
startGrid :: Univ GameOfLife.Cell
startGrid = makeUniv gridDim (\y x -> GameOfLife.bool2cell ((y ^ 13 `mod` 1023 <= 512) && (x ^ 17 `mod` 2047 <= 512)))

main = mainWith $ mkCAGif GameOfLife.gameOfLifeCA startGrid 20
