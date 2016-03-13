{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import qualified Data.Vector as V
import Diagrams.Backend.Cairo.CmdLine
import qualified GameOfLife
import qualified Seeds
import qualified BriansBrain
import qualified Cyclic1D 
import System.Random

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

briansGridDim = 20 
briansStartGrid :: Univ BriansBrain.Cell
briansStartGrid = makeUniv briansGridDim (\y x -> if (y ^ 13 `mod` 1023 <= 5)
                                      then
                                        if (x ^ 17 `mod` 2047 <= 2)
                                            then BriansBrain.On
                                            else BriansBrain.Dying
                                      else
                                        BriansBrain.Off)
--main = gifMain $ mkCAGif BriansBrain.briansBrainCA startGrid 100
-- Cyclic 1D
-- =========

cyclic1DDim = 200
cyclic1DTypes = 4

type Exp = Int
cyclic1Dgenerator :: Int -> IO Cyclic1D.Cell
cyclic1Dgenerator i = do
    val <- getStdRandom (randomR (0, cyclic1DTypes - 1))
    return $ Cyclic1D.Cell {
        Cyclic1D.total=cyclic1DTypes,
        Cyclic1D.value=val
  }

cyclic1DStartGrid :: IO (RingZipper Cyclic1D.Cell)
cyclic1DStartGrid = do
    randBefore <- V.generateM (cyclic1DDim - 1) cyclic1Dgenerator
    randAfter <- V.generateM (cyclic1DDim - 1) cyclic1Dgenerator
    return $ RingZipper {
        before=randBefore, 
        focus=Cyclic1D.Cell {Cyclic1D.total=cyclic1DTypes, Cyclic1D.value=0},
        after=randAfter
    }

main = do
    start <- cyclic1DStartGrid
    gifMain $ mkCAGif Cyclic1D.cyclic1DCA start  100
