{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Cellular
import qualified Data.Vector as V
import Diagrams.Backend.Rasterific.CmdLine
import qualified GameOfLife
import qualified Seeds
import qualified BriansBrain
import qualified Cyclic1D 
import qualified Cyclic2D 
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
-- Cyclic 1D
-- =========

cyclic1DDim = 200
cyclic1DTypes = 4

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

-- Cyclic 2D
-- =========

cyclic2dDim = 30
cyclic2DTypes = 15

cyclic2DGenerator :: Int -> IO Cyclic2D.Cell
cyclic2DGenerator i = do
    val <- getStdRandom (randomR (0, cyclic2DTypes - 1))
    return $ Cyclic2D.Cell {
        Cyclic2D.total=cyclic2DTypes,
        Cyclic2D.val=val
  }

cyclic2DStartGrid :: IO (Cyclic2D.Cyclic2D Cyclic2D.Cell)
cyclic2DStartGrid = do
    Cyclic2D.Cyclic2D <$> mMakeUniv cyclic2dDim  (\o i -> cyclic2DGenerator (o * i))

-- main = print "hello, world"
mainCyclic2D = do
    start <- cyclic2DStartGrid
    gifMain $ mkCAGif start 100


main = mainCyclic2D