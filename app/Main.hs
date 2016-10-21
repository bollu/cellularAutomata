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


caMain :: CA ca => IO ca -> Steps -> IO ()
caMain iostart nsteps = do
  start <- iostart
  gifMain $ mkCAGif start nsteps
  

-- Game of Life
-- ============

golDim = 20

golGenerator :: IO GameOfLife.Cell
golGenerator = do
    val <- getStdRandom (randomR (0, 1)) :: IO Int
    return $ if val == 0 then GameOfLife.Off else GameOfLife.On

golStartGrid :: IO (GameOfLife.GameOfLife)
golStartGrid = do
    univ <- makeUnivM golDim (const . const $ golGenerator)
    return $ GameOfLife.GameOfLife univ  


golMain = caMain golStartGrid 100

-- Life
-- =======

-- Brians Brain
-- ============

briansDim = 20 

briansGenerator :: IO BriansBrain.Cell
briansGenerator = do
  val <- getStdRandom (randomR (0, 2)) :: IO Int
  let cell = case val of
              0 -> BriansBrain.On
              1 -> BriansBrain.Dying
              2 -> BriansBrain.Off
  
  return cell

briansStartGrid :: IO (BriansBrain.BriansBrain)
briansStartGrid = do
  univ <- makeUniv briansDim (const . const $ briansGenerator)
  return $ BriansBrain.BriansBrain univ
--
-- Cyclic 1D
-- =========

-- Cyclic 2D
-- =========

cyclic2dDim = 30
cyclic2DTypes = 15

cyclic2DGenerator :: IO Cyclic2D.Cell
cyclic2DGenerator = do
    val <- getStdRandom (randomR (0, cyclic2DTypes - 1))
    return $ Cyclic2D.Cell {
        Cyclic2D.total=cyclic2DTypes,
        Cyclic2D.val=val
  }

cyclic2DStartGrid :: IO (Cyclic2D.Cyclic2D)
cyclic2DStartGrid = do
    univ <-  makeUnivM cyclic2dDim  (const . const $ cyclic2DGenerator)
    return $ Cyclic2D.Cyclic2D univ

cyclic2DMain = caMain cyclic2DStartGrid 100

main = cyclic2DMain
