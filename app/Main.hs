{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Cellular
import qualified Data.Vector as V
import Diagrams.Core.Compile
import Diagrams.Core.Types
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific
import Diagrams.TwoD
import Data.Monoid (Any)

import qualified GameOfLife
import qualified Seeds
import qualified BriansBrain
import qualified Cyclic1D 
import qualified Cyclic2D 
import System.Random


-- mkCAGifRasterific :: CA u => u -> Steps -> [(QDiagram Rasterific V2 n  Any, Int)]
-- mkCAGifRasterific = mkCAGif

renderOpts :: FilePath -> (DiagramOpts, GifOpts) --MainOpts [(QDiagram Rasterific V2 n Any, Int)]
renderOpts outpath = let 
              diagramOpts = DiagramOpts { _width = Just 128, _height = Just 128, _output = outpath }
              gifOpts = GifOpts {_dither = False, _noLooping = False, _loopRepeat = Nothing}
             in (diagramOpts, gifOpts) 

caMain :: CA ca => FilePath -> IO ca -> Steps -> IO ()
caMain outpath iostart nsteps = do
  start <- iostart
  gifMain $ (mkCAGif start nsteps)
  -- mainRender ((renderOpts outpath) :: MainOpts [(QDiagram Rasterific V2 n Any, Int)]) (mkCAGif start nsteps)

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


golMain = caMain "gameoflife.gif" golStartGrid 100


-- Brians Brain
-- ============

briansDim = 50

briansGenerator :: IO BriansBrain.Cell
briansGenerator = do
  newStdGen
  val <- getStdRandom (randomR (0, 3)) :: IO Int
  let cell = case val of
              0 -> BriansBrain.On
              1 -> BriansBrain.Off
              2 -> BriansBrain.Off
              3 -> BriansBrain.Off
  
  return cell

briansStartGrid :: IO (BriansBrain.BriansBrain)
briansStartGrid = do
  univ <- makeUnivM briansDim (const . const $ briansGenerator)
  return $ BriansBrain.BriansBrain univ

briansBrainMain = caMain "briansBrain.gif" briansStartGrid 100

--
-- Cyclic 1D
-- =========

cyclic1dDim = 200
cyclic1dTypes = 4

cyclic1dGenerator :: IO Cyclic1D.Cell
cyclic1dGenerator = do
  newStdGen
  val <- getStdRandom (randomR (0, cyclic1dTypes)) :: IO Int
  return $ Cyclic1D.Cell val cyclic1dTypes

cyclic1dStartGrid :: IO (Cyclic1D.Cyclic1D)
cyclic1dStartGrid = do
  rz <- makeRingZipperM cyclic1dDim (const $ cyclic1dGenerator)
  return $ Cyclic1D.Cyclic1D rz

cyclic1dMain = caMain "cyclic1d.gif" cyclic1dStartGrid 100


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

cyclic2DMain = caMain "cyclic2d.gif" cyclic2DStartGrid 100

main = cyclic1dMain
