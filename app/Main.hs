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
import qualified Heat1D 
import qualified Rule
import System.Random


-- mkCAGifRasterific :: CA u => u -> Steps -> [(QDiagram Rasterific V2 n  Any, Int)]
-- mkCAGifRasterific = mkCAGif

renderOpts :: FilePath -> (DiagramOpts, GifOpts) --MainOpts [(QDiagram Rasterific V2 n Any, Int)]
renderOpts outpath = let 
              diagramOpts = DiagramOpts { _width = Just 128, _height = Just 128, _output = outpath }
              gifOpts = GifOpts {_dither = False, _noLooping = False, _loopRepeat = Nothing}
             in (diagramOpts, gifOpts) 

caGifMain :: CA ca => FilePath -> IO ca -> Steps -> IO ()
caGifMain outpath iostart nsteps = do
  start <- iostart
  gifMain $ (mkCAGif start nsteps)
  -- mainRender ((renderOpts outpath) :: MainOpts [(QDiagram Rasterific V2 n Any, Int)]) (mkCAGif start nsteps)
  --
caImageMain :: CA ca => FilePath -> IO ca -> Steps -> IO ()
caImageMain outpath iostart nsteps = do
  start <- iostart
  defaultMain $ (mkCAImage start nsteps)


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


golMain = caGifMain "gameoflife.gif" golStartGrid 100


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

briansBrainMain = caGifMain "briansBrain.gif" briansStartGrid 100

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

cyclic1dMain = caGifMain "cyclic1d.gif" cyclic1dStartGrid 100


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

cyclic2DMain = caGifMain "cyclic2d.gif" cyclic2DStartGrid 100

-- Heat
-- ====
heat1dDim :: Int
heat1dDim = 100

{-
heat1dGenerator :: IO Heat1D.Cell
heat1dGenerator = do
  newStdGen
  val <- getStdRandom (randomR (0, cyclic1dTypes)) :: IO Float
  return $ Heat1D.Cell val
-}

clampheat :: Float -> Float
clampheat x = min (max x 0) 1

heatfn :: Int -> Float
heatfn x = normx
          where
            normx = (fromIntegral x) / (fromIntegral heat1dDim)

heat1dStartGrid :: IO (Heat1D.Heat1D)
heat1dStartGrid = do
  let rz = makeRingZipper heat1dDim (Heat1D.Cell . clampheat . heatfn)
  return $ Heat1D.Heat1D rz

heat1dMain = caGifMain "heat1d.gif" heat1dStartGrid 100

ruledim :: Int
ruledim = 100

randbool :: IO Bool
randbool = randomIO


ruleStartGridAtCenter :: Int -> IO (Rule.Rule)
ruleStartGridAtCenter ruleix = do
  let rz =  makeRingZipper ruledim (\i -> Rule.Cell ruleix (i * 2 == ruledim))
  return $ Rule.Rule rz


ruleStartGridNotAtCenter :: Int -> IO (Rule.Rule)
ruleStartGridNotAtCenter ruleix = do
  let rz =  makeRingZipper ruledim (\i -> Rule.Cell ruleix (i * 2 /= ruledim))
  return $ Rule.Rule rz

ruleRandomInit :: Int -> IO (Rule.Rule)
ruleRandomInit ruleix = do
  rz <-  makeRingZipperM ruledim (\i -> Rule.Cell <$> pure ruleix <*> randbool) 
  return $ Rule.Rule rz

-- generate 1 with `bias` probability and `0` with (1 - bias) probability
randBoolBiased :: Float -> IO Bool
randBoolBiased b = do
    p <- randomIO
    return $ p <= b

-- create the rule CA iniitialized with a random ratio
ruleRandomInitRatio :: Float -> Int -> IO (Rule.Rule)
ruleRandomInitRatio b ruleix = do
  rz <-  makeRingZipperM ruledim (\i -> Rule.Cell <$> pure ruleix <*> (randBoolBiased b)) 
  return $ Rule.Rule rz

rule184AtCenterMain = caImageMain "rule184.jpeg" (ruleStartGridAtCenter 184) 10
rule184NotAtCenterMain = caImageMain "rule184.jpeg" (ruleStartGridNotAtCenter 184) 10

rule184BiasPoint2 = caImageMain "rule184.jpeg" (ruleRandomInitRatio 0.2 184) 10
rule184BiasPoint8 = caImageMain "rule184.jpeg" (ruleRandomInitRatio 0.8 184) 10

-- Sierpinski triangle
rule90FromCenter = caImageMain "rule-90-from-center.jpeg" (ruleStartGridAtCenter 90) 20


rule90Random = caImageMain "rule-90-random.jpeg" (ruleRandomInitRatio 0.2 90) 20

main = rule90Random
