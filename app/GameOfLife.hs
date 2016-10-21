{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module GameOfLife where

import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import qualified Data.Vector as V
import Data.MonoTraversable

data Cell = On | Off deriving(Eq)

newtype GameOfLife = GameOfLife (Univ Cell)
type instance Element GameOfLife = Cell

instance CA GameOfLife where
  stepCell  = GameOfLife.stepCell
  renderCA = GameOfLife.renderCA

instance MonoFunctor GameOfLife where
  omap f (GameOfLife u) = GameOfLife (fmap f u)
  

instance MonoComonad GameOfLife where
  oextract (GameOfLife u) = extract u
  oextend f (GameOfLife u) = GameOfLife $ u =>> (f . GameOfLife)


liveNeighbourCount :: GameOfLife -> Int
liveNeighbourCount (GameOfLife grid) = V.sum $ fmap (\c -> if c == On then 1 else 0) (getUnivNeighbours grid)

stepCell :: GameOfLife -> Cell
stepCell gol = 
    cell'
    where
        cell' = if numNeighbours > 3 then Off
                else if numNeighbours < 2 then Off
                else if cell == Off && numNeighbours == 3 then On
                else cell
        cell = oextract gol 
        numNeighbours = liveNeighbourCount gol
                                

renderCA :: CADiagramBackend b => GameOfLife -> QDiagram b V2 (N b) Any
renderCA (GameOfLife (Univ grid)) = gridCat $ V.toList $ fmap cellToDiagram $ join (fmap mergeRingZipper (mergeRingZipper grid))

bool2cell :: Bool -> Cell
bool2cell True = On
bool2cell False = Off


cellToDiagram :: CADiagramBackend b => Cell -> QDiagram b V2 (N b) Any
cellToDiagram On = square 1 # fc blue
cellToDiagram Off = square 1 # fc white

