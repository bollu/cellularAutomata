{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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
import DeriveMonoComonadTH

data Cell = On | Off deriving(Eq)

newtype GameOfLife = GameOfLife (Univ Cell)
$(deriveMonoInstances ''GameOfLife)

instance CA GameOfLife where
  stepCell  = GameOfLife.stepCell
  renderCA = GameOfLife.renderCA

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
renderCA (GameOfLife univ) = univToDiagram cellToDiagram univ

cellToDiagram :: CADiagramBackend b => Cell -> QDiagram b V2 (N b) Any
cellToDiagram On = square 1 # fc cyan
cellToDiagram Off = square 1 # fc white

