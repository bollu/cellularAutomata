{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BriansBrain where

import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import MaterialColors
import qualified Data.Vector as V
import Data.MonoTraversable
import DeriveMonoComonadTH


data Cell = On | Off | Dying deriving(Eq)


newtype BriansBrain = BriansBrain (Univ Cell)
$(deriveMonoInstances ''BriansBrain)


instance CA BriansBrain where
  stepCell  = BriansBrain.stepCell
  renderCA = BriansBrain.renderCA

liveNeighbourCount :: BriansBrain -> Int
liveNeighbourCount (BriansBrain grid) = V.sum $ fmap (\c -> if c == On then 1 else 0) (getUnivNeighbours grid)

stepCell :: BriansBrain -> Cell
stepCell (briansbrain)  =
    cell'
    where
        cell' = if cell == Off && numNeighbours == 2 then On
                else if cell == On then Dying
                else Off
        cell = oextract briansbrain
        numNeighbours = liveNeighbourCount $ briansbrain


renderCA :: CADiagramBackend b => BriansBrain -> QDiagram b V2 (N b) Any
renderCA (BriansBrain grid) = univToDiagram cellToDiagram grid

cellToDiagram :: CADiagramBackend b => Cell -> QDiagram b V2 (N b) Any
cellToDiagram On = square 1 # fc cyan
cellToDiagram Dying = square 1 # fc indigo
cellToDiagram Off = square 1 # fc black

