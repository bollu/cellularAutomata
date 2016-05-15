{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Cyclic2D where

import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import Data.Function.Memoize
import qualified Data.Vector as V


data Cell = Cell { val :: Int, total :: Int }
type Grid = Univ Cell

stepCell :: Grid -> Cell
stepCell s =
    cell'
    where
        cell = extract s 
        cell' = if hasNextNeighbour (getUnivNeighbours s)
           then Cell { val = (val cell + 1) `mod` (total cell), total = total cell}
           else cell
        hasNextNeighbour neighbours = any (\c -> val c == ((val cell) + 1) `mod` (total cell)) neighbours

renderUniv :: Grid -> Diagram B
renderUniv (Univ univ) = gridCat $ V.toList $ fmap cellToDiagram $ join (fmap mergeRingZipper (mergeRingZipper univ))

cellToDiagram :: Cell -> Diagram B
cellToDiagram Cell{val=0, ..}  = rect 1 1# fc (sRGB24read "#1abc9c")
cellToDiagram Cell{val=1, ..} = rect 1 1 # fc (sRGB24read "#f1c40f")
cellToDiagram Cell{val=2, ..} = rect 1 1 # fc (sRGB24read "#e67e22")
cellToDiagram Cell{val=3, ..} = rect 1 1 # fc (sRGB24read "#9b59b6")
cellToDiagram Cell{val=4, ..} = rect 1 1 # fc (sRGB24read "#2c3e50")
cellToDiagram Cell{..} = square 1 # fc (sRGB 0.2 (1.0 - 0.2) 0.0)
cyclic2DCA = Cellular.CellularAutomata {
    Cellular.stepCell = Cyclic2D.stepCell,
    Cellular.renderUniv = Cyclic2D.renderUniv
}
