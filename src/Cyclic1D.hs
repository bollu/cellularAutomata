{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Cyclic1D(Cell(Cell, value, total), Simulation, cyclic1DCA) where
import Lib
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import qualified Data.Vector as V


data Cell = Cell { value :: Int, total :: Int }
type Simulation = RingZipper Cell

stepCell :: Simulation -> Cell
stepCell s =
    cell'
    where
        cell = extract s 
        cell' = if hasNextNeighbour (getRingZipperNeighbours s)
           then Cell { value = (Cyclic1D.value cell + 1) `mod` (total cell), total = total cell}
           else cell
        hasNextNeighbour neighbours = any (\c -> Cyclic1D.value c == ((Cyclic1D.value cell) + 1) `mod` (total cell)) neighbours

renderUniv :: Simulation -> Diagram B
renderUniv s = hcat $ V.toList $ fmap cellToDiagram $ (mergeRingZipper s)

cellToDiagram :: Cell -> Diagram B
cellToDiagram Cell{value=0, ..}  = (rect 1 4# fc (sRGB24read "#1abc9c"))
cellToDiagram Cell{value=1, ..} = (rect 1 4 # fc (sRGB24read "#f1c40f"))
cellToDiagram Cell{value=2, ..} = (rect 1 4 # fc (sRGB24read "#e67e22"))
cellToDiagram Cell{value=3, ..} = (rect 1 4 # fc (sRGB24read "#9b59b6"))
cellToDiagram Cell{value=4, ..} = (rect 1 4 # fc (sRGB24read "#2c3e50"))
cellToDiagram Cell{..} = square 1 # fc (sRGB f (1.0 - f) 0.0) where
                            f = (fromIntegral value / fromIntegral total)

cyclic1DCA = Lib.CellularAutomata {
    Lib.stepCell = Cyclic1D.stepCell,
    Lib.renderUniv = Cyclic1D.renderUniv
}
