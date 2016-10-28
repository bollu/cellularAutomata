{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Cyclic1D  where
import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import qualified Data.Vector as V
import Data.MonoTraversable
import DeriveMonoComonadTH


data Cell = Cell { value :: Int, total :: Int }
newtype Cyclic1D = Cyclic1D (RingZipper Cell)

$(deriveMonoInstances ''Cyclic1D)

instance CA Cyclic1D where
  stepCell  = Cyclic1D.stepCell
  renderCA = Cyclic1D.renderCA

stepCell :: Cyclic1D -> Cell
stepCell (Cyclic1D s) =
    cell'
    where
        cell = extract s 
        cell' = if hasNextNeighbour (getRingZipperNeighbours s)
           then Cell { value = (Cyclic1D.value cell + 1) `mod` (total cell), total = total cell}
           else cell
        hasNextNeighbour neighbours = any (\c -> Cyclic1D.value c == ((Cyclic1D.value cell) + 1) `mod` (total cell)) neighbours

renderCA :: CADiagramBackend b => Cyclic1D -> QDiagram b V2 (N b) Any
renderCA (Cyclic1D rz) = ringZipperToDiagram cellToDiagram rz


cellToDiagram :: CADiagramBackend b => Cell -> QDiagram b V2 (N b) Any
cellToDiagram Cell{value=0, ..}  = (rect 1 4# fc (sRGB24read "#1abc9c"))
cellToDiagram Cell{value=1, ..} = (rect 1 4 # fc (sRGB24read "#f1c40f"))
cellToDiagram Cell{value=2, ..} = (rect 1 4 # fc (sRGB24read "#e67e22"))
cellToDiagram Cell{value=3, ..} = (rect 1 4 # fc (sRGB24read "#9b59b6"))
cellToDiagram Cell{value=4, ..} = (rect 1 4 # fc (sRGB24read "#2c3e50"))
cellToDiagram Cell{..} = square 1 # fc (sRGB f (1.0 - f) 0.0) where
                            f = (fromIntegral value / fromIntegral total)
