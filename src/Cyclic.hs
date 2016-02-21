{-# LANGUAGE TemplateHaskell #-}

module BriansBrain where

import Lib
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import Data.Function.Memoize
import qualified Data.Vector as V


data Cell = Int
deriveMemoizable ''Cell

type Grid = { univ :: Univ Cell, total :: Int }


stepCell :: Grid -> Cell
stepCell grid = 
    cell'
    where
        cell = extract (univ grid)
        cell' = if hasNextNeighbour (getNeighbours (univ grid)) then next else cell
        next = (cell + 1) `mod` (total grid)
        hasNextNeighbour neighbours = any (\n -> n == next) neighbours
                                

renderUniv :: Grid -> Diagram B
renderUniv Grid{..} = gridCat $ V.toList $ fmap cellToDiagram $ join (fmap mergeRingZipper (mergeRingZipper univ))


cellToDiagram :: Cell -> Diagram B
cellToDiagram On = (square 1 # fc (sRGB24read "#03A9F4"))
cellToDiagram Dying = (square 1 # fc (sRGB24read "#455A64"))
cellToDiagram Off = (square 1 # fc (sRGB24read "#202020"))


briansBrainCA = Lib.CellularAutomata {
    Lib.stepCell = BriansBrain.stepCell,
    Lib.renderUniv = BriansBrain.renderUniv
}
