{-# LANGUAGE TemplateHaskell #-}

module GameOfLife where

import Lib
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import Data.Function.Memoize
data Cell = On | Off deriving(Eq)

deriveMemoizable ''Cell
type Grid = Univ Cell

liveNeighbourCount :: Grid -> Int
liveNeighbourCount grid = sum $ fmap (\c -> if c == On then 1 else 0) (getNeighboursMemo grid)

stepCell :: Grid -> Cell
stepCell grid = 
    cell'
    where
        cell' = if numNeighbours > 3 then Off
                else if numNeighbours < 2 then Off
                else if cell == Off && numNeighbours == 3 then On
                else cell
        cell = extract grid 
        numNeighbours = liveNeighbourCount $ grid
                                

renderUniv :: Grid -> Diagram B
renderUniv (Univ univ) = gridCat $ map cellToDiagram $ join (fmap mergeRingZipper (mergeRingZipper univ))

bool2cell :: Bool -> Cell
bool2cell True = On
bool2cell False = Off

cellToDiagram :: Cell -> Diagram B
cellToDiagram On = (square 1 # fc (sRGB24read "#03A9F4"))
cellToDiagram Off = (square 1 # fc (sRGB24read "#455A64"))


gameOfLifeCA = Lib.CellularAutomata {
    Lib.stepCell = GameOfLife.stepCell,
    Lib.renderUniv = GameOfLife.renderUniv
}
