
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Rule  where
import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import qualified Data.Vector as V
import Data.MonoTraversable
import DeriveMonoComonadTH
import Data.Bits


data Cell = Cell { ruleix:: Int, b :: Bool }
newtype Rule = Rule (RingZipper Cell)

$(deriveMonoInstances ''Rule)

instance CA Rule where
  stepCell  = Rule.stepCell
  renderCA = Rule.renderCA


boolstointbigendian :: [Bool] -> Int
boolstointbigendian [] = 0
boolstointbigendian (b:bs') = 
    let n = (boolstointbigendian bs')
    in if b
       then setBit n (length bs')
       else n

stepCell :: Rule -> Cell
stepCell (Rule s) =
    cell'
    where
        cell = extract s 
        cell' = update cell (getRingZipperNeighbours s) (ruleix cell)

update :: Cell -> -- current center
          V.Vector Cell -> -- neighours 
          Int -> -- rule index
          Cell
update cur cells ruleix =
    let bs = map b ((cells V.! 0):cur:cells V.! 1:[])
    in Cell { b = testBit ruleix (boolstointbigendian bs), ruleix=ruleix}

renderCA :: CADiagramBackend b => Rule -> QDiagram b V2 (N b) Any
renderCA (Rule rz) = ringZipperToDiagram cellToDiagram rz


cellToDiagram :: CADiagramBackend b => Cell -> QDiagram b V2 (N b) Any
cellToDiagram Cell{b=False, ..}  = (rect 1 4# fc (sRGB24read "#1abc9c"))
cellToDiagram Cell{b=True, ..} = (rect 1 4 # fc (sRGB24read "#f1c40f"))
