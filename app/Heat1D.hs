
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Heat1D  where
import Cellular
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import qualified Data.Vector as V
import Data.MonoTraversable
import DeriveMonoComonadTH
import qualified Debug.Trace as Trace

-- normalised heat value from [0, 1]
data Cell = Cell { heat :: Float }
newtype Heat1D = Heat1D (RingZipper Cell)

$(deriveMonoInstances ''Heat1D)

instance CA Heat1D where
  stepCell  = Heat1D.stepCell
  renderCA = Heat1D.renderCA



tracestr :: Float -> V.Vector Float -> String
tracestr me neigh = "|" <> (show (neigh V.! 0)) <> " | " <> (show me) <> " | " <> (show (neigh V.! 1)) <> "|\n"

evolve :: Float -> V.Vector Float -> Float
evolve me neighbour = Trace.trace (tracestr me neighbour) 
                    (me + kappa * deltat * (((neighbour V.! 1) - 2.0 * me + (neighbour V.! 0)) / deltax ** 2)) where
  deltax = 1.0 / 20.0
  kappa = 0.01
  deltat = 1.0

stepCell :: Heat1D -> Cell
stepCell (Heat1D s) =
    cell'
    where
        cell = extract s 
        cell' = Cell (evolve (heat cell) (fmap heat neighbours))
        neighbours = getRingZipperNeighbours s

renderCA :: CADiagramBackend b => Heat1D -> QDiagram b V2 (N b) Any
renderCA (Heat1D rz) = ringZipperToDiagram (Heat1D.cellToDiagram minheat maxheat) rz where
  maxheat = maximum (heat <$> mergeRingZipper rz)
  minheat = minimum (heat <$> mergeRingZipper rz)


hotcolor = sRGB24read "#FF0000"
coldcolor = sRGB24read "#0000FF"

type MaxHeat = Float
type MinHeat = Float

heatToColor :: MinHeat -> MaxHeat -> Float -> Colour Double
heatToColor min max h = blend (realToFrac t) hotcolor coldcolor
  where
    t =  y0 + m * (h - x0)
    (x0, y0) = (min, 0.0)
    (x1, y1) = (max, 1.0)
    m = (y1 - y0) / (x1 - x0)

cellToDiagram :: CADiagramBackend b => MinHeat -> MaxHeat -> Cell -> QDiagram b V2 (N b) Any
cellToDiagram minheat maxheat Cell{..}  = (rect 1 4 # fc (heatToColor minheat maxheat heat))
