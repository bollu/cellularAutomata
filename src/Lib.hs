{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib (RingZipper(RingZipper, before, focus, after),
            lengthRingZipper,
            focusIndexRingZipper,
            mergeRingZipper,
            shiftLeft,
            shiftRight,
            editRingZipper,
            shiftRightUniv,
            shiftLeftUniv,
            shiftUpUniv,
            shiftDownUniv,
            makeRingZipper,
            getRingZipperNeighbours,
            Univ(Univ),
            makeUniv,
            getUnivNeighbours,
            CellularAutomata(CellularAutomata, stepCell, renderUniv),
            mkCAGif) where


import GHC.Generics (Generic)
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import qualified Data.List as L--intersperse
import Data.Function.Memoize
import Control.Parallel.Strategies
import Data.Vector as V
import Data.Vector.Strategies

numParChunks :: Int
numParChunks = 10

-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.

data RingZipper a = RingZipper {
    before :: Vector a,
    focus  :: a,
    after  :: Vector a
} deriving(Eq, Generic, NFData)

instance Functor RingZipper where
    fmap f RingZipper{..} = RingZipper {
        before = fmap f (before `using` parTraversable rseq),
        focus = f focus,
        after = fmap f (after `using` parTraversable rseq)
    }

instance Show a => Show (RingZipper a) where
    show z = "!"<> showElements (before z) <> showCenterElement (focus z) <> showElements (Lib.after z) <> "!"
                where
                    --showElements l = folmconcat (L.intersperse "  " (fmap show l))
                    showElements l = foldl' (\str val -> str <> " " <> show val ) ""  l
                    showCenterElement x =  "  (" <> show x <> ")  "

lengthRingZipper :: RingZipper a -> Int
lengthRingZipper z = V.length (before z) + 1 + V.length (Lib.after z)

focusIndexRingZipper :: RingZipper a -> Int
focusIndexRingZipper z = V.length (before z)

mergeRingZipper :: RingZipper a -> V.Vector a
mergeRingZipper z = V.concat [before z, V.singleton (focus z), Lib.after z]


getRingZipperNeighbours :: RingZipper a -> (V.Vector a)
getRingZipperNeighbours z = V.fromList [extract $ shiftLeft z, extract $ shiftRight z]

shiftLeft :: RingZipper a -> RingZipper a
shiftLeft z = RingZipper {
        before = before',
        focus = focus',
        after = after'
    }
    where
        merged = mergeRingZipper z
        focusAt' = (focusIndexRingZipper z - 1) `mod` (V.length merged)
        
        focus' = merged V.! focusAt'
        before' =   
            if V.null (before z)
                then V.init merged
                else V.init (before z)
        after' = 
            if V.null (before z)
                then V.empty
                else V.cons (focus z) (Lib.after z)


shiftRight :: RingZipper a -> RingZipper a
shiftRight z = RingZipper {
        before = before',
        focus = focus',
        after = after'
    }
    where
        merged  = mergeRingZipper z
        focusAt' = (focusIndexRingZipper z + 1) `mod` (V.length merged)
        
        focus' = merged V.! focusAt'
        before' =   
            if V.null (Lib.after z)
                then empty
                else V.snoc (before z) (focus z)
        after' = 
            if V.null (Lib.after z)
                then V.tail merged
                else V.tail (Lib.after z)



iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = iterate f (f x)

instance Comonad RingZipper where
    -- extract :: RingZipper a -> a
    extract RingZipper {..} = focus

    -- duplicate :: RingZipper a -> RingZipper(RingZipper a)
    duplicate z = RingZipper {
        before = before,
        focus = z,
        after = after
    } where
        focusAt = focusIndexRingZipper z
        before = V.reverse $ V.iterateN focusAt shiftLeft (shiftLeft z)
        after = V.iterateN (lengthRingZipper z - focusAt - 1) shiftRight (shiftRight z)


editRingZipper :: RingZipper a -> (a -> a) -> RingZipper a
editRingZipper RingZipper{..} f = RingZipper {
    before = before,
    focus = f focus,
    after = after
}        


newtype Univ a = Univ (RingZipper (RingZipper a)) deriving(Eq)

-- deriveMemoizable ''Univ

instance Show a => Show (Univ a) where
    show (Univ RingZipper{..}) = " " <> showLines before <> showCenterLine focus <> showLines after where
            showLines l = L.foldl' (\str z -> str <> "\n" <> (show z)) ""  l
            showCenterLine = \x -> "\n(" <> show x <> ")\n "

instance Functor Univ where
    fmap f (Univ univ) = Univ((fmap . fmap) f univ)


-- lyxia help: Use Univ(Univ a) for performance
shiftRightUniv' :: Univ a -> Univ a
shiftRightUniv' (Univ univ) = Univ(fmap shiftRight univ)

shiftRightUniv = shiftRightUniv'

-- shiftRightUnivMemo :: Memoizable a =>  Univ a -> Univ a
-- shiftRightUnivMemo = memoize shiftRightUniv'


shiftLeftUniv' :: Univ a -> Univ a
shiftLeftUniv' (Univ univ) = Univ(fmap shiftLeft univ)

shiftLeftUniv = shiftLeftUniv'

-- shiftLeftUnivMemo :: Memoizable a => Univ a -> Univ a
-- shiftLeftUnivMemo = memoize shiftLeftUniv'

shiftUpUniv' :: Univ a -> Univ a
shiftUpUniv' (Univ univ) = Univ(shiftLeft univ)

shiftUpUniv = shiftUpUniv'

-- shiftUpUnivMemo :: Memoizable a => Univ a -> Univ a
-- shiftUpUnivMemo = memoize shiftUpUniv'

shiftDownUniv' :: Univ a -> Univ a
shiftDownUniv' (Univ univ) = Univ(shiftRight univ)

shiftDownUniv = shiftDownUniv'

-- shiftDownUnivMemo :: Memoizable a => Univ a -> Univ a
-- shiftDownUnivMemo = memoize shiftDownUniv'

instance Comonad Univ where
    extract (Univ univ) = extract $ extract univ
    -- duplicate :: Univ a -> Univ (Univ a)
    -- univ :: RingZipper (RingZipper a)
    duplicate (u @ (Univ univ)) = Univ $ RingZipper {
        before = outerBefores focusInner,
        focus = focusInner,
        after =  outerAfters focusInner
    } where
        focusInner = RingZipper {
            before = innerBefores u,
            focus = u,
            after = innerAfters u

        }
        outerBefores x = V.reverse $ V.iterateN outerFocusAt (fmap shiftUpUniv) (fmap shiftUpUniv x) 
        outerAfters x = V.iterateN (outerLength - outerFocusAt - 1) (fmap shiftDownUniv) (fmap shiftDownUniv x) 

        innerBefores x = V.reverse $ V.iterateN innerFocusAt shiftLeftUniv (shiftLeftUniv x)
        innerAfters x = V.iterateN (innerLength - innerFocusAt - 1) shiftRightUniv (shiftRightUniv x)

        -- outerBefores =  \x -> reverse $ take outerFocusAt $ iterate1 (fmap shiftUpUniv) x
        -- outerAfters = \x -> take (outerLength - outerFocusAt - 1) $ iterate1 (fmap shiftDownUniv) x 

        -- innerBefores = \x -> reverse $ take innerFocusAt $ iterate1 shiftLeftUniv x
        -- innerAfters = \x -> take (innerLength - innerFocusAt - 1) $ iterate1 shiftRightUniv x

        outerFocusAt = focusIndexRingZipper univ
        outerLength = lengthRingZipper univ

        innerFocusAt = focusIndexRingZipper $ extract univ 
        innerLength = lengthRingZipper $ extract univ
type Dim = Int
-- Dim \in [0, n - 1]
makeRingZipper :: Dim -> (Dim -> a) -> RingZipper a
makeRingZipper n f = RingZipper {
    -- before = fmap f [0..(center - 1)],
    before = fmap f (V.enumFromTo 0 (center - 1)),
    focus = f center,
    -- after = fmap f [(center + 1)..(n - 1)]
    after = fmap f (V.enumFromTo (center + 1) (n - 1))
} where
    center = n `div` 2

mMakeRingZipper :: Dim -> (Dim -> IO a) -> IO (RingZipper a)
mMakeRingZipper n f = do
    let mid = n `div` 2
    before <- V.generateM (mid - 1) f
    after <- V.generateM (n - mid + 1) (\x -> f (x + mid))
    focus <- f mid
    return $ RingZipper {
        before=randBefore, 
        focus=Cyclic1D.Cell {Cyclic1D.total=cyclic1DTypes, Cyclic1D.value=0},
        after=randAfter
    }

type OuterDim = Int
type InnerDim = Int


makeUniv :: Dim -> (OuterDim -> InnerDim -> a) -> Univ a
makeUniv dim f = Univ $ makeRingZipper dim (\outerDim -> makeRingZipper dim (f outerDim))



mMakeUniv :: Dim -> (OuterDim -> InnerDim -> IO a) -> IO (Univ a)
mMakeUniv dim f = do


getUnivNeighbours :: Univ a -> V.Vector a
getUnivNeighbours univ = V.fromList $ [extract . shiftUpUniv $ univ,
                      extract . shiftUpUniv . shiftRightUniv $ univ,
                      extract . shiftRightUniv $ univ,
                      extract . shiftDownUniv . shiftRightUniv $ univ,
                      extract . shiftDownUniv $ univ,
                      extract . shiftDownUniv . shiftLeftUniv $ univ,
                      extract . shiftLeftUniv $ univ,
                      extract . shiftUpUniv . shiftLeftUniv $ univ]

-- getUnivNeighboursMemo :: Memoizable a => Univ a -> [a]
-- getUnivNeighboursMemo univ = [extract . shiftUpUnivMemo $ univ,
--                       extract . shiftUpUnivMemo . shiftRightUnivMemo $ univ,
--                       extract . shiftRightUnivMemo $ univ,
--                       extract . shiftDownUnivMemo . shiftRightUnivMemo $ univ,
--                       extract . shiftDownUnivMemo $ univ,
--                       extract . shiftDownUnivMemo . shiftLeftUnivMemo $ univ,
--                       extract . shiftLeftUnivMemo $ univ,
--                      extract . shiftUpUnivMemo . shiftLeftUniv $ univ]

editUniverse :: Univ a -> (a -> a) -> Univ a 
editUniverse (Univ univ) f = Univ $ editRingZipper univ (\zip -> editRingZipper zip f)

--class Comonad u => CellularAutomata (u a) where
--    render :: u -> Diagram B
--    step :: u -> a

-- u = universe
-- a = smaller piece
data CellularAutomata u a = CellularAutomata {
    renderUniv :: u a -> Diagram B,
    stepCell :: u a -> a
}

type Steps = Int
mkCAGif :: Comonad u => CellularAutomata u a -> u a -> Steps -> [(QDiagram B V2 Double Any, Int)]
mkCAGif ca seed stepsOut = V.toList $ V.zip renderedSteps (V.replicate stepsOut  (10 :: Int)) where
    renderedSteps = fmap (renderUniv ca) (us `using` parTraversable rseq)
    stepUniv u = u =>> stepCell ca
    us = V.iterateN stepsOut stepUniv seed
