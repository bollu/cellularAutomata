{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}


module Lib (RingZipper(RingZipper),
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
            Univ(Univ),
            makeUniv,
            getNeighbours,
            getNeighboursMemo,
            CellularAutomata(CellularAutomata, stepCell, renderUniv),
            mkCAGif) where



import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import Data.List --intersperse
import Data.Function.Memoize

-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.

data RingZipper a = RingZipper {
    before :: [a],
    focus  :: a,
    after  :: [a]
} deriving(Functor, Eq)   

deriveMemoizable ''RingZipper

instance Show a => Show (RingZipper a) where
    show z = "!" ++ showElements (before z) ++ showCenterElement (focus z) ++ showElements (Lib.after z) ++ "!"
                where
                    showElements l = mconcat (intersperse "  " (fmap show l))
                    showCenterElement x =  "  (" ++ show x ++ ")  "

lengthRingZipper :: RingZipper a -> Int
lengthRingZipper z = length (before z) + 1 + length (Lib.after z)

focusIndexRingZipper :: RingZipper a -> Int
focusIndexRingZipper z = length (before z)

mergeRingZipper :: RingZipper a -> [a]
mergeRingZipper z = before z ++ [focus z] ++ Lib.after z

shiftLeft :: RingZipper a -> RingZipper a
shiftLeft z = RingZipper {
        before = before',
        focus = focus',
        after = after'
    }
    where
        merged = mergeRingZipper z
        focusAt' = (focusIndexRingZipper z - 1) `mod` (length merged)
        
        focus' = merged !! focusAt'
        before' =   
            if null (before z)
                then init merged
                else init (before z)
        after' = 
            if null (before z)
                then []
                else (focus z):(Lib.after z)


shiftRight :: RingZipper a -> RingZipper a
shiftRight z = RingZipper {
        before = before',
        focus = focus',
        after = after'
    }
    where
        merged  = mergeRingZipper z
        focusAt' = (focusIndexRingZipper z + 1) `mod` (length merged)
        
        focus' = merged !! focusAt'
        before' =   
            if null (Lib.after z)
                then []
                else (before z) ++ [focus z]
        after' = 
            if null (Lib.after z)
                then tail merged
                else tail (Lib.after z)



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
        before = reverse $ take focusAt $ iterate1 shiftLeft z
        after = take (lengthRingZipper z - focusAt - 1) $ iterate1 shiftRight z


editRingZipper :: RingZipper a -> (a -> a) -> RingZipper a
editRingZipper RingZipper{..} f = RingZipper {
    before = before,
    focus = f focus,
    after = after
}        


newtype Univ a = Univ (RingZipper (RingZipper a)) deriving(Eq)

deriveMemoizable ''Univ

instance Show a => Show (Univ a) where
    show (Univ RingZipper{..}) = " " ++ showLines before ++ showCenterLine focus ++ showLines after where
            showLines l = mconcat $ intersperse "\n " (fmap show l) 
            showCenterLine = \x -> "\n(" ++ show x ++ ")\n "

instance Functor Univ where
    fmap f (Univ univ) = Univ((fmap . fmap) f univ)



shiftRightUniv' :: Univ a -> Univ a
shiftRightUniv' (Univ univ) = Univ(fmap shiftRight univ)

shiftRightUniv = shiftRightUniv'

shiftRightUnivMemo :: Memoizable a =>  Univ a -> Univ a
shiftRightUnivMemo = memoize shiftRightUniv'


shiftLeftUniv' :: Univ a -> Univ a
shiftLeftUniv' (Univ univ) = Univ(fmap shiftLeft univ)

shiftLeftUniv = shiftLeftUniv'

shiftLeftUnivMemo :: Memoizable a => Univ a -> Univ a
shiftLeftUnivMemo = memoize shiftLeftUniv'

shiftUpUniv' :: Univ a -> Univ a
shiftUpUniv' (Univ univ) = Univ(shiftLeft univ)

shiftUpUniv = shiftUpUniv'

shiftUpUnivMemo :: Memoizable a => Univ a -> Univ a
shiftUpUnivMemo = memoize shiftUpUniv'

shiftDownUniv' :: Univ a -> Univ a
shiftDownUniv' (Univ univ) = Univ(shiftRight univ)

shiftDownUniv = shiftDownUniv'

shiftDownUnivMemo :: Memoizable a => Univ a -> Univ a
shiftDownUnivMemo = memoize shiftDownUniv'

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
        outerBefores =  \x -> reverse $ take outerFocusAt $ iterate1 (fmap shiftUpUniv) x
        outerAfters = \x -> take (outerLength - outerFocusAt - 1) $ iterate1 (fmap shiftDownUniv) x 

        innerBefores = \x -> reverse $ take innerFocusAt $ iterate1 shiftLeftUniv x
        innerAfters = \x -> take (innerLength - innerFocusAt - 1) $ iterate1 shiftRightUniv x

        outerFocusAt = focusIndexRingZipper univ
        outerLength = lengthRingZipper univ

        innerFocusAt = focusIndexRingZipper $ extract univ 
        innerLength = lengthRingZipper $ extract univ
type Dim = Int
-- Dim \in [0, n - 1]
makeRingZipper :: Dim -> (Dim -> a) -> RingZipper a
makeRingZipper n f = RingZipper {
    before = fmap f [0..(center - 1)],
    focus = f center,
    after = fmap f [(center + 1)..(n - 1)]
} where
    center = n `div` 2


type OuterDim = Int
type InnerDim = Int


makeUniv :: Dim -> (OuterDim -> InnerDim -> a) -> Univ a
makeUniv dim f = Univ $ makeRingZipper dim (\outerDim -> makeRingZipper dim (f outerDim))



getNeighbours :: Univ a -> [a]
getNeighbours univ = [extract . shiftUpUniv $ univ,
                      extract . shiftUpUniv . shiftRightUniv $ univ,
                      extract . shiftRightUniv $ univ,
                      extract . shiftDownUniv . shiftRightUniv $ univ,
                      extract . shiftDownUniv $ univ,
                      extract . shiftDownUniv . shiftLeftUniv $ univ,
                      extract . shiftLeftUniv $ univ,
                      extract . shiftUpUniv . shiftLeftUniv $ univ]

getNeighboursMemo :: Memoizable a => Univ a -> [a]
getNeighboursMemo univ = [extract . shiftUpUnivMemo $ univ,
                      extract . shiftUpUnivMemo . shiftRightUnivMemo $ univ,
                      extract . shiftRightUnivMemo $ univ,
                      extract . shiftDownUnivMemo . shiftRightUnivMemo $ univ,
                      extract . shiftDownUnivMemo $ univ,
                      extract . shiftDownUnivMemo . shiftLeftUnivMemo $ univ,
                      extract . shiftLeftUnivMemo $ univ,
                      extract . shiftUpUnivMemo . shiftLeftUniv $ univ]

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
mkCAGif ca seed stepsOut = zip renderedSteps (replicate stepsOut  (10 :: Int)) where
    renderedSteps = fmap (renderUniv ca) us
    stepUniv u = u =>> stepCell ca
    us = iterate stepUniv seed
