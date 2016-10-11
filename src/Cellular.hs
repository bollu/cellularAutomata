{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}

module Cellular (RingZipper(RingZipper, before, focus, after),
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
            mMakeUniv,
            getUnivNeighbours,
            CADiagramConstraints,
            CA(stepCell, renderCA),
            mkCAGif) where


import GHC.Generics (Generic)
import Control.Comonad
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Grid
import Control.Monad
import Data.Active
import qualified Data.List as L--intersperse
import Data.Function.Memoize
import Control.Parallel.Strategies
import Data.Vector as V
import Data.Vector.Strategies
import Diagrams.Core.Types
import Data.Typeable.Internal

numParChunks :: Int
numParChunks = 10

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
    show z = "|"<> showElements (before z) <> showCenterElement (focus z) <> showElements (Cellular.after z) <> "|"
                where
                    showElements l = foldl' (\str val -> str <> " " <> show val ) ""  l
                    showCenterElement x =  "  (" <> show x <> ")  "

lengthRingZipper :: RingZipper a -> Int
lengthRingZipper z = V.length (before z) + 1 + V.length (Cellular.after z)

focusIndexRingZipper :: RingZipper a -> Int
focusIndexRingZipper z = V.length (before z)

mergeRingZipper :: RingZipper a -> V.Vector a
mergeRingZipper z = V.concat [before z, V.singleton (focus z), Cellular.after z]


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
                else V.cons (focus z) (Cellular.after z)


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
            if V.null (Cellular.after z)
                then empty
                else V.snoc (before z) (focus z)
        after' = 
            if V.null (Cellular.after z)
                then V.tail merged
                else V.tail (Cellular.after z)



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

instance Show a => Show (Univ a) where
    show (Univ RingZipper{..}) = " " <> showLines before <> showCenterLine focus <> showLines after where
            showLines l = L.foldl' (\str z -> str <> "\n" <> (show z)) ""  l
            showCenterLine = \x -> "\n(" <> show x <> ")\n "

instance Functor Univ where
    fmap f (Univ univ) = Univ((fmap . fmap) f univ)


shiftRightUniv :: Univ a -> Univ a
shiftRightUniv (Univ univ) = Univ(fmap shiftRight univ)


shiftLeftUniv :: Univ a -> Univ a
shiftLeftUniv (Univ univ) = Univ(fmap shiftLeft univ)

shiftUpUniv :: Univ a -> Univ a
shiftUpUniv (Univ univ) = Univ(shiftLeft univ)

shiftDownUniv :: Univ a -> Univ a
shiftDownUniv (Univ univ) = Univ(shiftRight univ)

instance Comonad Univ where
    extract (Univ univ) = extract $ extract univ

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

        outerFocusAt = focusIndexRingZipper univ
        outerLength = lengthRingZipper univ

        innerFocusAt = focusIndexRingZipper $ extract univ 
        innerLength = lengthRingZipper $ extract univ

type Dim = Int


mMakeRingZipper :: Monad m => Dim -> (Dim -> m a) -> m (RingZipper a)
mMakeRingZipper n f = do
    let mid = n `div` 2
    before <- V.generateM (mid - 1) f
    after <- V.generateM (n - mid + 1) (\x -> f (x + mid))
    focus <- f mid
    return $ RingZipper {
        before=before, 
        focus=focus,
        after=after
    }

type OuterDim = Int
type InnerDim = Int

makeRingZipper :: Dim -> (Dim -> a) -> RingZipper a
makeRingZipper n f = runIdentity $  mMakeRingZipper n (return . f)


mMakeUniv :: Monad m => Dim -> (OuterDim -> InnerDim -> m a) -> m (Univ a)
mMakeUniv dim f = do
  Univ <$> mMakeRingZipper dim (\outerDim -> mMakeRingZipper dim (f outerDim))


makeUniv :: Dim -> (OuterDim -> InnerDim -> a) -> Univ a
makeUniv dim f = runIdentity $ mMakeUniv dim (\o i -> return $ f o i)


getUnivNeighbours :: Univ a -> V.Vector a
getUnivNeighbours univ = V.fromList $ [extract . shiftUpUniv $ univ,
                      extract . shiftUpUniv . shiftRightUniv $ univ,
                      extract . shiftRightUniv $ univ,
                      extract . shiftDownUniv . shiftRightUniv $ univ,
                      extract . shiftDownUniv $ univ,
                      extract . shiftDownUniv . shiftLeftUniv $ univ,
                      extract . shiftLeftUniv $ univ,
                      extract . shiftUpUniv . shiftLeftUniv $ univ]

editUniverse :: Univ a -> (a -> a) -> Univ a 
editUniverse (Univ univ) f = Univ $ editRingZipper univ (\zip -> editRingZipper zip f)


type CADiagramConstraints b = (Data.Typeable.Internal.Typeable (N b), RealFloat (N b), Backend b V2 (N b), Renderable (Path V2 (N b)) b)

class CA u a where
  renderCA :: CADiagramConstraints b  => u a -> QDiagram b V2 (N b) Any
  stepCell :: u a -> a



type Steps = Int

mkCAGif :: (Comonad u, CA u a, CADiagramConstraints b) => u a -> Steps -> [(QDiagram b V2 (N b) Any, Int)]
mkCAGif ca stepsOut = V.toList $ V.zip renderedSteps (V.replicate stepsOut  (5 :: Int)) where
    renderedSteps = fmap renderCA (us `using` parTraversable rseq)
    stepUniv u = u =>> stepCell
    us = V.iterateN stepsOut stepUniv ca
