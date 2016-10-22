

{-# LANGUAGE TemplateHaskell #-}
module DeriveMonoComonadTH where

import Cellular
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.MonoTraversable

-- template haskell to generate MonoComonad for a newtype of a monomorphised
-- comonad
-- eg:

-- newtype MC = MC(W a)
-- instance Comonad w => (MonoComonad MC) where ...

-- InstanceD :: Cxt -> Type -> [Dec] -> Dec

-- reify :: Name -> Q Info

-- dataDec = ..  | NewtypeD Cxt Name [TyVarBndr] Con [Name] | ..

{- data Con
  = NormalC Name [StrictType]
  | RecC Name [VarStrictType]
  | InfixC StrictType Name StrictType
  | ForallC [TyVarBndr] Cxt Con
-}

getinner :: Name -> Type -> Dec -> Q Exp
getinner outerName outerTy dec = 
  do 
    nameinner <- newName "inner"
    let args = (ConP outerName [VarP nameinner]) 
    let body = VarE nameinner
    return $ LamE [args] body

patternMatchInner :: Name -> Name -> Q Pat
patternMatchInner dataname  innername =
  do
    let pmatch = (ConP dataname [VarP innername])
    return pmatch


deriveMonoFunctor :: Name  -> Q[Dec]
deriveMonoFunctor ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, tyVars, con) <- case tyCon of
     NewtypeD _ nm tyVars con _ -> return (nm, tyVars, con)
     _ -> fail "deriveFunctor: tyCon may not be a type synonym."
  let (NormalC constyname [(_, (AppT (ConT comonad) (ConT inner)))]) =  con

  [d| type instance Element (constyname) = $(conT inner) |]
  
  let innername = mkName "innervar"
  let monoinst = [d| instance MonoFunctor $(conT ty)  where
                       omap f $(patternMatchInner constyname innername) =  $(conE constyname) $ fmap f $(varE innername) |] 
                      -- omap f new = (fmap f ($(getinner ty outertype tyCon) new)) |]
  strings <- fmap (\x -> show $ ppr x) monoinst
  monoinst
  

