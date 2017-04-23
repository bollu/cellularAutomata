{-# LANGUAGE TemplateHaskell #-}
module DeriveMonoComonadTH where

import Cellular
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.MonoTraversable
import Control.Comonad

type Constructorname = Name
type Typename = Name
type Variablename = Name

type NewtypeInfo a = a
type InnerInfo a = a
type ComonadInfo a = a


patternMatchInner :: NewtypeInfo Constructorname -> InnerInfo Variablename -> Q Pat
patternMatchInner newtypecons  var =
  do
    let pmatch = (ConP newtypecons [VarP var])
    return pmatch

deriveMonoElement :: NewtypeInfo Type -> InnerInfo Typename -> Q[Dec]
deriveMonoElement newtypetype elemname = [d| type instance Element $(return newtypetype) = $(conT elemname) |]

deriveMonoFunctor :: NewtypeInfo Type -> NewtypeInfo Constructorname -> Q[Dec]
deriveMonoFunctor newtypetype newtypeconsname = do
  let patternmatchname = mkName "innervar"
  let monoinst = [d| instance MonoFunctor $(return newtypetype)  where
                       omap f $(patternMatchInner newtypeconsname patternmatchname) =  $(conE newtypeconsname) $ fmap f $(varE patternmatchname) |]
  strings <- fmap (\x -> show $ ppr x) monoinst
  monoinst



deriveMonoComonad :: NewtypeInfo Type -> NewtypeInfo Constructorname  -> Q[Dec]
deriveMonoComonad newtypetype newtypeconsname  = do
  let patternmatchname = mkName "innerComonad"
  [d| instance MonoComonad $(return newtypetype) where
          oextract $(patternMatchInner newtypeconsname patternmatchname) = extract $(varE patternmatchname)
          oextend f $(patternMatchInner newtypeconsname patternmatchname) = $(conE newtypeconsname) $ $(varE patternmatchname) =>> (f . $(conE newtypeconsname)) |]
  

deriveMonoInstances :: Name  -> Q[Dec]
deriveMonoInstances newtypename = do
  (TyConI newtypedecl) <- reify newtypename
  con <- case newtypedecl of
     NewtypeD _ _  _ _ con _ -> return con
     _ -> fail $ "deriveFunctor: |" ++ (show newtypename) ++ "| must be a newtype"
     
  let (NormalC newtypeconsname [(_, (AppT  (ConT comonad) (ConT inner)))]) =  con
  let newtypetype = ConT newtypename

  monoelem <- deriveMonoElement newtypetype inner
  monofunctor <- deriveMonoFunctor newtypetype newtypeconsname
  monocomonad <- deriveMonoComonad newtypetype newtypeconsname
 
  return $ monoelem ++ monofunctor ++ monocomonad
  -- fail . show  . ppr $ monoelem ++ monofunctor ++ monocomonad
  
  

