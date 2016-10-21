

{-# LANGUAGE TemplateHaskell #-}
module DeriveMonoComonadTH where

import Cellular
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- template haskell to generate MonoComonad for a newtype of a monomorphised
-- comonad
-- eg:

-- newtype MC = MC(W a)
-- instance Comonad w => (MonoComonad MC) where ...

deriveMonoComonad :: Type -> Q InstanceDec
deriveMonoComonad (ConT name) = return $  SigD  name (VarT (mkName "woo"))
deriveMonoComonad a =  fail $ "expected ConT, given" ++ (show a) 


