module MaterialColors() where

import Data.Colour.SRGB

black ::Floating b => Ord b => Colour b
black = sRGB24read "#212121"

gray :: Floating b => Ord b => Colour b
gray = sRGB24read "9E9E9E"


white :: Floating b => Ord b => Colour b
white = sRGB24read "#F5F5F5"

blue :: Floating b => Ord b => Colour b
blue = sRGB24read "#2196f3"

cyan :: Floating b => Ord b => Colour b
cyan = sRGB24read "#00BCD4"

indigo :: Floating b => Ord b => Colour b
indigo = sRGB24read "#303F9F"

green :: Floating b => Ord b => Colour b
green = sRGB24read "27ae60"
