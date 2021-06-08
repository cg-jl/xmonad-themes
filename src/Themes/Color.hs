{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Themes.Color (Color (..), ColorSet (..), colorString, selectColor) where

import Data.Aeson
import Data.Char
import Data.String (IsString, fromString)
import Data.Text
import GHC.Generics
import Prelude hiding (all, length)

newtype Color = Color {unColor :: Text} deriving (Eq)

instance IsString Color where
  {-# INLINE fromString #-}
  fromString = Color . pack

parseColor :: Text -> Either String Text
parseColor all' = maybe wasEmpty checkRest $ uncons all'
  where
    wasEmpty = Left "color codes must be non-empty"
    checkRest ('#', xs) | all isHexDigit xs && length xs == 6 = return all'
    checkRest ('#', _) = Left "color codes must have exaclty 6 hexadecimal characters"
    checkRest _ = Left "color codes must start with '#'"

instance FromJSON Color where
  parseJSON = withText "Color" $ either fail (return . Color) . parseColor

instance Show Color where
  {-# INLINEABLE show #-}
  show = unpack . unColor

data ColorSet = ColorSet
  { black :: Color,
    blue :: Color,
    cyan :: Color,
    green :: Color,
    magenta :: Color,
    red :: Color,
    white :: Color,
    yellow :: Color
  }
  deriving (Generic, Show)

instance FromJSON ColorSet

colorString :: (a -> Color) -> a -> String
{-# INLINEABLE colorString #-}
colorString = (show .)

selectColor :: Text -> ColorSet -> Either String Color
selectColor "black" = return . black
selectColor "blue" = return . blue
selectColor "cyan" = return . cyan
selectColor "green" = return . green
selectColor "magenta" = return . magenta
selectColor "red" = return . red
selectColor "white" = return . white
selectColor "yellow" = return . yellow
selectColor other = const $ Left $ "no color named " ++ show other ++ " exists."
