{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Themes.Intermediates
  ( Theme (..),
    Borders (..),
    ColorSpec (..),
    ThemeAccessor (..),
    applyUsings,
    accessWith,
    defaultedWith,
    bordersOrDefault,
  )
where

import Control.Applicative
import Control.Monad.Reader
import Data.Aeson
import qualified Data.HashMap.Internal as HM
import Data.Maybe
import Data.Text
import qualified Data.Vector as V
import GHC.Generics
import Themes.Color

data ColorSpec
  = Sets (HM.HashMap Text ColorSet)
  | Single ColorSet

pickSubSet :: Text -> ColorSpec -> Either String ColorSet
pickSubSet _ (Single c) = return c
pickSubSet s (Sets ms) = maybe (Left $ "subset " ++ show s ++ " doesn't exist") return $ HM.lookup s ms

instance FromJSON ColorSpec where
  parseJSON v = sets v <|> single v
    where
      single = fmap Single . parseJSON
      sets = fmap Sets . withObject "spec with subsets" (\kvpair -> HM.fromList <$> mapM parseWithKey (HM.toList kvpair))
      parseWithKey (k, v) = includeFst (k, parseJSON v)
      includeFst (a, fb) = (a,) <$> fb

data ThemeAccessor = Simple Text | WithSubSet (Text, Text) | Direct Color
  deriving (Show)

instance FromJSON ThemeAccessor where
  parseJSON v = color v <|> simple v <|> wsubset v
    where
      color = fmap Direct . parseJSON
      simple = withText "simple theme accessor" (return . Simple)
      wsubset = withArray "key tuple" $ \vec ->
        if V.length vec /= 2
          then fail "must have an array with two strings"
          else do
            a <- string' $ vec V.! 0
            b <- string' $ vec V.! 1
            return $ WithSubSet (a, b)

      string' = withText "string" return

data Borders = Borders
  { normal :: Maybe ThemeAccessor,
    focused :: Maybe ThemeAccessor
  }
  deriving (Generic, Show)

instance FromJSON Borders

data Theme = Theme
  { uses :: Maybe Text, -- brings into scope a particular subset. useful for changing fast from "light" to "dark" themes and so.
    text :: ThemeAccessor, -- normal text color.
    background :: ThemeAccessor, -- background color.
    hidden :: Maybe ThemeAccessor, -- color for hidden workspaces. defaultd to `text`.
    title :: Maybe ThemeAccessor, -- window title. defaulted to `text`.
    urgent :: Maybe ThemeAccessor, -- color for urgent workspaces. defaulted to `text`.
    focus :: ThemeAccessor, -- focus color.
    separators :: Maybe ThemeAccessor,
    borders :: Maybe Borders
  }
  deriving (Generic, Show)

instance FromJSON Theme

using :: Text -> ThemeAccessor -> ThemeAccessor
using k (Simple v) = WithSubSet (k, v)
using k (WithSubSet (_, v)) = WithSubSet (k, v)

usingBorders :: Text -> Borders -> Borders
usingBorders t (Borders normal focused) = Borders (using t <$> normal) (using t <$> focused)

applyUsings :: Theme -> Theme
applyUsings t@(Theme uses text bg hidden title urgent focus seps borders) = maybe t applyUses uses
  where
    applyUses k = Theme Nothing (using k text) (using k bg) (using k <$> hidden) (using k <$> title) (using k <$> urgent) (using k focus) (using k <$> seps) (usingBorders k <$> borders)

access :: ThemeAccessor -> ReaderT ColorSpec (Either String) Color
access (Direct c) = return c
access (Simple k) = do
  t <- ask
  case t of
    Single t -> lift $ selectColor k t
    Sets {} -> lift $ Left "an accessor is missing for the subset"
access (WithSubSet (k, v)) = do
  t <- ask
  let doLookup = selectColor v

  case t of
    Single t -> lift $ doLookup t
    Sets {} -> lift $ pickSubSet k t >>= doLookup

defaulted :: Color -> Maybe ThemeAccessor -> ReaderT ColorSpec (Either String) Color
defaulted c = maybe (return c) access

accessWith :: (a -> ColorSpec) -> ThemeAccessor -> ReaderT a (Either String) Color
accessWith f = withReaderT f . access

defaultedWith :: (a -> ColorSpec) -> Color -> Maybe ThemeAccessor -> ReaderT a (Either String) Color
defaultedWith f m = withReaderT f . defaulted m

bordersOrDefault :: Theme -> Borders
bordersOrDefault = fromMaybe (Borders Nothing Nothing) . borders
