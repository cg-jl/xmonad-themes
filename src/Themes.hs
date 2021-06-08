{-# LANGUAGE OverloadedStrings #-}

module Themes
  ( colorString,
    colorText,
    defaultTheme,
    Theme (..),
    Borders (..),
    fetchTheme,
    fetchThemeWithCustomDir,
  )
where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bifunctor
import Data.ByteString as B
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO
import Themes.Color
import qualified Themes.Intermediates as I

data Borders = Borders {normal :: Color, focused :: Color}
  deriving (Show, Eq)

data Theme = Theme
  { text :: Color,
    background :: Color,
    focus :: Color,
    hidden :: Color,
    title :: Color,
    urgent :: Color,
    separators :: Color,
    borders :: Borders
  }
  deriving (Show, Eq)

fetchWithDefault :: Color -> (a -> Maybe I.ThemeAccessor) -> ReaderT (I.ColorSpec, a) (Either String) Color
fetchWithDefault def_col access_fn = asks (access_fn . snd) >>= I.defaultedWith fst def_col

fetch :: (a -> I.ThemeAccessor) -> ReaderT (I.ColorSpec, a) (Either String) Color
fetch access_fn = asks (access_fn . snd) >>= I.accessWith fst

convertBorders :: Color -> Color -> ReaderT (I.ColorSpec, I.Borders) (Either String) Borders
convertBorders def_normal def_focused = Borders <$> fetchWithDefault def_normal I.normal <*> fetchWithDefault def_focused I.focused

convertTheme :: ReaderT (I.ColorSpec, I.Theme) (Either String) Theme
convertTheme = local (second I.applyUsings) $ do
  text_col <- fetch I.text
  bg_col <- fetch I.background
  focus_col <- fetch I.focus
  hidden_col <- fetchWithDefault text_col I.hidden
  urgent_col <- fetchWithDefault text_col I.urgent
  title_col <- fetchWithDefault text_col I.title
  seps_col <- fetchWithDefault text_col I.separators

  borders <- withReaderT (second I.bordersOrDefault) $ convertBorders hidden_col text_col

  return $ Theme text_col bg_col focus_col hidden_col title_col urgent_col seps_col borders

instance FromJSON Theme where
  parseJSON = withObject "color theme" $ \v -> do
    theme <- v .: "theme"
    colors <- v .: "colors"
    let theme_result = runReaderT convertTheme (colors, theme)
    either fail return theme_result

getThemesDir :: IO FilePath
getThemesDir = do
  home <- getHomeDirectory
  return $ home </> ".xmonad" </> "themes"

fetchThemeWithCustomDir :: FilePath -> String -> IO (Either String Theme)
fetchThemeWithCustomDir themes_dir name = runExceptT $ do
  let theme_path = themes_dir </> name <.> "json"
  file_exists <- lift $ doesFileExist theme_path
  unless file_exists $ throwE $ "Sorry, but theme " ++ show name ++ " is not currently saved at " ++ show themes_dir
  ExceptT $ eitherDecodeFileStrict theme_path

fetchTheme :: String -> IO (Either String Theme)
fetchTheme name = getThemesDir >>= (`fetchThemeWithCustomDir` name)

defaultTheme :: Theme
defaultTheme =
  let cspec =
        I.Single $
          ColorSet
            { black = "#000000",
              blue = "#0000ff",
              cyan = "#00ffff",
              green = "#00ff00",
              magenta = "#ff00ff",
              red = "#ff0000",
              white = "#ffffff",
              yellow = "#ffff00"
            }
      tspec = I.Theme Nothing (I.Simple "black") (I.Simple "white") Nothing Nothing Nothing (I.Simple "white") Nothing Nothing
   in fromRight (error "default theme can't be parsed??") $
        runReaderT
          convertTheme
          ( cspec,
            tspec
          )
