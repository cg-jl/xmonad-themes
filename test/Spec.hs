{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Test.Hspec hiding (focus)
import Themes
import qualified Themes.Intermediates as I

main :: IO ()
main = hspec $ do
  describe "color theme" $ do
    it "file which doesn't exist" $ do
      theme_res <- fetchTheme "lelelel"
      theme_res `shouldSatisfy` isLeft

    it "theme" $ do
      theme <- fetchTheme "test"
      theme
        `shouldBe` Right
          ( Theme
              { text = "#e5e9f0",
                Themes.focus = "#88c0d0",
                hidden = "#373e4d",
                title = "#b48ead",
                urgent = "#bf616a",
                background = "#443352",
                separators = "#e5e9f0",
                borders =
                  Borders
                    { normal = "#aeb3bb",
                      focused = "#b29e75"
                    }
              }
          )

    it "with usings" $ do
      theme_res <- fetchTheme "test2"
      theme_res
        `shouldBe` Right
          ( Theme
              { background = "#3b4252",
                text = "#e5e9f0",
                separators = "#e5e9f0",
                focus = "#ebcb8b",
                hidden = "#3b4252",
                title = "#e5e9f0",
                urgent = "#bf616a",
                borders = Borders {normal = "#3b4252", focused = "#b48ead"}
              }
          )
