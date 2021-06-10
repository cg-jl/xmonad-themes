# xmonad-themes


A little library to manage custom themes using JSON.
The themes format is inspired by the way xmonad colors are customized, and the alacritty coloring format.
I decided to go like this as it will be easier to port the colors copying them directly from your alacritty config and then you only have
to specify which one goes where in the theme specification, as you'd do when doing it normally.

The point of this library is to have a very easy to use API and format so that changing the theme is just as easy as changing from

```hs
main :: IO ()
  theme_result <- fetchTheme "onedark"
  -- do stuff with theme_result
```

to 

```hs
main :: IO ()
  theme_result <- fetchTheme "gruvbox"
  -- do stuff with theme_result
```

## Table of contents

- [Usage](#Usage)
- [Xmonad example](#xmonad-example)
- [Theme format](#theme-format)


## Usage

This library is meant to be used along with a stack/cabal XMonad build, as you'll only need to add this library as a package.

Clone the library, inside your xmonad build:
```bash
git clone https://github.com/CyberGsus/xmonad-themes
```

Then, add it as a package to your `stack.yml`. (i.e, the `packages` key).

Last, execute a `stack build` to check everything's fine.



The library (`Themes`) exports a couple functions which you can use:

### Using the theme

```hs
colorString :: (a -> Color) -> a -> String
```
Useful to extract the color in string form from a theme, e.g

```hs
colorString background :: Theme -> String
```
as colors are internally represented by `Text`.
If you need the `Text` variant, use
```hs
colorText :: (a -> Color) -> a -> Text
colorText background :: Theme -> Text
```

### Fetching Themes

```hs
fetchTheme :: String -> IO (Either String Theme)
```

`fetchTheme name` will search in the default themes directory for `$HOME/.xmonad/themes/{name}.json` and try to parse it as a `Theme`. 
In case of the file not existing or a format error in the theme, the error will be reported through the `Left` variant.

```hs
fetchThemeWithCustomDir :: FilePath -> String -> IO (Either String Theme)
```
This one does the same as the last one except it uses a custom theme folder as the **first** argument.

## Xmonad Example

I managed to make my library work with XMonad, here's some of the code:

```hs
import           System.IO (hPutStrLn, stderr)
import           Themes

-- helper
putsErr :: String -> IO ()
putsErr = hPutStrLn stderr

themeName :: String
themeName = "nord" -- If you want the theme, go see it in my dotfiles under .xmonad/themes!

getTheme :: IO Theme
getTheme = do
  let logAndDefault err = do
        putsErr $ "XMonad (theme) : Couldn't load theme " ++ show themeName ++ ": " ++ err
        putsErr "XMonad (theme) : Resorting to default theme (ugly, duh)."
        return defaultTheme

  theme_res <- fetchTheme themeName
  either logAndDefault return theme_res


main :: IO ()
main = do
  theme <- getTheme
  let themeColor = (`colorString` theme)

  xmonad $ ewmh def {
          -- ...
          normalBorderColor = themeColor (normal . borders),
          focusedBorderColor = themeColor (focused . borders),
          logHook =
            workspaceHistoryHook
              <+> dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmobarPipe,
                    -- Current workspace in xmobar
                    ppCurrent = xmobarColor (themeColor Themes.focus) "" . mkWrap,
                    -- Visible but not current workspace
                    ppVisible = xmobarColor "#81a1c1" "" . mkWrap,
                    -- Hidden workspaces in xmobar
                    ppHidden = xmobarColor (themeColor hidden) "" . mkWrap,
                    -- Hidden workspaces (no windows)
                    ppHiddenNoWindows = xmobarColor (themeColor hidden) "" . mkWrap,
                    -- Title of active window in xmobar
                    ppTitle = xmobarColor (themeColor Themes.title) "" . shorten 55,
                    -- Separators in xmobar
                    ppSep = "<fc=" ++ themeColor separators ++ "> :: </fc>",
                    -- Urgent workspace
                   ppUrgent = xmobarColor (themeColor urgent) ""
                    -- ...
            }
    }
```
For the complete file, make sure to take a look at [my dotfiles](https://github.com/CyberGsus/dotfiles/blob/main/.xmonad)!

## Theme Format

The format of the themes is divided into two parts: the color specification (under `colors`) and the theme specification (under `theme`).

### Color specification

The color specification has to be one of these two:
- only one color set
- a dictionary with keys as names and color sets as values (e.g for `dark` and `light` theme variants).

The color set needs the following keys needed:

- `black`
- `blue`
- `cyan`
- `green`
- `magenta`
- `red`
- `white`
- `yellow`

where each value is an RGB hex color, that is, it should be accepted by the following regex:

```regex
/^#[0-9A-F]{6}$/i
```

or, in other words, exactly a pound `#` and 6 hexadecimal characters, lower or uppercase.

So these are valid examples of a color specification:

```json
{
  "black" : "#000000",
  "blue" : "#0000ff",
  "cyan" : "#00ffff",
  "green" : "#00ff00",
  "magenta" : "#ff00ff",
  "red" : "#ff0000",
  "white" : "#ffffff",
  "yellow" : "#ffff00"
}
```

```json
{
  "normal": {
    "black": "#000000",
    "blue": "#0000ff",
    "cyan": "#00ffff",
    "green": "#00ff00",
    "magenta": "#ff00ff",
    "red": "#ff0000",
    "white": "#ffffff",
    "yellow": "#ffff00"
  },
  "inverted": {
    "black": "#ffffff",
    "blue": "#ffff00",
    "cyan": "#ff0000",
    "green": "#ff00ff",
    "magenta": "#00ff00",
    "red": "#00ffff",
    "white": "#000000",
    "yellow": "#ffff00"
  }
}
```

### Theme specification

I will be mentioning the term *theme accessors* so I'll define it. A *theme accessor* is a value which has one of the two following structures:

- A string, meaning a key like `"black"` or `"white"`, useful when the color specification contains just one color set or when the `uses` key was specified (explained later).
- A string, with the structure of a color. Uses the color directly.
- An array consisting of a tuple with ONLY two values: `[subset, color_key]`, where `subset` is the subset name from the color specification and `color_key` is a string with the corresponding color name (`black`, `blue`, etc.).
  If there are no subsets as only one color set was specified, then the subset key is ignored. If the subset mentioned doesn't exist it will error out.

The theme specification has the following keys and values:

- `uses` : string, optional, brings a subset into scope for the entire specifiaction (e.g for switching from `dark` to `light`)
- `text` : theme accessor for the text color.
- `background` : theme accessor for the text background color (useful for bars like xmobar).
- `focus` : theme accessor. Used to focus the text for window borders/workspace titles.
- `hidden` : theme accessor, optional, used for hidden workspaces. Defaults to `text`.
- `title` : theme accessor, optional, used primarily for the window title. Defaults to `text`.
- `urgent` : theme accessor, optional, used for urgent workspaces. Defaults to `text`.
- `separators` : theme accessor, optional, defaulted to `text`.
- `borders` : optional, used for window borders. Specififed through the following keys:
  - `normal` : theme accessor, optional, defaults to `hidden`.
  - `focused` : theme accessor, optional, defaults to `focused`.
  If `borders` is not present, the default will have all of its keys with their default value.

As you can see, there are a bunch of keys, although you only have to specify three for this to work: `text`, `background` and `focus`.



This being said, the following themes are valid: 


```json
{
  "colors": {
    "bright": {
      "black": "#4c566a",
      "blue": "#81a1c1",
      "cyan": "#8fbcbb",
      "green": "#a3be8c",
      "magenta": "#b48ead",
      "red": "#bf616a",
      "white": "#eceff4",
      "yellow": "#ebcb8b"
    },
    "dim": {
      "black": "#373e4d",
      "blue": "#68809a",
      "cyan": "#6d96a5",
      "green": "#809575",
      "magenta": "#8c738c",
      "red": "#94545d",
      "white": "#aeb3bb",
      "yellow": "#b29e75"
    },
    "normal": {
      "black": "#3b4252",
      "blue": "#81a1c1",
      "cyan": "#88c0d0",
      "green": "#a3be8c",
      "magenta": "#b48ead",
      "red": "#bf616a",
      "white": "#e5e9f0",
      "yellow": "#ebcb8b"
    }
  },
  "theme": {
    "text": [
      "normal",
      "white"
    ],
    "hidden": [
      "dim",
      "black"
    ],
    "title": [
      "normal",
      "magenta"
    ],
    "urgent": [
      "bright",
      "red"
    ],
    "focus": [
      "normal",
      "cyan"
    ],
    "borders": {
      "normal": [
        "dim",
        "white"
      ],
      "focused": [
        "dim",
        "yellow"
      ]
    }
  }
}
```

```json
{
  "colors": {
    "bright": {
      "black": "#4c566a",
      "blue": "#81a1c1",
      "cyan": "#8fbcbb",
      "green": "#a3be8c",
      "magenta": "#b48ead",
      "red": "#bf616a",
      "white": "#eceff4",
      "yellow": "#ebcb8b"
    },
    "dim": {
      "black": "#373e4d",
      "blue": "#68809a",
      "cyan": "#6d96a5",
      "green": "#809575",
      "magenta": "#8c738c",
      "red": "#94545d",
      "white": "#aeb3bb",
      "yellow": "#b29e75"
    },
    "normal": {
      "black": "#3b4252",
      "blue": "#81a1c1",
      "cyan": "#88c0d0",
      "green": "#a3be8c",
      "magenta": "#b48ead",
      "red": "#bf616a",
      "white": "#e5e9f0",
      "yellow": "#ebcb8b"
    }
  },
  "theme": {
    "uses": "normal",
    "text": "white",
    "hidden": "black",
    "urgent": "red",
    "focus": "yellow",
    "borders": {
      "focused": "magenta"
    }
  }
}
```

```json
{
  "colors": {
    "black": "#3b4252",
    "blue": "#81a1c1",
    "cyan": "#88c0d0",
    "green": "#a3be8c",
    "magenta": "#b48ead",
    "red": "#bf616a",
    "white": "#e5e9f0",
    "yellow": "#ebcb8b"
  },
  "theme": {
    "text": "white",
    "hidden": "black",
    "urgent": "red",
    "focus": "yellow",
    "borders": {
      "focused": "magenta"
    }
  }
}
```

I discourage the use of these themes as legitimate color themes, use them as a template.
