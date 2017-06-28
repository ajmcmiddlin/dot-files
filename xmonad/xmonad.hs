import           XMonad                           (defaultConfig, xmonad)
import           XMonad.Core                      (XConfig (..))
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks)

import           System.Taffybar.Hooks.PagerHints (pagerHints)

main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ defaultConfig
  {
    terminal = "uxrvt"
  , layoutHook = avoidStruts $ layoutHook defaultConfig
  }
