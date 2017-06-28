import           XMonad                           (defaultConfig, xmonad)
import           XMonad.Core                      (XConfig (..))
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks)

import           System.Taffybar.Hooks.PagerHints (pagerHints)

-- docks: add dock (panel) functionality to your configuration
-- ewmh: https://en.wikipedia.org/wiki/Extended_Window_Manager_Hints - lets XMonad talk to panels
-- pagerHints: add support for Taffybar's current layout and workspaces hints
main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ defaultConfig
  {
    terminal = "uxrvt"
    -- avoidStruts tells windows to avoid the "strut" where docks live
  , layoutHook = avoidStruts $ layoutHook defaultConfig
  }
