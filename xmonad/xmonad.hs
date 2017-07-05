import qualified Data.Map                         as M
import           Data.Monoid                      ((<>))

import qualified Graphics.X11.Types               as XT
import           XMonad                           (Layout, X, XConfig (..),
                                                   defaultConfig, spawn, xmonad,
                                                   (.|.))
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks,
                                                   manageDocks)
import           XMonad.ManageHook                ((<+>))

import           System.Taffybar.Hooks.PagerHints (pagerHints)

myKeys :: XConfig Layout -> M.Map (XT.ButtonMask, XT.KeySym) (X ())
myKeys conf@(XConfig {modMask = modm, keys = defaultKeys}) =
  defaultKeys conf <> M.fromList
    [ ((modm .|. XT.shiftMask, XT.xK_Return), spawn $ terminal conf)
    , ((0, XT.xK_Print), spawn "maim -s ~/screenshots/$(date +%F_%T).png")
    ]

-- docks: add dock (panel) functionality to your configuration
-- ewmh: https://en.wikipedia.org/wiki/Extended_Window_Manager_Hints - lets XMonad talk to panels
-- pagerHints: add support for Taffybar's current layout and workspaces hints
main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ defaultConfig
  {
    terminal = "uxrvt"
    -- avoidStruts tells windows to avoid the "strut" where docks live
  , layoutHook = avoidStruts $ layoutHook defaultConfig
    -- let XMonad manage docks (taffybar)
  , manageHook = manageDocks <+> manageHook defaultConfig
--  , keys = myKeys
  }
