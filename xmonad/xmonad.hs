import qualified Data.Map                         as M
import           Data.Monoid                      ((<>))

import qualified Graphics.X11.Types               as XT
import           XMonad                           (Layout, X, XConfig (..),
                                                   def, spawn, xmonad,
                                                   (.|.))
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks,
                                                   manageDocks)
import           XMonad.ManageHook                ((<+>))

import           System.Taffybar.Hooks.PagerHints (pagerHints)

myKeys :: XConfig Layout -> M.Map (XT.ButtonMask, XT.KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  let xK_X86MonBrightnessDown = 0x1008ff03
      xK_X86MonBrightnessUp   = 0x1008ff02
      xK_X86AudioLowerVolume  = 0x1008ff11
      xK_X86AudioRaiseVolume  = 0x1008ff13
      xK_X86AudioMute         = 0x1008ff12
      kees =
        M.fromList [ ((0, XT.xK_Print), spawn "maim -c 1,0,0,0.6 -s ~/screenshots/$(date +%F_%T).png")
                   , ((modm, XT.xK_Print), spawn "maim -c 1,0,0,0.6 -s /dev/stdout | xclip -selection clipboard -t image/png -i")
                   , ((XT.controlMask .|. XT.mod1Mask, XT.xK_l), spawn "xscreensaver-command -lock")
                   , ((0, xK_X86MonBrightnessDown), spawn "xbacklight -dec 5")
                   , ((0, xK_X86MonBrightnessUp), spawn "xbacklight -inc 5")
                   , ((0, xK_X86AudioLowerVolume), spawn "amixer sset Master 5%-")
                   , ((0, xK_X86AudioRaiseVolume), spawn "amixer sset Master 5%+")
                   , ((0, xK_X86AudioMute), spawn "amixer sset Master toggle")
                   , ((XT.controlMask .|. XT.mod1Mask, XT.xK_q), spawn "~/.screenlayout/laptop-only.sh")
                   , ((XT.controlMask .|. XT.mod1Mask, XT.xK_w), spawn "~/.screenlayout/the-fort.sh")
                   ]
  in kees <> keys def conf

-- docks: add dock (panel) functionality to your configuration
-- ewmh: https://en.wikipedia.org/wiki/Extended_Window_Manager_Hints - lets XMonad talk to panels
-- pagerHints: add support for Taffybar's current layout and workspaces hints
main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ def
  {
    terminal = "urxvt"
    -- avoidStruts tells windows to avoid the "strut" where docks live
  , layoutHook = avoidStruts $ layoutHook def
    -- let XMonad manage docks (taffybar)
  , manageHook = manageDocks <+> manageHook def
  , keys = myKeys
  }
