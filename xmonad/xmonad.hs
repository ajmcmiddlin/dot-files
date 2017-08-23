import qualified Data.Map                         as M
import           Data.Monoid                      ((<>))

import qualified Graphics.X11.Types               as XT
import           XMonad                           (Layout, ManageHook, X,
                                                   XConfig (..), def, spawn,
                                                   windows, xmonad, (.|.))
import           XMonad.Actions.SpawnOn           (manageSpawn, spawnOn)
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks,
                                                   manageDocks)
import           XMonad.ManageHook                (className, composeAll,
                                                   doShift, (-->), (<+>), (=?))

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
                   , ((modm .|. XT.shiftMask, XT.xK_p), spawn "~/bin/passmenu")
                   ]
  in kees <> keys def conf

myWorkspaces :: [String]
myWorkspaces =
  [ "1:Web"
  , "2:Work"
  , "3:WorkBrowser"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8:Mail"
  , "9:Chat"
  ]

-- Class name can be found with `xprop | grep WM_CLASS`
myManageHook :: ManageHook
myManageHook =
  composeAll [ className =? "Emacs" --> doShift "2:Work"
             , className =? "Thunderbird" --> doShift "8:Mail"
             -- Below is the class name for Signal (launched via Chrome)
             , className =? "crx_bikioccmkafdpakkkcpdbppfkghcmihk" --> doShift "9:Chat"
             , className =? "Keepassx" --> doShift "9:Chat"
             ]

-- docks: add dock (panel) functionality to your configuration
-- ewmh: https://en.wikipedia.org/wiki/Extended_Window_Manager_Hints - lets XMonad talk to panels
-- pagerHints: add support for Taffybar's current layout and workspaces hints
main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ def
  {
   keys = myKeys
    -- avoidStruts tells windows to avoid the "strut" where docks live
  , layoutHook = avoidStruts $ layoutHook def
    -- let XMonad manage docks (taffybar)
  , manageHook = myManageHook <+> manageDocks <+> manageHook def
  , terminal = "urxvt"
  , workspaces = myWorkspaces
  }
