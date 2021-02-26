--{
-- xmonad.hs
-- Josue Teodoro Moreira
-- January 05, 2021
--}

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

personalLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 4 $ Tall nmaster delta ratio

    nmaster = 1
    delta   = (3/100)
    ratio   = (1/2)

main = do
  xmonad $ docks desktopConfig
    { modMask = mod4Mask
    , terminal = "gnome-terminal"

    , focusFollowsMouse = False
    
    , normalBorderColor  = "#A5A58D"
    , focusedBorderColor = "#264653"
    , borderWidth        = 2

    , workspaces = ["Web", "Terminal", "Code", "Chat", "Music"] ++ map show [6 .. 9]

    , startupHook = do
          startupHook desktopConfig <+> spawn "nitrogen --restore"
          startupHook desktopConfig <+> spawn "xmobar &"
          startupHook desktopConfig <+> spawn "setxkbmap dvorak"
          startupHook desktopConfig <+> spawn "light-locker --lock-on-lid --lock-on-suspend"
    
    , layoutHook = avoidStruts personalLayoutHook
    } `additionalKeys`
    [ ((mod4Mask, xK_Return), spawn "gnome-terminal")

    , ((mod4Mask, xK_r), spawn "rofi -show combi")

      -- screen layout
    , ((mod4Mask, xK_space), sendMessage NextLayout)

      -- focus table
    , ((mod4Mask, xK_h), windows W.focusDown)
    , ((mod4Mask, xK_s), windows W.focusUp)

      -- resize table
    , ((mod4Mask .|. shiftMask, xK_h), sendMessage Shrink)
    , ((mod4Mask .|. shiftMask, xK_s), sendMessage Expand)

      -- print
    , ((0, xK_Print), spawn "scrot -s")

      -- keyboard layout
    , ((mod4Mask, xK_d), spawn "setxkbmap dvorak")
    , ((mod4Mask, xK_b), spawn "setxkbmap br dvorak")
    ]
