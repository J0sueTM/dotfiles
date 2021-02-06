--{
-- xmonad.hs
-- Josue Teodoro Moreira
-- January 05, 2021
--}

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig

-- preferable over defaultConfig
baseConfig = desktopConfig

main = xmonad baseConfig {
  -- meta key
  modMask            = mod4Mask

  -- borders
  , normalBorderColor  = "#A5A58D"
  , focusedBorderColor = "#264653"

  -- personal hooks
  , startupHook = startupHook baseConfig <+> spawn "setxkbmap dvorak && xmobar &"
  }
