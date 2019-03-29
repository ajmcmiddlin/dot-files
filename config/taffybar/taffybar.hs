{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Taffybar
import           System.Taffybar.SimpleConfig                    (barHeight, defaultSimpleTaffyConfig,
                                                                  endWidgets,
                                                                  simpleTaffybar,
                                                                  startWidgets,
                                                                  widgetSpacing)

-- TODO: add this back in with taffybar2
-- import           System.Taffybar.DBus.Toggle              (handleDBusToggles)
import           System.Taffybar.Widget.FreedesktopNotifications (defaultNotificationConfig,
                                                                  notifyAreaNew)
import           System.Taffybar.Widget.MPRIS2                   (mpris2New)
import           System.Taffybar.Widget.SimpleClock
-- import           System.Taffybar.TaffyPager
import           System.Taffybar.Widget.Weather

import           System.Taffybar.Widget.Generic.PollingBar
import           System.Taffybar.Widget.Generic.PollingGraph

import           System.Taffybar.Information.CPU                 (cpuLoad)
import           System.Taffybar.Information.Memory
import           System.Taffybar.Widget.Battery                  (batteryIconNew)
import           System.Taffybar.Widget.SNITray                  (sniTrayNew)
import           System.Taffybar.Widget.Workspaces               (defaultWorkspacesConfig,
                                                                  workspacesNew)

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main =
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      clock = textClockNew Nothing "<span size='large' fgcolor='white'>%a %d %b %Y %H:%M:%S UTC+10</span>" 1
      -- pager = taffyPagerNew defaultPagerConfig
      workspaces = workspacesNew defaultWorkspacesConfig
      note = notifyAreaNew defaultNotificationConfig
      -- wea = weatherNew (defaultWeatherConfig "KMSN") 10
      mpris2 = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = sniTrayNew
      batt = batteryIconNew
  in
    -- TODO: add this back in with taffybar2
    -- defaultTaffybar . handleDBusToggle $
    simpleTaffybar
      defaultSimpleTaffyConfig { -- barHeight = 17
                            widgetSpacing = 10
                            , startWidgets = [ workspaces, note ]
                            , endWidgets = [ clock, tray, batt, mem, cpu, mpris2 ]
                            }
