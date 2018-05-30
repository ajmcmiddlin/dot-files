import           System.Taffybar

import           System.Taffybar.Battery (batteryBarNew, defaultBatteryConfig)
import           System.Taffybar.FreedesktopNotifications
import           System.Taffybar.MPRIS
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.TaffyPager
import           System.Taffybar.Weather

import           System.Taffybar.Widgets.PollingBar
import           System.Taffybar.Widgets.PollingGraph

import           System.Information.CPU
import           System.Information.Memory

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
      clock = textClockNew Nothing "<span fgcolor='white'>%a %d %b %Y %H:%M:%S UTC+10</span>" 1
      pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      -- wea = weatherNew (defaultWeatherConfig "KMSN") 10
      -- mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      batt = batteryBarNew defaultBatteryConfig 60.0
  in
    defaultTaffybar defaultTaffybarConfig { barHeight = 22
                                          , widgetSpacing = 10
                                          , startWidgets = [ pager, note ]
                                          , endWidgets = [ clock, tray, batt, mem, cpu ]
                                          }
