import Data.Time


-- timeConversion :: String -> String
timeConversion dateString =   let timeFormatIn = "%H:%M:%S%p"
                                  timeFormatOut = "%H:%M:%S"
                                  timeFromString = parseTimeOrError True defaultTimeLocale timeFormatIn dateString :: UTCTime
                              in formatTime defaultTimeLocale timeFormatOut timeFromString 


-- 07:05:45PM

-- let dateString = "26 Jan 2012 10:54 AM"
