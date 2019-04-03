{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.Theme.Light
import Data.IORef
import Data.Time.Clock.System(getSystemTime, SystemTime(..))
import Foreign
import Data.Text as T

offsetClock :: Rectangle -> Maybe T.Text -> Second -> IO (Ref Clock)
offsetClock rect l (Second offset) = do
  timerRef <- newIORef Nothing
  let kickoff :: Ref Clock -> IO ()
      kickoff clockRef = do
        (MkSystemTime ss nss) <- getSystemTime
        setValue clockRef (ClockSetSinceEpoch (ClockSinceEpoch (Second ((fromIntegral ss) + offset))))
        fp <- FL.addTimeout (schedule (fromIntegral nss / 1000)) (kickoff clockRef)
        writeIORef timerRef (Just fp)
      schedule micros =
        ((1000000 - micros) -- microseconds to next second
          + 25000) -- add 25 msec for timer inaccuracy https://www.fltk.org/str.php?L3516+P0+S-2+C0+I0+E0+V%25+Q3516.
         / 1000000
      cleanup = do
        fpMaybe <- readIORef timerRef
        case fpMaybe of
          Just fp -> do
            FL.removeTimeout fp
            freeHaskellFunPtr fp
            writeIORef timerRef Nothing
          Nothing -> return ()
  clockCustom rect l (Just drawClock)
    (Just
       (defaultCustomWidgetFuncs
         {
           handleCustom =
             (Just (\clockRef event ->
                       case event of
                         FL.Show -> Right<$>kickoff clockRef
                         FL.Hide -> Right<$>cleanup
                         _ -> handleClockBase (safeCast clockRef) event)),
           destroyCallbacksCustom =
             (Just (\clockRef callbacks -> cleanup >> defaultDestroyWidgetCallbacks clockRef callbacks))
         }))

ui :: (?assets :: Assets) => IO ()
ui = do
  window <- windowNew
            (Size (Width 600) (Height 400))
            Nothing
            Nothing
  begin window
  _ <- offsetClock (Rectangle (Position (X 100) (Y 0)) (Size (Width 400) (Height 400))) Nothing (Second (-(60 * 60)))
  end window
  showWidget window

main :: IO ()
main = do
  assets <- configureTheme
  let ?assets = assets
  ui
  FL.run
  FL.flush

replMain :: IO ()
replMain = do
  assets <- configureTheme
  let ?assets = assets
  ui
  FL.replRun
