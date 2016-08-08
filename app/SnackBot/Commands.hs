{-# LANGUAGE OverloadedStrings #-}
module SnackBot.Commands where

import SnackBot.SnackState

import Web.Slack
import Web.Slack.Message

import Data.Acid
import qualified Data.Text as T
import Data.Text (Text, pack)
import Control.Lens
import Control.Monad.IO.Class
import Control.Concurrent.MonadIO
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import System.Random


instance HasFork (Slack s) where
  fork slack = Slack . fork $ runSlack slack
                                      

instance HasFork m => HasFork (StateT s m) where
  fork state = StateT $ \initial ->
                         do thread <- fork $ (evalStateT state initial)
                            return (thread, initial)


data Command
  = LunchSuggest
  | AddLunchPlace Text
  | RemovePlaceCommand Text
  | ListLunchPlaces
  | AddSnack Text
  | RemoveSnackCommand Text
  | ListSnacks
  | AddRecipe Text
  | BotSnack
  | HighVoltage
  | DelayedHello
  | Help


runCommand :: ChannelId -> Command -> Slack (AcidState SnackState) ()
runCommand cid LunchSuggest =
  do acid <- use userState
     lunchPlaces <- liftIO $ query acid LookupPlaces
     maybePlace <- liftIO $ pickRandom lunchPlaces

     case maybePlace of
       Nothing -> sendMessage cid "No places exist!"
       Just place -> sendMessage cid place

runCommand cid (AddLunchPlace place) =
  do acid <- use userState
     liftIO $ update acid (InsertPlace place)
     sendMessage cid (T.concat ["\"", place, "\" has been added to the list of lunch places!"])

runCommand cid (RemovePlaceCommand place) =
  do acid <- use userState
     lunchPlaces <- liftIO $ query acid LookupPlaces
     if place `elem` lunchPlaces
       then if place == "High Voltage"
               then sendMessage cid "I can't let you do that."
               else do liftIO $ update acid (RemovePlace place)
                       sendMessage cid (T.concat ["I have forgotten ", "\"", place, ".\"", " Let us never speak of this again."])
       else sendMessage cid (T.concat ["\"", place, "\" isn't a place that I know about."])

runCommand cid (AddSnack snack) =
  do acid <- use userState
     liftIO $ update acid (InsertSnack snack)
     sendMessage cid (T.concat ["\"", snack, "\" has been added to the list of requested snacks!"])

runCommand cid (RemoveSnackCommand snack) =
  do acid <- use userState
     snacks <- liftIO $ query acid LookupSnacks
     if snack `elem` snacks
       then do liftIO $ update acid (RemoveSnack snack)
               sendMessage cid (T.concat ["I have forgotten ", "\"", snack, ".\"", " "])
       else sendMessage cid (T.concat ["I have never heard of this \"", snack, "!\""])

runCommand cid ListLunchPlaces =
  do acid <- use userState
     lunchPlaces <- liftIO $ query acid LookupPlaces
     if lunchPlaces == []
       then sendMessage cid "Oh no! No lunch places! Add one with !addplace"
       else sendMessage cid (T.concat ("Here are the lunch places:" : zipWith showPlace [1..] lunchPlaces))
       where showPlace i p = T.concat ["\n    ", (pack (show i)), ". ", p]

runCommand cid ListSnacks =
  do acid <- use userState
     snacks <- liftIO $ query acid LookupSnacks
     if snacks == []
       then sendMessage cid "Oh no! No snacks requested! Add one with !addsnack"
       else sendMessage cid (T.concat ["Here are the currently requested snacks: ", T.intercalate ", " (map quote snacks)])

runCommand cid Help =
  sendMessage cid (T.intercalate ", " (map quote commands))
  where commands = ["!help", "!suggest", "!addplace", "!rmplace", "!addsnack", "!rmsnack", "!listplaces", "!listsnacks", "!botsnack", "!hv"]

runCommand cid BotSnack =
  sendMessage cid ":)"

runCommand cid HighVoltage =
  sendMessage cid "High Voltage"

runCommand cid DelayedHello = sendDelayed cid 5000 "Hello!"

runCommand cid _ = return ()



pickRandom :: [a] -> IO (Maybe a)
pickRandom xs = do i <- randomRIO (0, length xs - 1)
                   return $ xs !? i


(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = return $ xs !! i


quote :: Text -> Text
quote text = T.concat ["\"", text, "\""]


sendDelayed :: ChannelId -> Int -> Text -> Slack s ()
sendDelayed cid delay message =
  do fork $ do threadDelay (delay * 1000)
               sendMessage cid message
     return ()
