{-# LANGUAGE OverloadedStrings #-}
module SnackBot.Commands where

import SnackBot.SnackState

import Web.Slack
import Web.Slack.Message

import Data.Acid
import Data.Text as T hiding (length, map)
import Control.Lens
import Control.Monad.IO.Class
import System.Random


data Command
  = LunchSuggest
  | AddLunchPlace Text
  | ListLunchPlaces
  | AddSnack Text
  | ListSnacks
  | AddRecipe Text
  | BotSnack
  | HighVoltage
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

runCommand cid (AddSnack snack) =
  do acid <- use userState
     liftIO $ update acid (InsertSnack snack)
     sendMessage cid (T.concat ["\"", snack, "\" has been added to the list of requested snacks!"])

runCommand cid ListLunchPlaces =
  do acid <- use userState
     lunchPlaces <- liftIO $ query acid LookupPlaces
     if lunchPlaces == []
       then sendMessage cid "Oh no! No lunch places! Add one with !addplace"
       else sendMessage cid (T.concat ["Here are the lunch places: ", T.intercalate ", " (map quote lunchPlaces)])

runCommand cid ListSnacks =
  do acid <- use userState
     snacks <- liftIO $ query acid LookupSnacks
     if snacks == []
       then sendMessage cid "Oh no! No snacks requested! Add one with !addsnack"
       else sendMessage cid (T.concat ["Here are the currently requested snacks: ", T.intercalate ", " (map quote snacks)])

runCommand cid Help =
  sendMessage cid (T.intercalate ", " (map quote commands))
  where commands = ["!help", "!suggest", "!addplace", "!addsnack", "!listplaces", "!listsnacks", "!botsnack", "!hv"]

runCommand cid BotSnack =
  sendMessage cid ":)"

runCommand cid HighVoltage =
  sendMessage cid "High Voltage"

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
