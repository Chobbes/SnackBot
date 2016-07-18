{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where

import SnackBot.SnackState
import SnackBot.Parser
import SnackBot.Commands

import System.Environment
import System.Random

import Web.Slack
import Web.Slack.Message

import Data.Acid
import Data.Attoparsec.Text
import Data.Text hiding (length)
import Control.Monad.IO.Class
import Control.Lens


main :: IO ()
main =
  do [configFile] <- getArgs
     config <- readConfig configFile
     acid <- openLocalState (SnackState [] [] [])
     runBot config snackBot acid


readConfig :: FilePath -> IO SlackConfig
readConfig file =
  do apiKey <- readFile file
     return $ SlackConfig apiKey


snackBot :: SlackBot (AcidState SnackState)
snackBot (Message cid _ msg _ _ _) =
  case parseOnly parseCommands msg of
    Left _ -> return ()
    Right cmd -> runCommand cid cmd

snackBot _ = return ()
