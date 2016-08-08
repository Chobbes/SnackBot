{-# LANGUAGE OverloadedStrings #-}

module SnackBot.Parser where

import SnackBot.Commands

import Data.Attoparsec.Text
import Control.Applicative


parseCommands :: Parser Command
parseCommands = suggest <|> addPlace <|> removePlace <|> addSnack <|> removeSnack <|> listPlaces <|> listSnacks <|> botSnack <|> help <|> highVoltage <|> delayedHello


suggest :: Parser Command
suggest = string "!suggest" *> endOfInput *> return LunchSuggest


addPlace :: Parser Command
addPlace = AddLunchPlace <$> (string "!addplace" *> skipMany1 space *> takeText)


removePlace :: Parser Command
removePlace = RemovePlaceCommand <$> (string "!rmplace" *> skipMany1 space *> takeText)


addSnack :: Parser Command
addSnack = AddSnack <$> (string "!addsnack" *> skipMany1 space *> takeText)


removeSnack :: Parser Command
removeSnack = RemoveSnackCommand <$> (string "!rmsnack" *> skipMany1 space *> takeText)


listPlaces:: Parser Command
listPlaces = string "!listplaces" *> endOfInput *> return ListLunchPlaces


listSnacks :: Parser Command
listSnacks = string "!listsnacks" *> endOfInput *> return ListSnacks


botSnack :: Parser Command
botSnack = string "!botsnack" *> endOfInput *> return BotSnack


highVoltage :: Parser Command
highVoltage = string "!hv" *> endOfInput *> return HighVoltage


help :: Parser Command
help = string "!help" *> endOfInput *> return Help


delayedHello :: Parser Command
delayedHello = string "!delayed" *> return DelayedHello
