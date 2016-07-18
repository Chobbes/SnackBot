{-# LANGUAGE OverloadedStrings #-}

module SnackBot.Parser where

import SnackBot.Commands

import Data.Attoparsec.Text
import Control.Applicative


parseCommands :: Parser Command
parseCommands = suggest <|> addPlace <|> addSnack <|> listPlaces <|> listSnacks <|> botSnack <|> help <|> highVoltage


suggest :: Parser Command
suggest = string "!suggest" *> endOfInput *> return LunchSuggest


addPlace :: Parser Command
addPlace = AddLunchPlace <$> (string "!addplace" *> skipMany1 space *> takeText)


addSnack :: Parser Command
addSnack = AddSnack <$> (string "!addsnack" *> skipMany1 space *> takeText)


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
