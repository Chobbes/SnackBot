{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SnackBot.SnackState where

import Data.Acid
import Data.SafeCopy
import Data.Text
import Data.Vector
import Data.Typeable
import Control.Lens
import Control.Monad.Reader (asks)


data SnackState =
  SnackState { _lunchList :: [Text]
             , _snackList :: [Text]
             , _recipeList :: [Text]
             }
  deriving (Typeable)


$(deriveSafeCopy 0 'base ''SnackState)
makeLenses ''SnackState


insertPlace :: Text -> Update SnackState [Text]
insertPlace place = lunchList <%= (place:)


lookupPlaces :: Query SnackState [Text]
lookupPlaces = asks _lunchList


insertSnack :: Text -> Update SnackState [Text]
insertSnack place = snackList <%= (place:)


lookupSnacks :: Query SnackState [Text]
lookupSnacks = asks _snackList


$(makeAcidic ''SnackState ['insertPlace, 'lookupPlaces, 'insertSnack, 'lookupSnacks])

