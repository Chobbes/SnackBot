{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SnackBot.SnackState where

import Data.Acid
import Data.SafeCopy
import Data.Text
import Data.List (delete)
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


removePlace :: Text -> Update SnackState [Text]
removePlace place = lunchList <%= (delete place)


lookupPlaces :: Query SnackState [Text]
lookupPlaces = asks _lunchList


insertSnack :: Text -> Update SnackState [Text]
insertSnack snack = snackList <%= (snack:)


removeSnack :: Text -> Update SnackState [Text]
removeSnack snack = snackList <%= (delete snack)


lookupSnacks :: Query SnackState [Text]
lookupSnacks = asks _snackList


$(makeAcidic ''SnackState [ 'insertPlace
                          , 'removePlace
                          , 'lookupPlaces
                          , 'insertSnack
                          , 'removeSnack
                          , 'lookupSnacks])

