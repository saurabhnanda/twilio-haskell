{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Capability
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Types.Capability where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

type Capabilities = Set Capability

data Capability
  = Voice
  | SMS
  | MMS
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance {-# OVERLAPPING #-} FromJSON Capabilities where
  parseJSON (Object map) 
    = let map' = fmap (\value -> case value of
                        Bool bool     -> bool
                        _             -> False) map
      in  return $ foldr (\capability set ->
            if fromMaybe False (KeyMap.lookup (Key.fromText $ T.pack $ show capability) map')
              then Set.insert capability set
              else set
          ) Set.empty [Voice, SMS, MMS]
  parseJSON _ = mzero
