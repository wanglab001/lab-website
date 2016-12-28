{-# LANGUAGE DeriveGeneric #-}

module WangLab.Types
    ( Member(..)
    ) where

import GHC.Generics
import Data.Yaml
import Data.Time

data Member = Member
    { name :: String
    , role :: String
    , startYear :: Day
    , endYear :: Maybe Day
    , email :: String
    , photo :: Maybe FilePath
    , current :: Maybe String
    } deriving Generic

instance FromJSON Member
