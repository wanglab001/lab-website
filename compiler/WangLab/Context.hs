module WangLab.Context
    ( memberCtx
    , siteCtx
    ) where

import           Data.Maybe
import           Data.Monoid   ((<>))
import           Hakyll

import           WangLab.Types

memberCtx :: Context Member
memberCtx = field "name" (\item -> return $ name $ itemBody item) <>
    field "email" (\item -> return $ email $ itemBody item) <>
    field "photo" (\item -> return $ fromMaybe "" $ photo $ itemBody item)

siteCtx :: Context String
siteCtx =
    constField "baseurl" "" <>
    constField "site_description" "wanglab" <>
    constField "twitter_username" "wanglab" <>
    constField "github_username" "wanglab" <>
    constField "google_username" "wanglab" <>
    defaultContext
