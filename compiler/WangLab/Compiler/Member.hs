{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module WangLab.Compiler.Member
    ( memberCompiler
    ) where

import           Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy            as BL
import           Data.Function                   (on)
import           Data.List
import           Data.List.Split (splitOn)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Time                       (defaultTimeLocale, formatTime,
                                                  toGregorian)
import           Data.Yaml
import           Hakyll
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H hiding (style)
import qualified Text.Blaze.Html5.Attributes     as H
import           Text.Pandoc
import qualified Text.Pandoc.Builder             as P
import           Text.Printf                     (printf)

import           WangLab.Types

--------------------------------------------------------------------------------
-- Group member compiler
--------------------------------------------------------------------------------
memberCompiler :: Compiler (Item String)
memberCompiler = getResourceLBS >>= (\(Item i content) ->
    return $ Item i $ renderHtml $ readMembers content)
  where
    readMembers :: BL.ByteString -> H.Html
    readMembers x = case decodeEither (BL.toStrict x) of
        Left msg -> error msg
        Right r -> do
            let (alumni, present) = partition (isJust . endYear) r
            H.div H.! H.class_ "container" $ do
                printMembers ["PI"] "Principal Investigator" present
                printMembers ["Scientist"] "Project Scientists" present
                printMembers ["Postdoc"] "Postdoctoral researchers" present
                printMembers ["PhD", "Master"] "Graduate Students" present
                H.h2 "Alumni"
                H.ul $ mapM_ (H.li . H.string) $ prettyPrintAlumi alumni

printMembers :: [String]    -- ^ Key
             -> H.Html -- ^ Renamed Key
             -> [Member]
             -> H.Html
printMembers key key' members = case filter ((`elem` key) . role) members of
    [] -> mempty
    xs -> do
        H.h2 key'
        membersToHTML xs
  where
    membersToHTML xs = H.div H.! H.class_ "card-deck-wrapper" $
        H.div H.! H.class_ "row" $ mapM_ f xs
      where
        f x = H.div H.! H.class_ "col-md-6 col-sm-6 col-lg-4 col-xl-3" $
            H.div H.! H.class_ "card text-center" H.! H.style "width: 260px" $ do
                H.img H.! H.class_ "card-img-top img-fluid rounded" H.! 
                    H.src (fromString $ fromMaybe "" $ photo x)
                H.div H.! H.class_ "card-body" $ do
                    H.h4 H.! H.class_ "card-title" $ H.toHtml $ name x
                    H.p H.! H.class_ "card-text" $ H.toHtml $ "Email: " ++ email x

prettyPrintAlumi :: [Member] -> [String]
prettyPrintAlumi = map f . sortBy (flip (comparing (endYear . last))) .
    map (sortBy (comparing startYear)) .
    groupBy ((==) `on` name) . sortBy (comparing name)
  where
    f xs = printf "%s (%s): %s" nm titles now
      where
        nm = name $ head xs
        titles = intercalate "; " $ map ( \x -> role x ++ ", " ++
            formatTime defaultTimeLocale "%Y/%m" (startYear x) ++ " - " ++
            formatTime defaultTimeLocale "%Y/%m" (fromJust $ endYear x) ) xs
        now = fromMaybe "" $ current $ last xs
