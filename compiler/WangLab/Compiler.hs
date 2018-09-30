{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module WangLab.Compiler
    ( publicationCompiler
    , memberCompiler
    ) where

import           Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy            as BL
import           Data.Function                   (on)
import           Data.List
import           Data.List.Split                 (chunksOf, splitOn)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Time                       (defaultTimeLocale, formatTime,
                                                  toGregorian)
import           Data.Yaml
import           Hakyll
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as H
import           Text.Pandoc
import qualified Text.Pandoc.Builder             as P
import           Text.Printf                     (printf)

import           WangLab.Types

--------------------------------------------------------------------------------
-- Publication formatter
--------------------------------------------------------------------------------
publicationCompiler :: [Member] -> Compiler (Item String)
publicationCompiler members = pandocCompilerWithTransform defaultHakyllReaderOptions
    defaultHakyllWriterOptions format
  where
    --transform = id
    format (Pandoc meta blks) = Pandoc meta [
        Div ("", ["container"], []) $ evalState (mapM f blks) Nothing ]
      where
        f x@(Header _ _ [Str y]) = do
            put $ Just (read y)
            return x
        f xs = do
            y <- get
            return $ highlightMember (fromJust y) xs

    highlightMember year (Para xs) = Para $
        bolden (authors ++ [lastAuthor]) ++ [Str "."] ++ rest ++
        if null marks then [] else P.toList (fromString $ " (" ++ marks ++ ")")
      where
        coAuthorMark = if '#' `elem` (toString $ authors ++ [lastAuthor])
            then ["# equal contribution"]
            else []
        coCorresMark = if '*' `elem` (toString $ authors ++ [lastAuthor])
            then ["* corresponding authors"]
            else []
        marks = intercalate ", " $ coAuthorMark ++ coCorresMark

        (authors, lastAuthor:rest) = break isEnd xs
        bolden = concat . intersperse [Str ","] . map f . splitOn "," .
            init . toString
        f x = case M.lookup (getName x) memberYear of
            -- Not a member
            Nothing -> P.toList $ fromString x
            -- Current member
            Just Nothing -> [Strong $ P.toList $ fromString x]
            -- Alumni
            Just (Just y) -> if year > y + 1
                then P.toList $ fromString x
                else [Strong $ P.toList $ fromString x]
    highlightMember _ _ = undefined

    -- convert markup to string
    toString = foldl' (\acc x -> acc ++ g x) ""
      where
        g Space = " "
        g (Str s) = s
        g SoftBreak = ""
        g x = error $ show x
    memberYear = M.fromList $
        map (\x -> (name x, fst' . toGregorian <$> endYear x)) members
    fst' (x,_,_) = x
    isEnd (Str x) = length x > 2 && last x == '.'
    isEnd _ = False
    getName x = head ws ++ " " ++ last ws
      where
        ws = words $ dropWhileEnd (`elem` ['*', '#']) x

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
        Right r ->
            let (alumni, present) = partition (isJust . endYear) r
                members = M.fromList $ map (\x -> (role $ head x, x)) $
                    groupBy ((==) `on` role) $ sortBy (comparing role) present
                pi = case M.lookup "PI" members of
                    Nothing -> mempty
                    Just xs -> do
                        H.h2 "Principal Investigator"
                        membersToHTML xs
                scientist = case M.lookup "Scientist" members of
                    Nothing -> mempty
                    Just xs -> do
                        H.h2 "Project Scientists"
                        membersToHTML xs
                postdoc = case M.lookup "Postdoc" members of
                    Nothing -> mempty
                    Just xs -> do
                        H.h2 "Postdoctoral researchers"
                        membersToHTML xs
                graduate | null elems = mempty
                         | otherwise = H.h2 "Graduate Students" >> membersToHTML elems
                  where
                    elems = M.findWithDefault [] "PhD" members ++
                        M.findWithDefault [] "Master" members
                alumni' = do
                    H.h2 "Alumni"
                    H.ul $ mapM_ (H.li . H.string) $ prettyPrintAlumi alumni

            in H.div H.! H.class_ "container" $ pi >> scientist >> postdoc >>
                graduate >> alumni'

    membersToHTML xs = H.div H.! H.class_ "card-deck-wrapper" $
        mapM_ ((H.div H.! H.class_ "card-deck") . mapM_ f) $ chunksOf 3 xs
      where
        f x = H.div H.! H.class_ "card text-xs-center" $ do
            H.img H.! H.class_ "card-img-top img-fluid rounded"
                H.! H.src (fromString $ fromMaybe "" $ photo x)
            H.div H.! H.class_ "card-block" $ do
                H.h4 H.! H.class_ "card-title" $ H.toHtml $ name x
                H.p H.! H.class_ "card-text" $ H.toHtml $ "Email: " ++ email x

        {-
    f (Item a b) = return $ case decodeEither (BL.toStrict b) of
        Left msg -> error msg
        Right r -> let (alumni, present) = partition (isJust . endYear) r
                   in M.fromList $ map (\x -> (role $ head x, map (Item a) x)) $
                        groupBy ((==) `on` role) $ sortBy (comparing role) present
                        -}

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
