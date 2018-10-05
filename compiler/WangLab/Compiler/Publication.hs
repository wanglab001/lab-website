{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module WangLab.Compiler.Publication
    ( publicationCompiler
    ) where

import           Control.Monad.State.Lazy
import           Data.List
import           Data.List.Split (splitOn)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.String
import           Data.Time                       (toGregorian)
import           Hakyll
import           Text.Pandoc
import qualified Text.Pandoc.Builder             as P

import           WangLab.Types

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