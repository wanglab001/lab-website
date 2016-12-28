{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import Data.Yaml (decodeFile)

import           WangLab.Compiler
import           WangLab.Context  (memberCtx, postCtx, siteCtx)


--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Read lab members
    Just members <- decodeFile "contents/people.yml"

    hakyll $ do
        match "static/*/*" $ do
            route idRoute
            compile copyFileCompiler

        match "static/*/*/*" $ do
            route idRoute
            compile copyFileCompiler

        match "templates/*" $ compile templateCompiler

        match (fromList ["contents/positions.md"]) $ do
            route $ composeRoutes (gsubRoute "contents/" $ const "") $
                setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default-markdown.html" siteCtx
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

        match "contents/publications.md" $ do
            route $ composeRoutes (gsubRoute "contents/" $ const "") $
                setExtension "html"
            compile $ publicationCompiler members
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

        match "contents/people.yml" $ do
            route $ composeRoutes (gsubRoute "contents/" $ const "") $
                setExtension "html"
            compile $ memberCompiler
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

        match "contents/*.html" $ do
            route $ composeRoutes (gsubRoute "contents/" $ const "") idRoute
            compile $ getResourceBody
                >>= applyAsTemplate siteCtx
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls
