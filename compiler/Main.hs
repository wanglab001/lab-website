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
    Just members <- decodeFile "web/contents/people.yml"

    hakyll $ do
        match "web/static/*/*" $ do
            route $ gsubRoute "web/" (const "")
            compile copyFileCompiler

        match "web/static/*/*/*" $ do
            route $ gsubRoute "web/" (const "")
            compile copyFileCompiler

        match "web/templates/*" $ compile templateCompiler

        match (fromList ["web/contents/positions.md"]) $ do
            route $ composeRoutes (gsubRoute "web/contents/" $ const "") $
                setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "web/templates/default-markdown.html" siteCtx
                >>= loadAndApplyTemplate "web/templates/default.html" siteCtx
                >>= relativizeUrls

        match "web/contents/publications.md" $ do
            route $ composeRoutes (gsubRoute "web/contents/" $ const "") $
                setExtension "html"
            compile $ publicationCompiler members
                >>= loadAndApplyTemplate "web/templates/default.html" siteCtx
                >>= relativizeUrls

        match "web/contents/people.yml" $ do
            route $ composeRoutes (gsubRoute "web/contents/" $ const "") $
                setExtension "html"
            compile $ memberCompiler
                >>= loadAndApplyTemplate "web/templates/default.html" siteCtx
                >>= relativizeUrls

        match "web/contents/*.html" $ do
            route $ composeRoutes (gsubRoute "web/contents/" $ const "") idRoute
            compile $ getResourceBody
                >>= applyAsTemplate siteCtx
                >>= loadAndApplyTemplate "web/templates/default.html" siteCtx
                >>= relativizeUrls
