--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Pandoc.Biblio
import           System.FilePath ((</>))
import           Debug.Trace


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "bib/*.bib" $ compile biblioCompiler

    match "csl/*.csl" $ compile cslCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ (pandocBiblioCompiler "csl/ieee.csl" "bib/biblio.bib")
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ (pandocBiblioCompiler "csl/ieee.csl" "bib/biblio.bib")
            >>= loadAndApplyTemplate "templates/draft.html"  postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            drafts <- recentFirst =<< loadAll "drafts/*"
            let archiveCtx =
                    listField "drafts" postCtx (return drafts) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            drafts <- recentFirst =<< loadAll "drafts/*"
            let indexCtx =
                    listField "drafts" postCtx (return drafts) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    modificationTimeField "updated" "%0Y-%m-%d %H:%M UTC" `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
