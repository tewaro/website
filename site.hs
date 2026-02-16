--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Pandoc.Biblio
import           System.FilePath ((</>))
import           Debug.Trace
import           Data.Char
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    tags <- buildTags (fromRegex "posts/*|drafts/*") (fromCapture "tags/*.html" . (fmap toLower))

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
        compile $ do 
            let tagsCtx = tagsFieldWith getTags renderLink mconcat "tags" tags
            (pandocBiblioCompiler "csl/ieee.csl" "bib/biblio.bib")
                >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx <> postCtx)
                >>= loadAndApplyTemplate "templates/default.html" (tagsCtx <> postCtx)
                >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ do
            let tagsCtx = tagsFieldWith getTags renderLink mconcat "tags" tags
            (pandocBiblioCompiler "csl/ieee.csl" "bib/biblio.bib")
                >>= loadAndApplyTemplate "templates/draft.html"  (tagsCtx <> postCtx)
                >>= loadAndApplyTemplate "templates/default.html" (tagsCtx <> postCtx)
                >>= relativizeUrls

    tagsRules tags $ \tagStr tagsPattern -> do
        route idRoute
        compile $ do
            posts <- loadAll tagsPattern >>= recentFirst
            let postsCtx = constField "title" tagStr
                    <> listField "posts" postCtx (return posts)
                    <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
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

renderLink _ Nothing = Nothing
renderLink tag (Just url) = Just $
    H.li $
      H.a
        ! ( A.href (H.toValue ("/" <> url))
              <> A.title ("Navigate posts by tag: " <> H.stringValue tag))
        $ do
          H.span ! A.class_ "fa fa-tag" $ " "
          " "
          H.toHtml tag

