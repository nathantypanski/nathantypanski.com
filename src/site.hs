{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, (<>))
import Hakyll
import Text.Pandoc.Options as Pandoc.Options
import Data.Set (insert)
import Data.Maybe (isJust)
import Data.Map (lookup)

pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
    { Pandoc.Options.writerHtml5 = True
    , Pandoc.Options.writerHtmlQTags = True
    , Pandoc.Options.writerReferenceLinks = True
    , Pandoc.Options.writerSectionDivs = True
    , writerHTMLMathMethod = MathJax ""
    }

pandocReaderOptions :: Pandoc.Options.ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions
    {
     readerExtensions = ext
    }
    where
      myExtensions =
           [
           Ext_tex_math_dollars
         , Ext_superscript
         , Ext_yaml_metadata_block
           ]
      defaultExtensions = readerExtensions defaultHakyllReaderOptions
      ext = foldr insert defaultExtensions myExtensions

tocWriterOptions :: Pandoc.Options.WriterOptions
tocWriterOptions = pandocWriterOptions
    { writerTableOfContents = True
    , writerTemplate =
        "$if(toc)$<div id=\"toc\"><h2>Contents</h2>\n$toc$ </div>\n$endif$$body$"
    , writerStandalone = True
    , writerHTMLMathMethod = MathJax ""
    }

main :: IO ()
main = hakyllWith config $ do
    tags <- buildTags ("blog/*" .||. "pages/*") (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "tag: " ++ tag
        route idRoute
        compile $ do
            posts <- loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" (postCtx <> tagsCtx tags) (return posts)
                      <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- put all the images in /images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- copy site icon to `favicon.ico`
    match "images/favicon.ico" $ do
        route   (constRoute "favicon.ico")
        compile copyFileCompiler

    -- route the fonts
    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    -- route my extra files
    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- javascript
    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- route the css
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- robots
    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    -- compile the scss and put it in _site/css/
    match (fromRegex "scss/[a-zA-Z].*s?css") $ do
        route $ gsubRoute "scss/" (const "css/") `composeRoutes`
            setExtension "css"
        compile $ getResourceString
            >>= withItemBody
                ( unixFilter "sass"
                    [ "-s"
                    , "--scss"
                    , "--style"
                    , "compressed"]
                )
            >>= return . fmap compressCss

    -- make top-level pages
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes`
            setExtension "html"
        let context = tagsCtx tags <> defaultCtx
        compile $ pandocCompilerWith pandocReaderOptions tocWriterOptions
            >>= loadAndApplyTemplate "templates/essay.html" context
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= relativizeUrls

    match "blog/*" $ do
        route $ setExtension "html"
        let context = postCtx <> tagsCtx tags <> defaultCtx
        compile $
            pandocCompilerWith pandocReaderOptions tocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html" context
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let archiveCtx =
                    constField     "rssblog" "" `mappend`
                    listField      "posts" postCtx (return posts) `mappend`
                    constField     "title" "Blog" `mappend`
                    defaultCtx

            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "blog/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

    create ["404.html"] $ do
        route idRoute
        compile $ do
            let notFoundCtx =
                    constField "mathjaxScript" "" <>
                    constField "title" "404 you're lost" `mappend`
                    defaultCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/default.html" notFoundCtx

    match "pages/index.html" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

config :: Configuration
config = defaultConfiguration
    {   deployCommand = "rsync --delete-before --checksum -ave 'ssh' _site/* "
                        ++ "athen@ephesus:/srv/http/nathantypanski.com"
      , inMemoryCache = True
      , previewPort = 8080
    }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Nathan's Blog"
    , feedDescription = "Linux and software development"
    , feedAuthorName  = "Nathan Typanski"
    , feedAuthorEmail = "ndt@nathantypanski.com"
    , feedRoot        = "http://www.nathantypanski.com"
    }

-- Originally found on John Lenz's blog:
-- http://blog.wuzzeb.org/posts/2012-06-08-hakyll-and-latex.html
-- and adapted to work with Hakyll 4
mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ case Data.Map.lookup "math" metadata of
        Just "true" -> "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
        otherwise   -> ""

tagsCtx :: Tags -> Context String
tagsCtx = tagsField "tagLinks"

postCtx :: Context String
postCtx =
    modificationTimeField "modified" "%B %e, %Y" <>
    dateField "date" "%B %e, %Y" <>
    defaultCtx

defaultCtx :: Context String
defaultCtx = defaultContext <> mathCtx
