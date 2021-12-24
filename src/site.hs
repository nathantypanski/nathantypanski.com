{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, (<>))
import Hakyll
import System.Process
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Monad (foldM)
import Text.Pandoc as Pandoc
import Text.Pandoc.Options as Pandoc.Options
import Text.Pandoc.Templates as Pandoc.Templates

pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
    { -- Pandoc.Options.writerHtml5 = True
    Pandoc.Options.writerHtmlQTags = True
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
      myExtensions = extensionsFromList
           [
           Ext_tex_math_dollars
         , Ext_tex_math_double_backslash
         , Ext_superscript
         , Ext_yaml_metadata_block
         , Ext_latex_macros
           ]
      defaultExtensions = readerExtensions defaultHakyllReaderOptions
      ext = defaultExtensions `mappend` myExtensions

tocTemplate :: Pandoc.Templates.Template Text
tocTemplate = either error id $ either (error . show) id $
    Pandoc.runPure $ Pandoc.runWithDefaultPartials $
    Pandoc.compileTemplate "" "$if(toc)$<div id=\"toc\"><h2>Contents</h2>\n$toc$ </div>\n$endif$$body$"

tocWriterOptions :: Pandoc.Options.WriterOptions
tocWriterOptions = pandocWriterOptions
    { writerTableOfContents = True
    , writerTemplate = Just tocTemplate
    , writerHTMLMathMethod = MathJax ""
    }

main :: IO ()
main = hakyllWith config $ do
    tags <- buildTags ("blog/*" .||. "pages/*") (fromCapture "tags/*.html")
    modifications <- buildModifications "**"

    tagsRules tags $ \tag pattern -> do
        let title = "tag: " ++ tag
        route idRoute
        compile $ do
            posts <- loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" (postCtx modifications <> tagsCtx tags) (return posts)
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
        let context = postCtx modifications <> tagsCtx tags <> defaultCtx
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
                    listField      "posts" (postCtx modifications) (return posts) `mappend`
                    constField     "title" "Blog" `mappend`
                    defaultCtx

            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtx modifications) `mappend` bodyField "description"
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
                    listField "posts" (postCtx modifications) (return posts) `mappend`
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
    metadata <- getMetadataField (itemIdentifier item) "math"
    return $ case metadata of
        Just "true" -> "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({jax: [\"input/TeX\", \"output/HTML-CSS\"], TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\"]}})</script><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>"
        _  -> ""

tagsCtx :: Tags -> Context String
tagsCtx = tagsField "tagLinks"

postCtx :: [(Identifier, String)] -> Context String
postCtx times =
    modificationCtx times
    <> dateField "date" "%B %e, %Y"
    <> defaultCtx

buildModifications ::  Pattern -> Rules [(Identifier, String)]
buildModifications pattern = do
    ids <- getMatches pattern
    pairs <- preprocess $ foldM getLastModified [] ids
    return pairs
    where
        getLastModified l id' = do
            t <- readProcess "git"
                ["log", "-1", "--format=%ad", "--date=format:%b %d, %Y", "--", (toFilePath id')] ""
            return $ (id', t) : l

modificationCtx :: [(Identifier, String)] -> Context String
modificationCtx modificationTimes = field "modified" $ \item -> do
    let time = find (\x -> (fst x) == (itemIdentifier item)) modificationTimes >>= return . snd 
    return $ fromMaybe "no recent modifications" $ time

defaultCtx :: Context String
defaultCtx = defaultContext <> mathCtx
