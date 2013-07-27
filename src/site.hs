--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options as Pandoc.Options

--------------------------------------------------------------------------------

pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                        { Pandoc.Options.writerHtml5 = True
                        , Pandoc.Options.writerHtmlQTags = True
                        --, Pandoc.Options.writerNumberSections = True
                        --, Pandoc.Options.writerNumberOffset = [1]
                        , Pandoc.Options.writerSectionDivs = True
                        , Pandoc.Options.writerTableOfContents = True
                    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- put all the images in /images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- copy site icon to `favicon.ico`
    match "images/favicon.ico" $ do
        route   (constRoute "favicon.ico")
        compile copyFileCompiler

    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

--    match "css/*" $ do
--        route   idRoute
--        compile compressCssCompiler
        --    getResourceString
        --        >>= withItemBody (unixFilter "sass" ["-s", "--scss"])

    match "scss/*" $do
        route $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "--style", "compressed"])
            >>= return . fmap compressCss

    match (fromList ["index.markdown"
                    ,"about.markdown"
                    ,"contact.markdown"]
          ) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/post.html"
                    (
                    constField "title"      title `mappend`
                    constField "body"       ""    `mappend`
                    constField "date"       ""    `mappend`
                    constField "prettytags" ""    `mappend`
                    constField "posts"      ""    `mappend`
                    defaultContext
                    )
                >>= loadAndApplyTemplate "templates/default.html"
                    (
                    constField "title" title `mappend`
                    defaultContext
                    )
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField      "posts" postCtx (return posts) `mappend`
                    constField     "title" "Archives"             `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    postCtx

--------------------------------------------------------------------------------
