{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid                    ( mappend )
import           Hakyll

main :: IO ()
main = hakyll $ do
  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler

  match ("about.markdown" .||. "contact.markdown") $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtx <> authorCtx <> siteCtx)
      >>= relativizeUrls

  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" (postCtx <> authorCtx) (return posts)
              <> constField "title" "Home"
              <> siteCtx
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match ("includes/*" .||. "templates/*") $ compile templateCompiler


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

siteCtx :: Context String
siteCtx = constField "site_title" "Test" <> constField "site_logo" "../assets/img/icosaedron.svg" <> constField "site_description" "Test" <> constField "site_cover" "../assets/img/geometric.jpg" <> defaultContext

authorCtx :: Context String
authorCtx = constField "author" "Vincibean" <> constField "author_image" "../assets/img/Vincibean.jpeg" <> constField "author_bio" ""