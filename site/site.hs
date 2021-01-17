{-# LANGUAGE OverloadedStrings #-}

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
      >>= loadAndApplyTemplate "templates/page.html" context
      >>= relativizeUrls
  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" context
      >>= relativizeUrls
  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" context (return posts)
              <> constField "title" "Home"
              <> context
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match ("includes/*" .||. "templates/*") $ compile templateCompiler

context :: Context String
context = authorCtx <> postCtx <> siteCtx

authorCtx :: Context String
authorCtx =
  constField "author" "Vincibean"
    <> constField "author_image" "../assets/img/Vincibean.jpeg"
    <> constField "author_bio"   ""
    <> defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

siteCtx :: Context String
siteCtx =
  constField "site_title" "Vincibean"
    <> constField "site_logo"        "../assets/img/icosaedron.svg"
    <> constField "site_description" "Just a Bunch of Tips"
    <> constField "site_cover"       "../assets/img/geometric.jpg"
    <> constField "page_url"         "https://vincibean.github.io"
    <> defaultContext
