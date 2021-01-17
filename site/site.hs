{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid                    ( mappend )
import           Hakyll

main :: IO ()
main = hakyll $ do
  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= relativizeUrls

  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
  match "includes/*" $ compile templateCompiler


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

