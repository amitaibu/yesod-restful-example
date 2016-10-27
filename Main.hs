#!/usr/bin/env stack
{- stack
     --resolver lts-5.10
     --install-ghc
     runghc
     --package yesod
     --package persistent-sqlite
 -}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Either (lefts)
import Data.Text           (Text)
import Data.Time.Clock
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Network.HTTP.Types
import Yesod


data App = App ConnectionPool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Store
    name Text
    deriving Show Generic
Product
    name Text
    price Int
    store StoreId
    deriving Show Generic
|]


mkYesod "App" [parseRoutes|
/ StoreR GET
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance FromJSON Item
instance ToJSON Item


getStoreR :: StoreId -> Handler Value
getApiItemR storeId = do
    store <- runDB $ get404 storeId
    return $ toJSON store


openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        store <- insert $ Store "Store1"
        _ <- insert $ Product "Product1" 10 store
        _ <- insert $ Product "Product2" 50 store
        _ <- insert $ Product "Product3" 100 store
    warp 3000 $ App pool
