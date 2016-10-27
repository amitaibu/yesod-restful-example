#!/usr/bin/env stack
{- stack
     --resolver lts-5.10
     --install-ghc
     runghc
     --package unordered-containers
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
import qualified Data.HashMap.Strict as HM
import Data.Text           (Text)
import Data.Time.Clock
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import GHC.Exts
import qualified Data.Vector as V
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
/#StoreId StoreR GET
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


instance ToJSON Store
instance ToJSON Product


getStoreR :: StoreId -> Handler Value
getStoreR storeId = do
    store <- runDB $ get404 storeId

    let storeJson = entityIdToJSON (Entity storeId store)
    let (Object storeHashMap) = storeJson

    products <- runDB $ selectList [ProductStore ==. storeId] [] :: Handler [Entity Product]
    let productsJson = [entityIdToJSON (Entity k r) | Entity k r <- products]

    let productsValue = Array (V.fromList productsJson)

    let storeWithProducts = HM.insert "products" productsValue storeHashMap
    return $ object ["data" .= storeWithProducts]

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        store <- insert $ Store "Store1"
        _ <- insert $ Product "Product1" 10 store
        _ <- insert $ Product "Product2" 50 store
        insert $ Product "Product3" 100 store
    warp 3000 $ App pool
