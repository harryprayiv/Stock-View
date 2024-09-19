module BudView where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Internal.RequestBody (class ToRequestBody)
import Fetch.Yoga.Json (fromJSON)
import Foreign (Foreign, ForeignError)
import Foreign.Index (readProp)
import JS.Fetch.RequestBody as RB
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, unsafeStringify, writeImpl)

data InventoryResponse
  = InventoryData Inventory
  | Message String

newtype ForeignRequestBody = ForeignRequestBody Foreign

newtype Inventory = Inventory (Array MenuItem)

newtype MenuItem = MenuItem
  { name :: String
  , category :: String
  , subcategory :: String
  , species :: String
  , sku :: String
  , price :: Number
  , quantity :: Int
  }

instance writeForeignMenuItem :: WriteForeign MenuItem where
  writeImpl (MenuItem item) = writeImpl item

instance readForeignMenuItem :: ReadForeign MenuItem where
  readImpl json = do
    name <- readProp "name" json >>= readImpl
    category <- readProp "category" json >>= readImpl
    subcategory <- readProp "subcategory" json >>= readImpl
    species <- readProp "species" json >>= readImpl
    sku <- readProp "sku" json >>= readImpl
    price <- readProp "price" json >>= readImpl
    quantity <- readProp "quantity" json >>= readImpl
    pure $ MenuItem { name, category, subcategory, species, sku, price, quantity }

instance ToRequestBody ForeignRequestBody where
  toRequestBody (ForeignRequestBody foreignValue) =
    RB.fromString (unsafeStringify foreignValue)

instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: ExceptT (NonEmptyList ForeignError) Identity (Array MenuItem)
    pure (Inventory items)

instance showMenuItem :: Show MenuItem where
  show (MenuItem item) =
    "MenuItem { name: " <> show item.name <>
    ", category: " <> show item.category <>
    ", subcategory: " <> show item.subcategory <>
    ", species: " <> show item.species <>
    ", sku: " <> show item.sku <>
    ", price: " <> show item.price <>
    ", quantity: " <> show item.quantity <> " }"

instance showInventory :: Show Inventory where
  show (Inventory items) = "Inventory " <> show items

instance showInventoryResponse :: Show InventoryResponse where
  show (InventoryData inventory) = "InventoryData " <> show inventory
  show (Message msg) = "Message " <> show msg

fetchInventoryFromJson :: Aff (Either String InventoryResponse)
fetchInventoryFromJson = do
  result <- attempt do
    timestamp <- liftEffect $ show <$> now
    let url = "/inventory.json?t=" <> timestamp
    liftEffect $ log ("Fetching URL: " <> url)
    coreResponse <- fetch url {}
    inventory <- fromJSON coreResponse.json :: Aff Inventory
    pure inventory
  case result of
    Left err -> pure $ Left $ "Fetch error: " <> show err
    Right inventory -> pure $ Right $ InventoryData inventory

fetchInventoryFromHttp :: Aff (Either String InventoryResponse)
fetchInventoryFromHttp = do
  result <- attempt do
    let requestHeaders = { "Content-Type": "application/json" }
    let requestBody = ForeignRequestBody (writeImpl { hello: "world" })
    coreResponse <- fetch "https://httpbin.org/post"
      { method: POST
      , body: requestBody
      , headers: requestHeaders
      }
    res <- fromJSON coreResponse.json :: Aff Foreign
    pure $ "Received response: " <> unsafeStringify res
  case result of
    Left err -> pure $ Left $ "Fetch error: " <> show err
    Right msg -> pure $ Right $ Message msg