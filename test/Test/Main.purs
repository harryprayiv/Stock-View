module Test.Main where

import Prelude

import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (new, write)
import Effect.Timer (setInterval, clearInterval)

import Fetch (Method(..), fetch)
import Fetch.Internal.RequestBody (class ToRequestBody)
import Fetch.Yoga.Json (fromJSON)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, unsafeStringify, writeImpl)

import JS.Fetch.RequestBody as RB
import Foreign (Foreign)
import Foreign.Index (readProp)

import Web.DOM.Document (createElement)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.Element (setAttribute)

import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body, documentElement, setTitle, toDocument)
import Web.HTML.Window (document)
import Web.DOM.Element as EL
import Web.HTML.HTMLElement as HEL
import Web.HTML.HTMLHtmlElement as HHEL

-- Define the unified result type
data InventoryResponse
  = InventoryData Inventory
  | Message String

newtype ForeignRequestBody = ForeignRequestBody Foreign

-- Define Inventory as a newtype
newtype Inventory = Inventory (Array MenuItem)

data MenuItem = MenuItem
  { name :: String
  , sku :: String
  , price :: Number
  , quantity :: Int
  }

instance WriteForeign MenuItem where
  writeImpl (MenuItem { name, sku, price, quantity }) = writeImpl
    { name, sku, price, quantity }

instance ReadForeign MenuItem where
  readImpl json = do
    name <- readImpl =<< readProp "name" json
    sku <- readImpl =<< readProp "sku" json
    price <- readImpl =<< readProp "price" json
    quantity <- readImpl =<< readProp "quantity" json
    pure $ MenuItem { name, sku, price, quantity }

-- Define ReadForeign instance for Inventory
instance ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json
    pure (Inventory items)

instance ToRequestBody ForeignRequestBody where
  toRequestBody (ForeignRequestBody foreignValue) =
    RB.fromString (unsafeStringify foreignValue)

-- Fetch the inventory from a local JSON file using Fetch and fromJSON
fetchInventoryFromJson :: Aff (Either String InventoryResponse)
fetchInventoryFromJson = do
  result <- attempt do
    coreResponse <- fetch "/inventory.json" {}
    fromJSON coreResponse.json :: Aff Inventory
  case result of
    Left err -> pure $ Left $ "Fetch error: " <> show err
    Right inventory -> pure $ Right $ InventoryData inventory

-- Fetch inventory data via HTTP POST request using Fetch and fromJSON
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

-- Helper function to render a MenuItem as a string
renderItem :: MenuItem -> String
renderItem (MenuItem { name, price, quantity }) =
  "Item: " <> name <> ", Price: $" <> show price <> ", Quantity: " <> show quantity

-- Define sorting configuration types
data SortField = SortByName | SortByPrice | SortByQuantity

data SortOrder = Ascending | Descending

type Config =
  { sortField :: SortField
  , sortOrder :: SortOrder
  , hideOutOfStock :: Boolean
  }

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

compareMenuItems :: Config -> MenuItem -> MenuItem -> Ordering
compareMenuItems { sortField, sortOrder } (MenuItem item1) (MenuItem item2) =
  let
    baseComparison = case sortField of
      SortByName -> compare item1.name item2.name
      SortByPrice -> compare item1.price item2.price
      SortByQuantity -> compare item1.quantity item2.quantity
  in
    case sortOrder of
      Ascending -> baseComparison
      Descending -> invertOrdering baseComparison

-- Adjust renderInventory function
renderInventory :: Config -> Inventory -> String
renderInventory config (Inventory items) =
  let
    -- Filter out items with quantity zero if hideOutOfStock is true
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items

    -- Sort the filtered items
    sortedItems = sortBy (compareMenuItems config) filteredItems
    itemStrings = map renderItem sortedItems
  in
    intercalate "\n" itemStrings

-- Corrected showBodyError function
showBodyError :: HTMLDocument -> Effect Unit
showBodyError doc = do
  setTitle "Could not load document." doc
  div <- createElement "div" (toDocument doc)
  setTextContent "Could not load document." (EL.toNode div)
  rootMaybe <- documentElement doc
  case rootMaybe of
    Just root -> do
      _ <- appendChild (EL.toNode div) (HHEL.toNode root)
      pure unit
    Nothing -> log "No root element found; cannot append error message."

-- Main app function with mode switch handling
app :: Effect Unit
app = do
  log "App started"
  doc <- window >>= document
  bod <- body doc
  case bod of
    Nothing -> do
      log "No body element found"
      showBodyError doc
    Just bodyElement -> do
      log "Body element found"
      let document = toDocument doc
      div <- createElement "div" document
      setAttribute "id" "app" div
      appendChild (EL.toNode div) (HEL.toNode bodyElement)
      log "Appended div to body"

      -- Configuration for sorting and hiding out-of-stock items
      let config =
            { sortField: SortByPrice
            , sortOrder: Descending
            , hideOutOfStock: true -- Set to false to show items with quantity zero
            }

      currentInventory <- new (Inventory [])
      unsubscribe <- new (pure unit)

      let
        mode = "json" -- Switch between "json" and "http" mode
        renderInventoryEvent = do
          log "Starting renderInventoryEvent"
          launchAff_ do
            result <- case mode of
              "json" -> fetchInventoryFromJson
              "http" -> fetchInventoryFromHttp
              _ -> pure $ Left "Invalid mode"

            case result of
              Left err -> do
                log ("Error loading inventory: " <> err)
                liftEffect $ setTextContent ("Error loading inventory: " <> err) (EL.toNode div)
              Right res -> liftEffect $ case res of
                InventoryData inventory -> do
                  write inventory currentInventory
                  setTextContent (renderInventory config inventory) (EL.toNode div)
                Message msg ->
                  setTextContent msg (EL.toNode div)

        inventoryEvent = do
          renderInventoryEvent
          i <- setInterval 3000 renderInventoryEvent
          pure do
            clearInterval i

      u <- inventoryEvent
      write u unsubscribe

  pure unit

-- Entry point for the application
main :: Effect Unit
main = do
  log "Starting main"
  app
