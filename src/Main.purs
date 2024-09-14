module Main where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Array (filter, intercalate, sortBy)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Ref (new, write)
import Effect.Timer (setInterval, clearInterval)
import Fetch (Method(..), fetch)
import Fetch.Internal.RequestBody (class ToRequestBody)
import Fetch.Yoga.Json (fromJSON)
import Foreign (Foreign, ForeignError)
import Foreign.Index (readProp)
import JS.Fetch.RequestBody as RB
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element, setAttribute)
import Web.DOM.Element as EL
import Web.DOM.Node (appendChild, firstChild, ownerDocument, removeChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body, documentElement, setTitle, toDocument)
import Web.HTML.HTMLElement as HEL
import Web.HTML.HTMLHtmlElement as HHEL
import Web.HTML.Window (document)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, unsafeStringify, writeImpl)


-- Define the unified result type
data InventoryResponse
  = InventoryData Inventory
  | Message String

newtype ForeignRequestBody = ForeignRequestBody Foreign

-- Define Inventory as a newtype
newtype Inventory = Inventory (Array MenuItem)

-- Define MenuItem as a newtype over a record
newtype MenuItem = MenuItem
  { name :: String
  , sku :: String
  , price :: Number
  , quantity :: Int
  }

-- WriteForeign instance for MenuItem
instance writeForeignMenuItem :: WriteForeign MenuItem where
  writeImpl (MenuItem item) = writeImpl item

-- ReadForeign instance for MenuItem
instance readForeignMenuItem :: ReadForeign MenuItem where
  readImpl json = do
    name <- readProp "name" json >>= readImpl
    sku <- readProp "sku" json >>= readImpl
    price <- readProp "price" json >>= readImpl
    quantity <- readProp "quantity" json >>= readImpl
    pure $ MenuItem { name, sku, price, quantity }

-- ToRequestBody instance for ForeignRequestBody
instance ToRequestBody ForeignRequestBody where
  toRequestBody (ForeignRequestBody foreignValue) =
    RB.fromString (unsafeStringify foreignValue)

-- ReadForeign instance for Inventory
instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: ExceptT (NonEmptyList ForeignError) Identity (Array MenuItem)
    pure (Inventory items)

-- Show instance for MenuItem
instance showMenuItem :: Show MenuItem where
  show (MenuItem item) =
    "MenuItem { name: " <> show item.name <>
    ", sku: " <> show item.sku <>
    ", price: " <> show item.price <>
    ", quantity: " <> show item.quantity <> " }"

-- Show instance for Inventory
instance showInventory :: Show Inventory where
  show (Inventory items) = "Inventory " <> show items

-- Show instance for InventoryResponse
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

-- Sorting and configuration types
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
compareMenuItems config (MenuItem item1) (MenuItem item2) =
  let
    baseComparison = case config.sortField of
      SortByName -> compare item1.name item2.name
      SortByPrice -> compare item1.price item2.price
      SortByQuantity -> compare item1.quantity item2.quantity
  in
    case config.sortOrder of
      Ascending -> baseComparison
      Descending -> invertOrdering baseComparison

-- Helper function to clear an element's content
clearElement :: Element -> Effect Unit
clearElement element = do
  let node = EL.toNode element
  let removeChildren = do
        maybeChild <- firstChild node
        case maybeChild of
          Nothing -> pure unit
          Just child -> do
            _ <- removeChild node child
            removeChildren
  removeChildren

-- Helper function to append an item element
appendItemElement :: Element -> MenuItem -> Effect Unit
appendItemElement parent (MenuItem { name, price, quantity }) = do
  maybeDoc <- ownerDocument (EL.toNode parent)
  case maybeDoc of
    Just doc -> do
      itemElement <- createElement "div" doc
      let text = name <> " | $" <> show price <> " | x" <> show quantity
      setTextContent text (EL.toNode itemElement)
      appendChild (EL.toNode itemElement) (EL.toNode parent)
    Nothing -> log "Error: Could not get ownerDocument of parent element."

-- Rendering inventory
-- renderInventory :: Config -> Inventory -> Element -> Effect Unit
-- renderInventory config (Inventory items) parent = do
--   log "Rendering inventory"
--   clearElement parent

--   let filteredItems = if config.hideOutOfStock
--         then filter (\(MenuItem item) -> item.quantity > 0) items
--         else items

--   let sortedItems = sortBy (compareMenuItems config) filteredItems

--   for_ sortedItems (appendItemElement parent)

renderInventory :: Config -> Inventory -> String
renderInventory config (Inventory items) =
  let
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items

    sortedItems = sortBy (compareMenuItems config) filteredItems
    itemStrings = map (\(MenuItem { name, price, quantity }) ->
      name <> " | $" <> show price <> " | x" <> show quantity) sortedItems
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

app :: Effect Unit
app = do
  log "App started"
  doc <- window >>= document
  bod <- body doc
  case bod of
    Nothing -> log "No body element found"
    Just bodyElement -> do
      let document = toDocument doc
      div <- createElement "div" document
      setAttribute "id" "app" div
      appendChild (EL.toNode div) (HEL.toNode bodyElement)
      log "Appended div to body"

      let config =
            { sortField: SortByPrice
            , sortOrder: Descending
            , hideOutOfStock: true
            }

      currentInventory <- new (Inventory [])
      unsubscribe <- new (pure unit)

      let
        mode = "json" -- Switch between "json" and "http" mode
        renderInventoryEvent = do
          log "Starting renderInventoryEvent"
          launchAff_ do
            liftEffect $ log "Inside launchAff_"
            result <- case mode of
              "json" -> fetchInventoryFromJson
              "http" -> fetchInventoryFromHttp
              _ -> pure $ Left "Invalid mode"

            liftEffect $ case result of
              Left err -> do
                log ("Error loading inventory: " <> err)
                clearElement div
                setTextContent ("Error loading inventory: " <> err) (EL.toNode div)
              Right res -> do
                log ("Fetched data: " <> show res)
                case res of
                  InventoryData inventory -> do
                    write inventory currentInventory
                    setTextContent (renderInventory config inventory) (EL.toNode div)
                  Message msg -> do
                    clearElement div
                    setTextContent msg (EL.toNode div)

      -- Call renderInventoryEvent immediately
      renderInventoryEvent

      -- Set up the interval
      i <- setInterval 3000 renderInventoryEvent
      write (clearInterval i) unsubscribe

  pure unit

-- Entry point for the application
main :: Effect Unit
main = do
  log "Starting main"
  app