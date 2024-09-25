module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), fetchInventoryFromJson, fetchInventoryFromHttp)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut(..), text_)
import Deku.DOM as D
import Deku.DOM.Attributes (klass_)
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, error, killFiber, launchAff, never)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FRP.Event.Time (interval')

-- Sorting Configuration
data SortField = SortByName | SortByCategory | SortBySubCategory | SortByPrice | SortByQuantity
data SortOrder = Ascending | Descending

type Config =
  { sortField :: SortField
  , sortOrder :: SortOrder
  , hideOutOfStock :: Boolean
  , screens :: Int
  }

-- Sort helper functions
invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

compareMenuItems :: Config -> MenuItem -> MenuItem -> Ordering
compareMenuItems config (MenuItem item1) (MenuItem item2) =
  let
    baseComparison = case config.sortField of
      SortByName -> compare item1.name item2.name
      SortByCategory -> compare item1.category item2.category
      SortBySubCategory -> compare item1.subcategory item2.subcategory
      SortByPrice -> compare item1.price item2.price
      SortByQuantity -> compare item1.quantity item2.quantity
  in
    case config.sortOrder of
      Ascending -> baseComparison
      Descending -> invertOrdering baseComparison

-- Rendering functions using Deku
renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) = D.div
  [ klass_ "inventory" ]
  (map renderItem sortedItems)
  where
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items
    sortedItems = sortBy (compareMenuItems config) filteredItems

renderItem :: MenuItem -> Nut
renderItem (MenuItem item) = D.div
  [ klass_ "inventory-item" ]
  [ D.div [] [ text_ ("Name: " <> item.name)]
  , D.div [] [ text_ ("Category: " <> item.category) ]
  , D.div [] [ text_ ("Subcategory: " <> item.subcategory) ]
  , D.div [] [ text_ ("Species: " <> item.species) ]
  , D.div [] [ text_ ("SKU: " <> item.sku) ]
  , D.div [] [ text_ ("Price: $" <> show item.price) ]
  , D.div [] [ text_ ("Quantity: " <> show item.quantity) ]
  ]

-- Custom hook to fetch inventory and update state in Aff context
fetchInventoryAff :: (Inventory -> Effect Unit) -> String -> Aff Unit
fetchInventoryAff setInventory mode = do
  liftEffect $ log "Fetching inventory..."
  result <- case mode of
    "json" -> fetchInventoryFromJson
    "http" -> fetchInventoryFromHttp
    _ -> pure $ Left "Invalid mode"
  
  case result of
    Left err -> liftEffect $ log ("Error fetching inventory: " <> err)
    Right (InventoryData inv) -> liftEffect $ setInventory inv
    _ -> pure unit

app :: Effect Unit
app = do
  -- Create a reference to store the fiber so it can be killed if necessary
  initialFiber <- launchAff never  -- Create a never-ending fiber to use as initial value
  fiberRef <- Ref.new initialFiber

  runInBody Deku.do
    -- Initialize inventory state
    setInventory /\ inventory <- useState (Inventory [])

    let config =
          { sortField: SortByCategory
          , sortOrder: Ascending
          , hideOutOfStock: true
          , screens: 1
          }

    -- Set up polling with `interval'` to fetch inventory every 3000ms
    unsubscribe <- interval' (withInventoryPoll fiberRef setInventory) 3000

    -- Render inventory when it changes
    renderInventory config inventory

    -- Return a function to clean up the interval when the component is destroyed
    pure $ do
      log "Cleaning up polling"
      unsubscribe

  where
  -- This function handles the polling and fiber management for fetching inventory
  withInventoryPoll :: Ref.Ref (Fiber Unit) -> (Inventory -> Effect Unit) -> Effect Unit
  withInventoryPoll fiberRef setInventory = do
    -- Kill any existing fiber before starting a new one
    existingFiber <- Ref.read fiberRef
    killFiber (error "Cancelling previous fiber") existingFiber

    -- Launch a new fiber to fetch the inventory (launching Aff from Effect context)
    launchNewFiber fiberRef setInventory

  -- This launches the new `Aff` fiber from the `Effect` context
  launchNewFiber :: Ref.Ref (Fiber Unit) -> (Inventory -> Effect Unit) -> Effect Unit
  launchNewFiber fiberRef setInventory = do
    newFiber <- launchAff $ fetchInventoryAff setInventory "json"
    Ref.write newFiber fiberRef
