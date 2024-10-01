module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), fetchInventoryFromJson, fetchInventoryFromHttp)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Functor (void)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut(..), text_)
import Deku.DOM as D
import Deku.DOM.Attributes (klass_)
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Timer (setInterval)


-- Sorting Configuration
data SortField = SortByName | SortByCategory | SortBySubCategory | SortByPrice | SortByQuantity
data SortOrder = Ascending | Descending

type Config =
  { sortField :: SortField
  , sortOrder :: SortOrder
  , hideOutOfStock :: Boolean
  , screens :: Int
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

-- Fetch inventory and update state
fetchInventoryAff :: (Inventory -> Effect Unit) -> String -> Aff Unit
fetchInventoryAff setInventory mode = do
  result <- case mode of
    "json" -> fetchInventoryFromJson
    "http" -> fetchInventoryFromHttp
    _ -> pure $ Left "Invalid mode"

  case result of
    Left err -> liftEffect $ log ("Error fetching inventory: " <> err)
    Right (InventoryData inv) -> liftEffect $ setInventory inv
    Right (Message msg) -> liftEffect $ log ("Message: " <> msg)

app :: Effect Unit
app = do
  -- Initialize inventory state
  setInventory /\ inventoryPoll <- useState (Inventory [])

  -- Perform an initial fetch
  launchAff_ $ fetchInventoryAff setInventory "json"

  -- Set up the interval
  void $ setInterval 3000 $ do
    launchAff_ $ fetchInventoryAff setInventory "json"

  -- Run the UI
  runInBody $ D.div [] [ inventoryPoll <#~> renderInventory config ]
  where
    config =
      { sortField: SortByCategory
      , sortOrder: Ascending
      , hideOutOfStock: true
      , screens: 1
      }
