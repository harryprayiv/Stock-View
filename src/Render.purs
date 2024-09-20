module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), fetchInventoryFromJson)
import Control.Monad.Rec.Class (forever)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Attributes (klass_)
import Deku.Control (text_)
import Deku.Core (Nut(..))
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useDyn, useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Effect.Timer (setInterval)
import Effect.Class (liftEffect)

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

-- Poll function for fetching inventory periodically
fetchInventoryPoll :: Aff (Either String InventoryResponse)
fetchInventoryPoll = fetchInventoryFromJson

app :: Effect Unit
app = void runInBody Deku.do
  -- Initialize inventory state
  setInventory /\ inventory <- useState (Inventory [])

  let config =
        { sortField: SortByCategory
        , sortOrder: Ascending
        , hideOutOfStock: true
        , screens: 1
        }

  -- Function to fetch inventory periodically
  liftAff $ launchAff_ $ do
    setInterval 3000 $ do -- 3000 ms interval
      result <- fetchInventoryPoll
      case result of
        Left err -> log ("Error: " <> err)
        Right (InventoryData inv) -> liftEffect $ setInventory inv
        Right (Message msg) -> log ("Message: " <> msg)
      pure unit

  -- Render inventory when it changes
  do
    { value: inv } <- useDyn inventory
    renderInventory config inv
    pure unit
