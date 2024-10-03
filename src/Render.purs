module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), QueryMode(..), fetchInventory)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, text_)
import Deku.DOM as D
import Deku.DOM.Attributes (klass_)
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (subscribe)
import FRP.Event.Time (interval)

-- Sorting Configuration
data SortField = SortByName | SortByCategory | SortBySubCategory | SortByPrice | SortByQuantity
data SortOrder = Ascending | Descending

type Config =
  { sortField :: SortField
  , sortOrder :: SortOrder
  , hideOutOfStock :: Boolean
  , mode :: QueryMode
  , refreshRate :: Int
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

app :: Effect Unit
app = do
  setInventory /\ inventory <- useState (Inventory [])
  let
    config =
      { sortField: SortByCategory
      , sortOrder: Ascending
      , hideOutOfStock: true
      , mode: JsonMode
      , refreshRate: 3000
      , screens: 1
      }

    fetchAndUpdateInventory :: Effect Unit
    fetchAndUpdateInventory = launchAff_ do
      result <- fetchInventory config.mode
      liftEffect $ case result of
        Left err -> log ("Error fetching inventory: " <> err)
        Right (InventoryData inv) -> setInventory inv
        Right (Message msg) -> log ("Message: " <> msg)

  _ <- fetchAndUpdateInventory

  do
    { event: tickEvent} <- interval config.refreshRate
    void $ subscribe tickEvent \_ -> do
      fetchAndUpdateInventory

  -- Run the UI
  void $ runInBody $ Deku.do
    D.div [] [ inventory <#~> renderInventory config ]
