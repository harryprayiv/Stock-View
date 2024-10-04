module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), QueryMode(..), fetchInventory)
import CSS.Color (fromInt)
import CSS.Color (Color) as CSS
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
 
-- needed autoFill, minMax, gap, scale, gridTemplateColumns

red:: CSS.Color
red = fromInt 0xff0000

-- Sorting Configuration
data SortField =  SortByName 
                | SortByCategory 
                | SortBySubCategory 
                | SortBySpecies
                | SortBySKU
                | SortByPrice 
                | SortByQuantity

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
      SortBySpecies -> compare item1.species item2.species
      SortBySKU -> compare item1.sku item2.sku
      SortByPrice -> compare item1.price item2.price
      SortByQuantity -> compare item1.quantity item2.quantity
  in
    case config.sortOrder of
      Ascending -> baseComparison
      Descending -> invertOrdering baseComparison

-- -- styling sucks with type safety!
-- gridStyle :: CSS
-- gridStyle = do
--   display grid
--   key (fromString "grid-template-columns") (Value (fromString "repeat(auto-fill, minmax(200px, 1fr))"))  -- Wrap value with Value
--   key (fromString "gap") (px 16.0)  -- Add gap between grid items

-- -- Define card styles using purescript-css
-- cardStyle :: CSS
-- cardStyle = do
--   display inlineBlock
--   padding (px 16.0)
--   border solid (px 1.0)
--   borderRadius (px 8.0)
--   -- boxShadow (fromString "0 2px 4px rgba(0, 0, 0, 0.1)")
--   boxShadow $ singleton $ shadow (px 2.0) (px 4.0)
--   backgroundColor (fromString "#fff")
--   width (pct 100.0)
--   maxWidth (px 300.0)
--   -- transition (fromString "transform 0.2s, box-shadow 0.2s")

-- -- -- Hover effect for the card
-- -- hoverCardStyle :: CSS
-- -- hoverCardStyle = hover do
-- --   boxShadow (fromString "0 4px 8px rgba(0, 0, 0, 0.2)")
-- --   transform (scale 1.05)

renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) = D.div
  [ klass_ "inventory-grid" ]
  (map renderItem sortedItems)
  where
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items
    sortedItems = sortBy (compareMenuItems config) filteredItems

renderItem :: MenuItem -> Nut
renderItem (MenuItem item) = D.div
  [ klass_ "inventory-item-card" ]
  [ D.div [ klass_ "item-name" ] [ text_ ("'" <> item.name <> "'") ]
  , D.div [ klass_ "item-category" ] [ text_ (item.category <> " - " <> item.subcategory) ]
  , D.div [ klass_ "item-species" ] [ text_ ( item.species) ]
  , D.div [ klass_ "item-price" ] [ text_ ("$" <> show item.price) ]
  , D.div [ klass_ "item-quantity" ] [ text_ ("In Stock: " <> show item.quantity) ]
  ]

app :: Effect Unit
app = do
  setInventory /\ inventory <- useState (Inventory [])
  let
    config =
      { sortField: SortByCategory
      , sortOrder: Descending
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
