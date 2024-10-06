module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), QueryMode(..), fetchInventory)
import Control.Applicative (pure)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Functor.Variant (traverse)
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut)
import Deku.DOM.SVG as DS
import Deku.DOM.SVG.Attributes as DSA
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (subscribe)
import FRP.Event.Time (interval)
import FRP.Poll (Poll)

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

-- Define the size for SVG elements
size :: Int
size = 50

-- Function to assign a shape (rect or circle) based on category
-- Function to assign a shape (rect or circle) based on category
renderShape :: MenuItem -> Nut
renderShape (MenuItem item) =
  case item.category of
    "Flower" ->
      DS.circle
        [ DSA.cx_ "100"
        , DSA.cy_ "100"
        , DSA.r_ "50"
        , DSA.fill_ "green"
        ]
        []
    "Vape" ->
      DS.rect
        [ DSA.x_ "50"
        , DSA.y_ "50"
        , DSA.width_ "100"
        , DSA.height_ "50"
        , DSA.fill_ "blue"
        ]
        []
    _ ->
      DS.rect
        [ DSA.x_ "50"
        , DSA.y_ "50"
        , DSA.width_ "50"
        , DSA.height_ "50"
        , DSA.fill_ "gray"
        ]
        []


renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) =
  DS.svg
    [ DSA.viewBox_ "0 0 600 600"
    , DSA.width_ "600"
    , DSA.height_ "600"
    ]
    (map renderShape sortedItems)
  where
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items
    sortedItems = sortBy (compareMenuItems config) filteredItems

-- Main application
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
    { event: tickEvent } <- interval config.refreshRate
    void $ subscribe tickEvent \_ -> do
      fetchAndUpdateInventory

  -- Run Deku UI with SVG rendering
  void $ runInBody $ Deku.do
    DS.svg [] [ inventory <#~> renderInventory config ]
