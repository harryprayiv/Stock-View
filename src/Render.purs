module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), QueryMode(..), fetchInventory)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Functor.Variant (traverse)
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, useRant)
import Deku.DOM.SVG as DS
import Deku.DOM.SVG.Attributes as DSA
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (sampleOnRight, subscribe)
import FRP.Event.AnimationFrame (animationFrame')
import FRP.Event.Time (interval)
import FRP.Poll (Poll, sham)

-- Sorting Configuration
data SortField = SortByName | SortByCategory | SortBySubCategory | SortBySpecies | SortBySKU | SortByPrice | SortByQuantity

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

renderShape :: MenuItem -> Poll Nut
renderShape (MenuItem item) = do
  -- Extract the event by binding to the effect??
  frameData <- animationFrame'
  let animEvent = frameData.event -- Error: Could not match type Record with type Effect

  ticked <- useRant $ sampleOnRight animEvent (pure initialState)
  
  case item.category of
    "Flower" -> pure $ DS.circle
      [ DSA.cx $ show <$> (_.positionX <$> ticked)  
      , DSA.cy $ show <$> (_.positionY <$> ticked)  
      , DSA.r $ show <$> (_.diameter <$> ticked)    
      , DSA.fill_ "green"
      ] []
    "Concentrate" -> pure $ DS.rect
      [ DSA.x $ show <$> (_.positionX <$> ticked)   
      , DSA.y $ show <$> (_.positionY <$> ticked)   
      , DSA.width $ show <$> (_.diameter <$> ticked) 
      , DSA.height $ show <$> (_.diameter <$> ticked) 
      , DSA.fill_ "blue"
      ] []
    _ -> pure $ DS.rect
      [ DSA.x $ show <$> (_.positionX <$> ticked)
      , DSA.y $ show <$> (_.positionY <$> ticked)
      , DSA.width $ show <$> (_.diameter <$> ticked)
      , DSA.height $ show <$> (_.diameter <$> ticked)
      , DSA.fill_ "gray"
      ] []
  where
    -- Define initial state for `useRant`
    initialState = 
      { positionX: 100.0
      , positionY: 100.0
      , diameter: 50.0
      }

renderInventory :: Config -> Inventory -> Poll Nut
renderInventory config (Inventory items) = do
  viewBox <- DSA.viewBox_ "0 0 600 600"
  width <- DSA.width_ "600"
  height <- DSA.height_ "600"
  shapes <- traverse renderShape sortedItems
  pure $ DS.svg [viewBox, width, height] shapes
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
