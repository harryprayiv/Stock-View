module Render where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), fetchInventoryFromJson, fetchInventoryFromHttp)
import Data.Array (filter, sortBy)
import Data.Compactable (compact)
import Data.Compactable (compact)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Either (Either(..), hush)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.Op (Op)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut(..), text_)
import Deku.DOM as D
import Deku.DOM.Attributes (klass_)
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, error, killFiber, launchAff, launchAff_, never)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId)
import Effect.Timer (TimeoutId, clearInterval, setInterval, setTimeout)
import FRP.Event (Event, makeEventE, mapAccum)
import FRP.Event (subscribe)
import FRP.Event.Random (withRandom)
-- import FRP.Event.Time (interval')
import Safe.Coerce (coerce)
import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), fetchInventoryFromJson, fetchInventoryFromHttp)
import Data.Array (filter, sortBy)
import Data.Compactable (compact)
import Data.Either (Either(..), hush)
import Data.Int (round)
import Data.Tuple (snd)
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut(..), text_)
import Deku.DOM as D
import Deku.DOM.Attributes (klass_)
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, error, killFiber, launchAff, never)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearInterval, setInterval, setTimeout)
import FRP.Event (Event, subscribe)
import FRP.Event.Random (withRandom)
import FRP.Event.Time (interval)

withDelay' :: forall a. (a -> Int) -> Op (Effect Unit) (Either TimeoutId (Tuple TimeoutId a)) -> Op (Effect Unit) a
withDelay' nf = (coerce :: (_ -> a -> _ Unit) -> _ -> _) go
  where
  go f value = launchAff_ do
    tid <- Avar.empty
    o <- liftEffect $ setTimeout (nf value) $ launchAff_ do
      t <- Avar.read tid
      liftEffect $ f (Right (Tuple t value))
    Avar.put o tid
    liftEffect $ f (Left o)

interval' :: forall a. (Op (Effect Unit) a -> Op (Effect Unit) Instant) -> Int -> Effect { event :: Event a, unsubscribe :: Effect Unit }
interval' f n = makeEventE \k -> do
  id <- setInterval n do
    time <- now
    (coerce :: _ -> _ -> _ -> _ Unit) f k time
  pure (clearInterval id)

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

-- Launching the Aff in the Effect context
launchNewFiber :: Ref.Ref (Fiber Unit) -> (Inventory -> Effect Unit) -> Effect Unit
launchNewFiber fiberRef setInventory = do
  newFiber <- launchAff $ fetchInventoryAff setInventory "json"
  Ref.write newFiber fiberRef

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

    -- Set up polling with `interval'`, using random delays and event stream mapping
    let opChain =
          withDelay' (\_ -> 3000)  -- Delay function can be tuned or randomized
          >>> withRandom

    -- Create interval event stream
    { event } <- interval' (\k _ -> opChain k) 3000

    -- Process the event stream and filter compacted results
    let resultStream = map snd $ compact $ map hush event

    -- Subscribe to the event stream to trigger inventory polling
    void $ subscribe resultStream (\_ -> withInventoryPoll fiberRef setInventory)

    -- Render inventory when it changes
    renderInventory config inventory

  where
  -- This function handles the polling and fiber management for fetching inventory
  withInventoryPoll :: Ref.Ref (Fiber Unit) -> (Inventory -> Effect Unit) -> Effect Unit
  withInventoryPoll fiberRef setInventory = do
    -- Kill any existing fiber before starting a new one
    existingFiber <- Ref.read fiberRef
    liftAff $ killFiber (error "Cancelling previous fiber") existingFiber

    -- Launch a new fiber to fetch the inventory
    launchNewFiber fiberRef setInventory
