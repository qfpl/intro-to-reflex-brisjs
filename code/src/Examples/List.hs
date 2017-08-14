{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.List (
    attachListExamples
  ) where

import Control.Monad (void)
import Data.Semigroup (Semigroup(..))

import Control.Lens

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

import Util.Runner

import Examples.List.Add
import Examples.List.Edit

trackRemove ::
  MonadWidget t m =>
  m (Event t ()) ->
  m ()
trackRemove w = do
  eRemove <- w

  dRemove <- holdDyn False (True <$ eRemove)

  let
    mkClass False = "hide"
    mkClass True = ""
    dClass = fmap mkClass dRemove

  elDynClass "div" dClass $
    text "Removed"

  pure ()

resetWidget ::
  MonadWidget t m =>
  m () ->
  m ()
resetWidget w = mdo
  _ <- widgetHold w (w <$ eReset)
  eReset <- button "Reset"
  pure ()

complete1 ::
  MonadWidget t m =>
  m (Dynamic t Bool)
complete1 = do
  cb <- checkbox False def

  let
    dComplete = view checkbox_value cb

  pure dComplete

itemEdit1 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
itemEdit1 dText = do
  initial <- sample (current dText)
  Edit _ eRemove <- edit (EditConfig initial)
  pure eRemove

remove1 ::
  MonadWidget t m =>
  m (Event t ())
remove1 =
  button "Remove"

todoItem1 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem1 dText = elClass "li" "todo-item" $ do
  dComplete <- complete1

  let
    mkComplete False = ""
    mkComplete True = "completed "
    dClass = fmap mkComplete dComplete

  eRemoveEmpty <- elDynClass "div" dClass $
    itemEdit1 dText

  eRemoveClick <- remove1

  let
    eRemove = leftmost [eRemoveEmpty, eRemoveClick]

  pure eRemove

todoList1 ::
  MonadWidget t m =>
  m ()
todoList1 = elClass "div" "todo-list" $ mdo
  eAdd <- addItem
  dCount :: Dynamic t Int <- count eAdd

  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmRemove <-
    el "ul" . list dMap $ \dv ->
      todoItem1 $ dv

  let
    eRemoves =
      fmap Map.keys .
      switch .
      current .
      fmap mergeMap $
      dmRemove

  pure ()

tellRemove ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  Event t a ->
  m ()
tellRemove e = do
  k <- ask
  tellEvent $ Set.singleton k <$ e

complete2 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  m (Dynamic t Bool)
complete2 = do
  cb <- checkbox False def

  let
    dComplete = view checkbox_value cb

  pure dComplete

itemEdit2 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  m ()
itemEdit2 dText = do
  initial <- sample (current dText)
  Edit _ eRemove <- edit (EditConfig initial)
  tellRemove eRemove
  pure ()

remove2 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  m ()
remove2 = do
  eRemove <- button "Remove"
  tellRemove eRemove

todoItem2 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  m (Dynamic t Bool)
todoItem2 dText = elClass "li" "todo-item" $ do
  dComplete <- complete2

  let
    mkComplete False = ""
    mkComplete True = "completed "
    dClass = fmap mkComplete dComplete

  elDynClass "div" dClass $
    itemEdit2 dText

  remove2

  pure dComplete

todoList2 ::
  MonadWidget t m =>
  m ()
todoList2 = elClass "div" "todo-list" $ mdo
  eAdd <- addItem
  dCount :: Dynamic t Int <- count eAdd

  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  (dmdComplete, eRemoves) <-
    runEventWriterT .
    el "ul" . listWithKey dMap $ \k dv ->
      flip runReaderT k $
        todoItem2 dv
  let
    dCompletes = joinDynThroughMap dmdComplete

  pure ()

complete3 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  m (Dynamic t Bool)
complete3 eMarkAllComplete eClearComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dComplete = view checkbox_value cb

  tellRemove $ gate (current dComplete) eClearComplete

  pure dComplete

todoItem3 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (Set k) m
  , MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  Dynamic t Text ->
  m (Dynamic t Bool)
todoItem3 eMarkAllComplete eClearComplete dText = elClass "li" "todo-item" $ do
  dComplete <- complete3 eMarkAllComplete eClearComplete

  let
    mkComplete False = ""
    mkComplete True = "completed "
    dClass = fmap mkComplete dComplete

  elDynClass "div" dClass $
    itemEdit2 dText

  remove2

  pure dComplete

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete dAllComplete = el "div" $ do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure (view checkbox_change cb)

clearComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
clearComplete dAnyComplete = do
  let
    mkClass False = "hide"
    mkClass True = ""

    dClass = fmap mkClass dAnyComplete

  elDynClass "div" dClass $
    button "Clear complete"

todoList3 ::
  MonadWidget t m =>
  m ()
todoList3 = elClass "div" "todo-list" $ mdo

  eAdd <- addItem
  dCount :: Dynamic t Int <- count eAdd

  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  (dmdComplete, eRemoves) <-
    runEventWriterT .
    el "ul" . listWithKey dMap $ \k dv ->
      flip runReaderT k .
      todoItem3 eMarkAllComplete eClearComplete $ dv
  let
    dCompletes = joinDynThroughMap dmdComplete
    dAllComplete = fmap and dCompletes
    dAnyComplete = fmap or dCompletes

  eMarkAllComplete <- markAllComplete dAllComplete
  eClearComplete <- clearComplete dAnyComplete

  pure ()

data TodoItemModel =
  TodoItemModel {
    _timComplete :: Bool
  , _timText     :: Text
  }

makeLenses ''TodoItemModel

data TodoWriter k i =
  TodoWriter {
    twChanges :: Map k (i -> i)
  , twRemoves :: Set k
  }

instance Ord k => Semigroup (TodoWriter k i) where
  (<>) (TodoWriter m1 s1) (TodoWriter m2 s2) =
    TodoWriter (Map.unionWith (.) m1 m2) (Set.union s1 s2)

instance Ord k => Monoid (TodoWriter k i) where
  mempty = TodoWriter Map.empty Set.empty

changeEvent ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (TodoWriter k i) m
  , Reflex t
  ) =>
  Event t (i -> i) ->
  m ()
changeEvent e = do
  k <- ask
  tellEvent $ (\f -> TodoWriter (Map.singleton k f) Set.empty) <$> e

removeEvent ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (TodoWriter k i) m
  , Reflex t
  ) =>
  Event t a ->
  m ()
removeEvent e = do
  k <- ask
  tellEvent $ TodoWriter Map.empty (Set.singleton k) <$ e

todoListHelper ::
  ( Num k
  , Ord k
  , MonadWidget t m
  ) =>
  Event t i ->
  (Dynamic t i -> ReaderT k (EventWriterT t (TodoWriter k i) m) a) ->
  m (Dynamic t (Map k i), Dynamic t (Map k a))
todoListHelper eAdd w = mdo
  dCount <- count eAdd

  let
    applyChanges = Map.mergeWithKey (\_ f x -> Just $ f x) (const Map.empty) id
    applyRemoves = flip (foldr Map.delete)

  dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert   <$> current dCount <@> eAdd
    , applyChanges <$> eChanges
    , applyRemoves <$> eRemoves
    ]

  (dResult, writer) <-
    runEventWriterT .
      listWithKey dModel $ \k dv ->
      flip runReaderT k .
      w $ dv

  let
    eChanges = twChanges <$> writer
    eRemoves = twRemoves <$> writer

  pure (dModel, dResult)

complete4 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (TodoWriter k TodoItemModel) m
  , MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  m (Dynamic t Bool)
complete4 eMarkAllComplete eClearComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    eComplete = view checkbox_change cb
    dComplete = view checkbox_value cb

  changeEvent $ set timComplete <$> eComplete
  removeEvent $ gate (current dComplete) eClearComplete

  pure dComplete

itemEdit4 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (TodoWriter k TodoItemModel) m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  m ()
itemEdit4 dText = do
  initial <- sample (current dText)
  Edit eText eRemove <- edit (EditConfig initial)
  changeEvent $ set timText <$> eText
  removeEvent eRemove
  pure ()

remove4 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (TodoWriter k TodoItemModel) m
  , MonadWidget t m
  ) =>
  m ()
remove4 = do
  eRemove <- button "Remove"
  removeEvent eRemove

todoItem4 ::
  ( Ord k
  , MonadReader k m
  , EventWriter t (TodoWriter k TodoItemModel) m
  , MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItemModel ->
  m (Dynamic t Bool)
todoItem4 eMarkAllComplete eClearComplete dModel = elClass "li" "todo-item" $ do
  dText <- holdUniqDyn (view timText <$> dModel)

  dComplete <- complete4 eMarkAllComplete eClearComplete

  let
    mkComplete False = ""
    mkComplete True = "completed "
    dClass = fmap mkComplete dComplete

  elDynClass "div" dClass $
    itemEdit4 dText

  remove4

  pure dComplete

todoList4 ::
  MonadWidget t m =>
  m ()
todoList4 = elClass "div" "todo-list" $ mdo
  eName <- addItem
  let
    eAdd = TodoItemModel False <$> eName

  (dModel, dmdComplete) <- el "ul" $
    todoListHelper eAdd $
      todoItem4 eMarkAllComplete eClearComplete

  let
    dCompletes = joinDynThroughMap dmdComplete
    dAllComplete = fmap and dCompletes
    dAnyComplete = fmap or dCompletes

  eMarkAllComplete <- markAllComplete dAllComplete
  eClearComplete <- clearComplete dAnyComplete

  pure ()

complete5 ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  m (Dynamic t Bool, Event t ())
complete5 eMarkAllComplete eClearComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    eComplete = view checkbox_change cb
    dComplete = view checkbox_value cb
    eRemove   = gate (current dComplete) eClearComplete

  pure (dComplete, eRemove)

itemEdit5 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
itemEdit5 dText = do
  initial <- sample (current dText)
  Edit _ eRemove <- edit (EditConfig initial)
  pure eRemove

remove5 ::
  MonadWidget t m =>
  m (Event t ())
remove5 =
  button "Remove"

todoItem5 ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t Text ->
  m (Dynamic t Bool, Event t ())
todoItem5 eMarkAllComplete eClearComplete dText = elClass "li" "todo-item" $ do
  (dComplete, eRemoveComplete) <- complete5 eMarkAllComplete eClearComplete

  let
    mkComplete False = ""
    mkComplete True = "completed "

    dClass = fmap mkComplete dComplete

  eRemoveEmpty <- elDynClass "div" dClass $
    itemEdit5 dText

  eRemoveClick <- remove5

  let
    eRemove = leftmost [
        eRemoveComplete
      , eRemoveEmpty
      , eRemoveClick
      ]

  pure (dComplete, eRemove)

todoList5 ::
  MonadWidget t m =>
  m ()
todoList5 = elClass "div" "todo-list" $ mdo
  eAdd <- addItem
  dCount <- count eAdd

  let
    applyRemoves = flip (foldr Map.delete)

  dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert   <$> current dCount <@> eAdd
    , applyRemoves <$> eRemoves
    ]

  dList <- el "ul" . list dModel $ \dv ->
    todoItem5 eMarkAllComplete eClearComplete dv

  let
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dList
    dmCompletes = joinDynThroughMap . fmap (fmap fst) $ dList

    dAllComplete = fmap and dmCompletes
    dAnyComplete = fmap or dmCompletes

  eMarkAllComplete <- markAllComplete dAllComplete
  eClearComplete <- clearComplete dAnyComplete

  pure ()

attachListExamples ::
  MonadJSM m =>
  m ()
attachListExamples = do
  attachId_ "examples-list" $
    todoList4
  attachId_ "examples-list-2" $
    todoList4
