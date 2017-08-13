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
module Examples.List (
    attachListExamples
  ) where

import Control.Monad (void)

import Control.Lens

import Control.Monad.Reader (MonadReader, ask, runReaderT)

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
  eAdd <- elClass "div" "add-item" addItem
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
  eAdd <- elClass "div" "add-item" addItem
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

  eAdd <- elClass "div" "add-item" addItem
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

attachListExamples ::
  MonadJSM m =>
  m ()
attachListExamples = do
  attachId_ "examples-list-item" $
    resetWidget . trackRemove . todoItem1 . pure $ "Test Me"
