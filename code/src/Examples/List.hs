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
module Examples.List (
    attachListExamples
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

import Util.Runner

import Examples.List.Add
import Examples.List.Edit

data TodoItemModel =
  TodoItemModel {
    timComplete :: Bool
  , timText     :: Text
  }

data TodoItemConfig t =
  TodoItemConfig {
    ticComplete :: Dynamic t Bool
  , ticText     :: Dynamic t Text
  }

data TodoItem t =
  TodoItem {
    tiComplete :: Event t Bool
  , tiText     :: Event t Text
  , tiRemove   :: Event t ()
  }

complete ::
  MonadWidget t m =>
  m (Event t Bool)
complete = do
  cb <- checkbox False def

  let
    eComplete = view checkbox_change cb

  pure eComplete

itemEdit ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
itemEdit initial = do
  Edit _ eRemove <- edit (EditConfig initial)
  pure eRemove

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

todoItem ::
  MonadWidget t m =>
  Text ->
  m (Event t Bool, Event t ())
todoItem initial = elClass "div" "todo-item" $ do

  eComplete <- complete

  dComplete <- holdDyn False eComplete
  let
    mkComplete False = ""
    mkComplete True = "completed "
    dClass = fmap mkComplete dComplete

  eRemoveEmpty <- elDynClass "div" dClass $
    itemEdit initial

  eRemoveClick <- remove

  let
    eRemove = leftmost [eRemoveEmpty, eRemoveClick]

  pure (eComplete, eRemove)

todoItemExample' ::
  MonadWidget t m =>
  m ()
todoItemExample' = do

  (_, eRemove) <- todoItem "Test me"

  dRemove <- holdDyn False (True <$ eRemove)

  let
    mkClass False = "hide"
    mkClass True = ""
    dClass = fmap mkClass dRemove

  elDynClass "div" dClass $
    text "Removed"

  pure ()

todoItemExample ::
  MonadWidget t m =>
  m ()
todoItemExample = mdo
  _ <- widgetHold todoItemExample' (todoItemExample' <$ eReset)
  eReset <- button "Reset"
  pure ()

todoList ::
  MonadWidget t m =>
  m ()
todoList = elClass "div" "todo-list" $ mdo
  eAdd <- elClass "div" "add-item" addItem
  dCount <- count eAdd

  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmPair <- el "ul" . list dMap $ \dv -> do
    v <- sample . current $ dv
    el "li" . todoItem $ v

  let
    dmComplete = fmap (fmap fst) dmPair
    dmRemove = fmap (fmap snd) dmPair
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmRemove

  pure ()

attachListExamples ::
  MonadJSM m =>
  m ()
attachListExamples = do
  attachId_ "examples-list-item" $
    todoItemExample
