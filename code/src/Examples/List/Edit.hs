{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.List.Edit (
    EditConfig(..)
  , editConfig_initialValue
  , Edit(..)
  , edit_value
  , edit_remove
  , edit
  ) where

import Control.Lens

import Data.Text (Text)

import Data.Default

import Reflex.Dom.Core

import Examples.List.TextField

data EditConfig =
  EditConfig {
    _editConfig_initialValue :: Text
  }

instance Default EditConfig where
  def = EditConfig ""

makeLenses ''EditConfig

data Edit t =
  Edit {
    _edit_value  :: Event t Text
  , _edit_remove :: Event t ()
  }

makeLenses ''Edit

editRead ::
  MonadWidget t m =>
  EditConfig ->
  Workflow t m (Edit t)
editRead ec = Workflow . elClass "div" "todo-item-edit-read" $ do
  (l,_) <- el' "div" . text $ ec ^. editConfig_initialValue
  pure (Edit never never, editWrite ec <$ domEvent Dblclick l)

editWrite ::
  MonadWidget t m =>
  EditConfig ->
  Workflow t m (Edit t)
editWrite ec = Workflow . elClass "div" "todo-item-edit-write" $ do
  tf <- textField $
    TextFieldConfig
      (ec ^. editConfig_initialValue)
      (pure mempty)
  let
    eValue = tf ^. textField_change
    eRemove = tf ^. textField_cleared

  pure (Edit eValue eRemove, (editRead . EditConfig) <$> eValue)

edit ::
  MonadWidget t m =>
  EditConfig ->
  m (Edit t)
edit ec = do
  dEdit <- workflow $ editRead ec
  let
    switchE l = switch . current . fmap (view l)
    eValue = switchE edit_value dEdit
    eRemove = switchE edit_remove dEdit

  pure $ Edit eValue eRemove
