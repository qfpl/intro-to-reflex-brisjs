{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Examples.List.Add (
    addItem
  ) where

import Control.Lens

import Data.Text (Text)

import Reflex.Dom.Core

import Examples.List.TextField

addItem ::
  MonadWidget t m =>
  m (Event t Text)
addItem = elClass "div" "add-item" $ mdo
  tf <- textField $ TextFieldConfig "" (pure $ "placeholder" =: "What shall we do today?")

  pure $ tf ^. textField_change
