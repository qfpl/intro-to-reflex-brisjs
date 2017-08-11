{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.List.TextField (
    TextFieldConfig(..)
  , textFieldConfig_initialValue
  , textFieldConfig_attributes
  , TextField(..)
  , textField_value
  , textField_change
  , textField_cleared
  , textField
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Default

import Reflex.Dom.Core

data TextFieldConfig t =
  TextFieldConfig {
    _textFieldConfig_initialValue :: Text
  , _textFieldConfig_attributes   :: Dynamic t (Map Text Text)
  }

instance Reflex t => Default (TextFieldConfig t) where
  def = TextFieldConfig "" (pure Map.empty)

makeLenses ''TextFieldConfig

data TextField t =
  TextField {
    _textField_value   :: Dynamic t Text
  , _textField_change  :: Event t Text
  , _textField_cleared :: Event t ()
  }

makeLenses ''TextField

textField ::
  forall t m.
  MonadWidget t m =>
  TextFieldConfig t ->
  m (TextField t)
textField tfc = mdo
  ti <- textInput $
    def & textInputConfig_setValue .~
          eClear
        & textInputConfig_initialValue .~
          (tfc ^. textFieldConfig_initialValue)
        & textInputConfig_attributes .~
          (tfc ^. textFieldConfig_attributes)

  let
    getKey k =
      void .
      ffilter ((== k) . keyCodeLookup . fromIntegral) $
      ti ^. textInput_keypress
    eEnter =
      getKey Enter
    eEscape =
      getKey Escape
    dValue =
      ti ^. textInput_value
    eAtEnter =
      current dValue <@ eEnter
    eDone =
      ffilter (not . Text.null) eAtEnter
    eRemove =
      void . ffilter Text.null $ eAtEnter
    eClear =
      "" <$ leftmost [void eDone, eEscape]

  pure $ TextField dValue eDone eRemove
