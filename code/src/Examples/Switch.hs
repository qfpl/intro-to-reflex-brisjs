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
{-# LANGUAGE TemplateHaskell #-}
module Examples.Switch (
    attachSwitchExamples
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Grid
import Colour

attachSwitchExamples ::
  MonadJSM m =>
  m ()
attachSwitchExamples = do
  attachId_ "examples-switch-colour-1" $
    switchColourExample switchColour1
  attachId_ "examples-switch-colour-2" $
    switchColourExample switchColour2
  attachId_ "examples-switch-widget-hide"
    widgetHideExample
  attachId_ "examples-switch-widget-hold"
    widgetHoldExample
  attachId_ "examples-switch-dyn"
    dynExample
  attachId_ "examples-switch-workflow-1"
    workflowExample1
  attachId_ "examples-switch-workflow-2"
    workflowExample2

switchColour1 ::
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t Colour ->
  m (Event t Colour, Event t Colour)
switchColour1 eSwitch1 eSwitch2 eInput = do
  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (eOut1, eOut2)

switchColour2 ::
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t Colour ->
  m (Event t Colour, Event t Colour)
switchColour2 eSwitch1 eSwitch2 eInput = do
  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  bOut2 <- hold never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (switch bOut1, switch bOut2)

switchColourExample ::
  MonadWidget t m =>
  (Event t () -> Event t () -> Event t Colour -> m (Event t Colour, Event t Colour)) ->
  m ()
switchColourExample guest = el "div" $ mdo
  eSwitch1 <- el "div" $ do
    eSwitch <- button "Switch"
    drawGrid defaultGridConfig [ Row "eOutput" 1 dOut1 ]
    pure eSwitch

  eSwitch2 <- el "div" $ do
    eSwitch <- button "Switch"
    drawGrid defaultGridConfig [ Row "eOutput" 1 dOut2 ]
    pure eSwitch

  eInput <- mkRedBlueInput

  (eOut1, eOut2) <- guest eSwitch1 eSwitch2 eInput

  dOut1 <- foldDyn (:) [] . leftmost $ [
      Just <$> eOut1
    , Nothing <$ eOut2
    ]

  dOut2 <- foldDyn (:) [] . leftmost $ [
      Just <$> eOut2
    , Nothing <$ eOut1
    ]

  pure ()

w1 ::
  MonadWidget t m =>
  m (Event t Text)
w1 = do
  eClick <- button "OK"
  pure $ "OK" <$ eClick

w2 ::
  MonadWidget t m =>
  m (Event t Text)
w2 = do
  ti <- textInput def
  pure $ ti ^. textInput_input

widgetHideExample ::
  MonadWidget t m =>
  m ()
widgetHideExample = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"

  dCount <- count eSwitch
  let
    eEven = fmap even dCount
    eOdd =  fmap odd dCount

  let
    mkHidden False  = "hide"
    mkHidden True = ""

  eText1 <- elDynClass "div" (mkHidden <$> eEven)
    w1
  eText2 <- elDynClass "div" (mkHidden <$> eOdd)
    w2

  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

widgetHoldExample ::
  MonadWidget t m =>
  m ()
widgetHoldExample = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd =  ffilter odd . updated $ dCount

  deText <- widgetHold w1 . leftmost $ [
      w1 <$ eEven
    , w2 <$ eOdd
    ]

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

dynExample ::
  MonadWidget t m =>
  m ()
dynExample = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd =  ffilter odd . updated $ dCount

  dWidget <- holdDyn w1 . leftmost $ [
      w1 <$ eEven
    , w2 <$ eOdd
    ]

  eeText <- dyn dWidget
  eText <- switchPromptly never eeText

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

workflowExample1 ::
  forall t m.
  MonadWidget t m =>
  m ()
workflowExample1 = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"

  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText1 <- w1
      pure (eText1, wf2 <$ eSwitch)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText2 <- w2
      pure (eText2, wf1 <$ eSwitch)

  deText <- workflow wf1

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

workflowExample2 ::
  forall t m.
  MonadWidget t m =>
  m ()
workflowExample2 = elClass "div" "widget-hold-wrapper" $ do

  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eSwitch <- el "div" $ button "On to page 2"
      eText <- w1
      pure (eText, wf2 <$ eSwitch)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eSwitch <- el "div" $ button "Back to page 1"
      eText <- w2
      pure (eText, wf1 <$ eSwitch)

  deText <- workflow wf1

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

