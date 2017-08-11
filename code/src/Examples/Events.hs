{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module Examples.Events (
    attachEventExamples
  ) where

import Data.Monoid ((<>))

import Control.Lens
import Control.Monad.Trans (liftIO)

import Data.Text (Text)
import Data.Time (getCurrentTime)

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM(..))

import Colour

import Util.Reflex
import Util.Attach
import Util.Grid
import Util.SVG

import Examples.Events.FizzBuzz

import Util.Runner

attachEventExamples ::
  MonadJSM m =>
  m ()
attachEventExamples = do
  attachId_ "examples-events-frame" $
    wrapDemo id mkRedBlueInput
  attachId_ "examples-events-flipper" $
    wrapDemo flipper mkRedBlueInput
  attachId_ "examples-events-blue" $
    wrapDemo blue mkRedBlueInput
  attachId_ "examples-events-red" $
    wrapDemo red mkRedBlueInput
  attachId_ "examples-events-parse" $
    wrapDemo (parse . fmap unParseText) mkParseInput
  attachId_ "examples-events-either"
    demoEither

  attachId_ "examples-events-tick"
    demoTick

  attachFizzBuzzExamples

flipper :: Reflex t
        => Event t Colour
        -> Event t Colour
flipper eInput =
  let
    eOutput =
      flipColour <$> eInput
  in
    eOutput

blue :: Reflex t
     => Event t Colour
     -> Event t Colour
blue eInput =
  let
    eOutput =
      Blue <$ eInput
  in
    eOutput

isRed :: Colour
      -> Bool
isRed Red =
  True
isRed Blue =
  False

red :: Reflex t
    => Event t Colour
    -> Event t Colour
red eInput =
  let
    eOutput =
      ffilter isRed eInput
  in
    eOutput

newtype ParseText = ParseText { unParseText :: Text }

instance Square ParseText where
  mkSquare gc y x c =
    let
      attrs =
        "class" =: "grid-square" <>
        "fill" =: "gray" <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()

parseColour :: Text
            -> Maybe Colour
parseColour t =
  case t of
    "Red"  -> Just Red
    "Blue" -> Just Blue
    _ -> Nothing

parse :: Reflex t
      => Event t Text
      -> Event t Colour
parse eInput =
  let
    eOutput =
      fmapMaybe parseColour eInput
  in
    eOutput

mkParseInput :: MonadWidget t m
             => m (Event t ParseText)
mkParseInput = do
  tiColour <- textInput $ def & textInputConfig_initialValue .~ "Blue"
  eClick <- button "Go"
  return $ ParseText <$> current (tiColour ^. textInput_value) <@ eClick

wrapDemo ::
  ( MonadWidget t m
  , Square a
  , Square b) =>
  (Event t a -> Event t b) ->
  m (Event t a) ->
  m ()
wrapDemo guest mkIn = divClass "panel panel-default" . divClass "panel-body" $ mdo
  let w = runDemo guest eInput
  _ <- widgetHold w (w <$ eReset)
  (eInput, eReset) <- el "div" $ do
    eInput <- mkIn
    eReset <- buttonClass "btn btn-default pull-right" "Reset"
    return (eInput, eReset)
  return ()

runDemo ::
  ( MonadWidget t m
  , Square a
  , Square b
  ) =>
  (Event t a -> Event t b) ->
  Event t a ->
  m ()
runDemo guest eInput = do

  let
    eOutput =
      guest eInput

  dInputs <- foldDyn (:) [] .
             leftmost $ [
                 Just <$> eInput
               , Nothing <$ eOutput
               ]

  dOutputs <- foldDyn (:) [] .
              leftmost $ [
                  Just <$> eOutput
                , Nothing <$ eInput
                ]

  drawGrid
    defaultGridConfig
    [ Row "eInput" 1 dInputs
    , Row "eOutput" 3 dOutputs
    ]

data UnitSquare = UnitSquare

instance Square UnitSquare where
  mkSquare gc y x c =
    let
      attrs =
        "class" =: "grid-square" <>
        "fill" =: "gray" <>
        standardAttrs gc y x
    in
      svgAttr "rect" attrs $ pure ()

splitColour :: Colour
            -> Either () ()
splitColour Red =
  Left ()
splitColour Blue =
  Right ()

fanDemo :: Reflex t
        => Event t Colour
        -> (Event t (), Event t ())
fanDemo eInput =
  let
    (eLeft, eRight) =
      fanEither .
      fmap splitColour $
      eInput
  in
    (eLeft, eRight)

demoEither ::
  MonadWidget t m =>
  m ()
demoEither = divClass "panel panel-default" . divClass "panel-body" $ mdo
  let
    widget = do
      let
        (eLeft, eRight) = fanDemo eInput

      dInputs <- foldDyn (:) [] .
                leftmost $ [
                    Just <$> eInput
                  , Nothing <$ eLeft
                  , Nothing <$ eRight
                  ]

      dLefts <- foldDyn (:) [] .
                  leftmost $ [
                      Just UnitSquare <$ eLeft
                    , Nothing <$ eInput
                    , Nothing <$ eRight
                    ]

      dRights <- foldDyn (:) [] .
                  leftmost $ [
                      Just UnitSquare <$ eRight
                    , Nothing <$ eInput
                    , Nothing <$ eLeft
                    ]

      drawGrid
        (defaultGridConfig & gcRows .~ 7)
        [ Row "eInput" 1 dInputs
        , Row "eLeft" 3 dLefts
        , Row "eRight" 5 dRights
        ]

  _ <- widgetHold widget (widget <$ eReset)
  (eInput, eReset) <- el "div" $ do
    eInput <- mkRedBlueInput
    eReset <- buttonClass "btn btn-default pull-right" "Reset"
    return (eInput, eReset)
  return ()

demoTick ::
  MonadWidget t m =>
  m ()
demoTick = divClass "panel panel-default" . divClass "panel-body" $ mdo
  let
    wInitial = Workflow $ do
      eNext <- button "Start"
      pure ((), wRunning <$ eNext)
    wRunning = Workflow $ do
      eStop <- demoTick'
      pure ((), wInitial <$ eStop)

  _ <- workflow wInitial
  pure ()

demoTick' ::
  MonadWidget t m =>
  m (Event t ())
demoTick' = mdo
  dClick <- foldDyn (:) [] .
            leftmost $ [
                Just <$> eClick
              , Nothing <$ eTick
              ]

  dTick <- foldDyn (:) [] .
           leftmost $ [
               Just <$> eTick
             , Nothing <$ eClick
             ]

  drawGrid defaultGridConfig
    [ Row "eClick" 1 dClick
    , Row "eTick" 3 dTick
    ]

  (eClick, eTick, eReset) <- el "div" $ do
    eClick' <- buttonClass "btn btn-default pull-right" "Click"
    now <- liftIO getCurrentTime
    eTick' <- tickLossy 2 now
    eReset' <- buttonClass "btn btn-default pull-right" "Reset"
    return (Blue <$ eClick', Red <$ eTick', eReset')

  return eReset

