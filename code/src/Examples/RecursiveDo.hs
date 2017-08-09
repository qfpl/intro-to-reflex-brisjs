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
module Examples.RecursiveDo (
    attachRecursiveDoExamples
  , counterExample1
  ) where

import Control.Monad.Fix (MonadFix)

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

attachRecursiveDoExamples ::
  MonadJSM m =>
  m ()
attachRecursiveDoExamples = do
  attachId_ "examples-recursiveDo-1"
    counterExample1
  attachId_ "examples-recursiveDo-2"
    counterExample2

mkCounter ::
  MonadWidget t m =>
  (Event t () -> Event t () -> m (Dynamic t Int)) ->
  m ()
mkCounter network = mdo
  el "div" $
    display dCount

  (eAdd, eClear) <- el "span" $
    (,) <$> button "Add" <*> button "Clear"

  dCount <- network eAdd eClear

  pure ()

counter1 ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Event t () ->
  Event t () ->
  m (Dynamic t Int)
counter1 eAdd eClear =
  foldDyn ($) 0 .
  mergeWith (.) $ [
      const 0 <$ eClear
    , (+ 1) <$ eAdd
    ]

counterExample1 ::
  MonadWidget t m =>
  m ()
counterExample1 =
  mkCounter counter1

counter2 ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Dynamic t Int ->
  Event t () ->
  Event t () ->
  m (Dynamic t Int)
counter2 dLimit eAdd eClear = mdo
  let
    dAtLimit = (<) <$> dCount <*> dLimit

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      const 0 <$ eClear
    , (+ 1)   <$ gate (current dAtLimit) eAdd
    ]

  return dCount

counterExample2 ::
  MonadWidget t m =>
  m ()
counterExample2 =
  mkCounter $ counter2 (pure 5)
