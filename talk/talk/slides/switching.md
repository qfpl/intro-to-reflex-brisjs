
# Switching

##

<div class="demo" id="examples-switch-colour-1"></div>

##

```haskell
switchPromptly :: (Reflex t, MonadHold t m) 
               => Event a 
               -> Event (Event a) 
               -> m (Event a)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => 
             

                m (Event t Colour, Event t Colour)
switchColour                          = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             

             -> m (Event t Colour, Event t Colour)
switchColour                          = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             

             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1                 = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()

             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1                 = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()

             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2        = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2        = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do











  
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- _




  eOut2 <- _




  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly _




  eOut2 <- switchPromptly _




  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput _




  eOut2 <- switchPromptly _




  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput _




  eOut2 <- switchPromptly never _




  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [


    ]

  eOut2 <- switchPromptly never . leftmost $ [


    ]

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1

    ]

  eOut2 <- switchPromptly never . leftmost $ [


    ]

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [


    ]

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t ()
             -> Event t ()
             -> Event t Colour
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1

    ]

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t () 
             -> Event t () 
             -> Event t Colour 
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (eOut1, eOut2)
```

##

```haskell
switchPromptly :: (Reflex t, MonadHold t m) 
               => Event a 
               -> Event    (Event a) 
               -> m (Event a)
```

##

```haskell
switch         :: (Reflex t, MonadHold t m) 
               => Event a 
               -> Event    (Event a) 
               -> m (Event a)
```

##

```haskell
switch         ::  Reflex t
               => Event a 
               -> Event    (Event a) 
               -> m (Event a)
```

##

```haskell
switch         ::  Reflex t
               => Event a 
               -> Event    (Event a) 
               ->    Event a
```

##

```haskell
switch         ::  Reflex t
               => 
                  Event    (Event a) 
               ->    Event a
```

##

```haskell
switch         ::  Reflex t
               => 
                  Behavior (Event a)
               ->    Event a
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t () 
             -> Event t () 
             -> Event t Colour 
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (       eOut1,        eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t () 
             -> Event t () 
             -> Event t Colour 
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold           eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (       eOut1,        eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t () 
             -> Event t () 
             -> Event t Colour 
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold           eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  bOut2 <- hold           never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (       eOut1,        eOut2)
```

##

```haskell
switchColour :: (Reflex t, MonadHold t m) 
             => Event t () 
             -> Event t () 
             -> Event t Colour 
             -> m (Event t Colour, Event t Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold           eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  bOut2 <- hold           never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (switch eOut1, switch eOut2)
```

## 

```haskell
widget1 :: MonadWidget t m 
   => m (Event t Text)
widget1 = do

  
```

```haskell
widget2 :: MonadWidget t m 
   => m (Event t Text)
widget2 = do

  
```

## 

```haskell
widget1 :: MonadWidget t m 
   => m (Event t Text)
widget1 = do
  eClick <- button "OK"
  pure $ "OK" <$ eClick
```

```haskell
widget2 :: MonadWidget t m 
   => m (Event t Text)
widget2 = do

  
```

## 

```haskell
widget1 :: MonadWidget t m 
   => m (Event t Text)
widget1 = do
  eClick <- button "OK"
  pure $ "OK" <$ eClick
```

```haskell
widget2 :: MonadWidget t m 
   => m (Event t Text)
widget2 = do
  ti <- textInput def
  pure $ ti ^. textInput_input
```

##

```haskell
  eSwitch <- button "Switch"


















  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
















  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount













  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""









  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1







  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2






  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2

  let
    eText = leftmost [eText1, eText2]


  
  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2

  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText

  
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap even dCount
    dOdd  = fmap odd  dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2

  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$
    dynText dText
```

##

<div class="demo" id="examples-switch-widget-hide"></div>

##

```haskell
widgetHold :: (MonadAdjust t m, MonadHold t m) 
           => m a 
           -> Event t (m a) 
           -> m (Dynamic t a)
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    dEven = fmap    even             dCount
    dOdd  = fmap    odd              dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2


  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    dOdd  = fmap    odd              dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2


  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  let
    mkHidden False = "hide"
    mkHidden True  = ""

  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2


  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount





  eText1 <- elDynClass "div" (mkHidden <$> dEven) widget1
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2


  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount





      widget1 <$ eEven
  eText2 <- elDynClass "div" (mkHidden <$> dOdd)  widget2


  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount





      widget1 <$ eEven
    , widget2 <$ eOdd


  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount




                                 leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount




  deText <- widgetHold widget1 . leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  let
    eText = leftmost [eText1, eText2]

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount




  deText <- widgetHold widget1 . leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

<div class="demo" id="examples-switch-widget-hold"></div>

## 

```haskell
dyn :: (MonadAdjust t m, PostBuild t m) 
    => Dynamic t (m a) 
    -> m (Event t a)
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  deText  <- widgetHold widget1 . leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  deText  <- holdDyn widget1    . leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  dWidget <- holdDyn widget1    . leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  let
    eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  dWidget <- holdDyn widget1    . leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]




  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  dWidget <- holdDyn widget1 .    leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  eeText <- dyn dWidget


  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  dCount <- count eSwitch
  let
    eEven = ffilter even . updated $ dCount
    eOdd  = ffilter odd  . updated $ dCount

  dWidget <- holdDyn widget1 .    leftmost $ [
      widget1 <$ eEven
    , widget2 <$ eOdd
    ]

  eeText <- dyn dWidget
  eText  <- switchPromptly never eeText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

<div class="demo" id="examples-switch-dyn"></div>

##

```haskell
newtype Workflow t m a = Workflow { 
    unWorkflow :: m (a, Event t (Workflow t m a))
  }
```

```haskell
workflow :: (DomBuilder t m, MonadFix m, MonadHold t m) 
         => Workflow t m a 
         -> m (Dynamic t a)
```

##

```haskell
  eSwitch <- button "Switch"












  deText <- _

  let eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  let
    workflow1 :: Workflow t m (Event t Text)
    worfklow1 = Workflow $ do
      eText1 <- widget1
      pure (eText1, workflow2 <$ eSwitch)






  deText <- _

  let eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  let
    workflow1 :: Workflow t m (Event t Text)
    worfklow1 = Workflow $ do
      eText1 <- widget1
      pure (eText1, workflow2 <$ eSwitch)

    workflow2 :: Workflow t m (Event t Text)
    workflow2 = Workflow $ do
      eText2 <- widget2
      pure (eText2, workflow1 <$ eSwitch)

  deText <- _

  let eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

```haskell
  eSwitch <- button "Switch"

  let
    workflow1 :: Workflow t m (Event t Text)
    worfklow1 = Workflow $ do
      eText1 <- widget1
      pure (eText1, workflow2 <$ eSwitch)

    workflow2 :: Workflow t m (Event t Text)
    workflow2 = Workflow $ do
      eText2 <- widget2
      pure (eText2, workflow1 <$ eSwitch)

  deText <- workflow workflow1

  let eText = switch . current $ deText

  dText <- holdDyn "" eText
  el "div"$ dynText dText
```

##

<div class="demo" id="examples-switch-workflow-1"></div>

