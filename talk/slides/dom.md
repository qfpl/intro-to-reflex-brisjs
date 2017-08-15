
# Working with the DOM

## 

<!--
We layout DOM elements using a monad that is supplied by `reflex-dom`

## 

We'll be introducing some pieces of TodoMVC as we go

##
-->

```haskell
el :: ReflexM m
   => Text 
   -> m a 
   -> m a
```

```haskell
text :: ReflexM m
     => Text 
     -> m ()
```

## 

```haskell
el "div" $
  text "TODO"
```

<div class="demo" id="examples-dom-todo"></div>

<!--
##

We can use Dynamics to set up the JS to listen for changes and to make them happen, at the elements that need to change

##

Thanks to some internal magic, if you don't use an `Event` from a component then no listeners are set up for that `Event` 

##

The end result is doing the same thing a virtual DOM framework would do, but is eliding the diffs and patches

##

And we have finer grain control of what undergoes rendering changes
-->

##

```haskell
button :: ReflexM m
       => Text 
       -> m (Event ())
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem label =
  el "div" $ do
    el "div" $ 
      text label
    button "Remove"
```

<div class="demo" id="examples-dom-todoitem-1"></div>

##

```haskell
dynText :: ReflexM m
        => Dynamic Text 
        -> m ()
```

##

```haskell
el "div" $ do



  eRemove <- todoItem "TODO"




  return ()
```

##

```haskell
el "div" $ do



  eRemove <- todoItem "TODO"

  dLabel <- holdDyn "" $ 
    "Removed:" <$ eRemove

  return ()
```

##

```haskell
el "div" $ do
  el "div" $ 
    dynText dLabel

  eRemove <- todoItem "TODO"

  dLabel <- holdDyn "" $ 
    "Removed:" <$ eRemove

  return ()
```

##

```haskell
el "div" $ do
  el "div" $ 
    dynText dLabel

  eRemove <- todoItem "TODO"

  dLabel <- holdDyn "" $ 
    "Removed:" <$ eRemove

  return ()
```

<div class="demo" id="examples-dom-todoitem-2"></div>

##

```haskell
elAttr    :: ReflexM m
          => Text 
          ->          Map Text Text 
          -> m a 
          -> m a
```

##

```haskell
elDynAttr :: ReflexM m
          => Text 
          -> Dynamic (Map Text Text) 
          -> m a 
          -> m a
```

##

```haskell
elClass    :: ReflexM m
           => Text 
           ->         Text 
           -> m a 
           -> m a
```

##

```haskell
elDynClass :: ReflexM m
           => Text 
           -> Dynamic Text 
           -> m a 
           -> m a
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem placeholder =
  el      "div"             $ do

    el         "div"              $
      text placeholder

    eRemove <- button "Remove"




    return eRemove
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem placeholder =
  elClass "div"             $ do

    el         "div"              $
      text placeholder

    eRemove <- button "Remove"




    return eRemove
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem placeholder =
  elClass "div" "todo-item" $ do

    el         "div"              $
      text placeholder

    eRemove <- button "Remove"




    return eRemove
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem placeholder =
  elClass "div" "todo-item" $ do

    el         "div"              $
      text placeholder

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed" <$ eRemove

    return eRemove
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem placeholder =
  elClass "div" "todo-item" $ do

    elDynClass "div" dRemoveClass $
      text placeholder

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed" <$ eRemove

    return eRemove
```

##

```haskell
todoItem :: ReflexM m
         => Text 
         -> m (Event ())
todoItem placeholder =
  elClass "div" "todo-item" $ do

    elDynClass "div" dRemoveClass $
      text placeholder

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed" <$ eRemove

    return eRemove
```

<div class="demo" id="examples-dom-todoitem-3"></div>

##

```haskell
data CheckboxConfig = 
  CheckboxConfig { 
      checkboxConfig_setValue   :: Event   Bool
    , checkboxConfig_attributes :: Dynamic (Map Text Text)
    }
```

```haskell
instance Default CheckboxConfig where ...
```

## 

```haskell
checkbox :: (...) 
         => Bool 
         -> CheckboxConfig
         -> m Checkbox
```

## 

```haskell
data Checkbox = 
  Checkbox { 
     checkbox_value  :: Dynamic Bool
   , checkbox_change :: Event   Bool
   }
```

##

```haskell
data TodoItemConfig =
  TodoItemConfig {
    todoItemConfig_dText :: Dynamic Text
  }

data TodoItem =
  TodoItem {
    todoItem_dComplete :: Dynamic Bool
  , todoItem_eRemove   :: Event   ()
  }
```

##

```haskell
todoItem :: ReflexM m
         => TodoItemConfig 
         -> m TodoItem
todoItem (TodoItemConfig dText) =
  elClass "div" "todo-item" $ do
  
  
  

    elDynClass "div" dRemoveClass $
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed" <$ eRemove

    return $ 
      TodoItem           eRemove
```

##

```haskell
todoItem :: ReflexM m
         => TodoItemConfig 
         -> m TodoItem
todoItem (TodoItemConfig dText) =
  elClass "div" "todo-item" $ do
    cb <- checkbox False def
  
  

    elDynClass "div" dRemoveClass $
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed" <$ eRemove

    return $ 
      TodoItem           eRemove
```

##

```haskell
todoItem :: ReflexM m
         => TodoItemConfig 
         -> m TodoItem
todoItem (TodoItemConfig dText) =
  elClass "div" "todo-item" $ do
    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value

    elDynClass "div" dRemoveClass $
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed" <$ eRemove

    return $ 
      TodoItem           eRemove
```

##

```haskell
todoItem :: ReflexM m
         => TodoItemConfig 
         -> m TodoItem
todoItem (TodoItemConfig dText) =
  elClass "div" "todo-item" $ do
    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value

    elDynClass "div" dRemoveClass $
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed" <$ eRemove

    return $ 
      TodoItem dComplete eRemove
```

##

```haskell
todoItem :: ReflexM m
         => TodoItemConfig 
         -> m TodoItem
todoItem (TodoItemConfig dText) =
  elClass "div" "todo-item" $ do
    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value

    elDynClass "div" dRemoveClass $
      dynText dText

    eRemove <- button "Remove"
    dRemoveClass <- holdDyn "" $ 
      "removed" <$ eRemove

    return $ 
      TodoItem dComplete eRemove
```

<div class="demo" id="examples-dom-todoitem-4"></div>

##

```haskell
  ...
    let
      dComplete = cb ^. checkbox_value






    elDynClass "div"                    dRemoveClass  $ 
      dynText dText
  ...
```

##

```haskell
  ...
    let
      dComplete = cb ^. checkbox_value

      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "



    elDynClass "div"                    dRemoveClass  $ 
      dynText dText
  ...
```

##

```haskell
  ...
    let
      dComplete = cb ^. checkbox_value

      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "

      dCompleteClass = mkCompleteClass <$> dComplete

    elDynClass "div"                    dRemoveClass  $ 
      dynText dText
  ...
```

##

```haskell
  ...
    let
      dComplete = cb ^. checkbox_value

      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "

      dCompleteClass = mkCompleteClass <$> dComplete

    elDynClass "div" (dCompleteClass <> dRemoveClass) $ 
      dynText dText
  ...
```

##

```haskell
  ...
    let
      dComplete = cb ^. checkbox_value

      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "

      dCompleteClass = mkCompleteClass <$> dComplete

    elDynClass "div" (dCompleteClass <> dRemoveClass) $ 
      dynText dText
  ...
```

<div class="demo" id="examples-dom-todoitem-5"></div>

##

```haskell
data TextInputConfig = 
  TextInputConfig { 
      textInputConfig_inputType    :: Text
    , textInputConfig_initialValue :: Text
    , textInputConfig_setValue     :: Event  Text
    , textInputConfig_attributes   :: 
        Dynamic (Map Text Text)
    }
```

```haskell
instance Default TextInputConfig where ...
```

##

```haskell
textInput :: ReflexM m
          => TextInputConfig
          -> m TextInput
```

## 

```haskell
data TextInput = 
  TextInput { 
      textInput_value          :: Dynamic Text
    , textInput_input          :: Event Text
    , textInput_keypress       :: Event Word
    , textInput_keydown        :: Event Word
    , textInput_keyup          :: Event Word
    , textInput_hasFocus       :: Dynamic Bool
    , textInput_builderElement :: 
        InputElement EventResult GhcjsDomSpace
    }
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def                                                      









  
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")








  
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")



  let
    bValue   = current $ ti ^. textInput_value



  
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")



  let
    bValue   = current $ ti ^. textInput_value
    eAtEnter = tag bValue (getKey ti Enter)


  
```


## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")



  let
    bValue   = current $ ti ^. textInput_value
    eAtEnter = tag bValue (getKey ti Enter)
    eDone    = ffilter (not . Text.null) eAtEnter

  
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")



  let
    bValue   = current $ ti ^. textInput_value
    eAtEnter = tag bValue (getKey ti Enter)
    eDone    = ffilter (not . Text.null) eAtEnter

  return eDone
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eDone)

  let
    bValue   = current $ ti ^. textInput_value
    eAtEnter = tag bValue (getKey ti Enter)
    eDone    = ffilter (not . Text.null) eAtEnter

  return eDone
```

## 

```haskell
addItem :: ReflexM m 
        => m (Event Text)
addItem = do
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eDone)

  let
    bValue   = current $ ti ^. textInput_value
    eAtEnter = tag bValue (getKey ti Enter)
    eDone    = ffilter (not . Text.null) eAtEnter

  return eDone
```

<div class="demo" id="examples-dom-todoitem-6"></div>
