
# Higher order FRP

##

Let's have a look at some higher order functions

##

```haskell
map   :: (a -> b) 
      -> [a] 
      -> [b]
  
```

##

```haskell
foldr :: (a -> b -> b) 
      -> b 
      -> [a] 
      -> b
```

##

Let's have a look at some higher order FRP

##

```haskell
switch        :: 
               
                 Behavior (Event a)
              ->           Event a
```

##

```haskell
switchPrompty :: ReflexM m 
              =>           Event a
              -> Event    (Event a)
              -> m        (Event a)
```

##

<div class="demo" id="examples-switch-colour-2"></div>

##

```haskell
switchColour :: ReflexM m
             =>


             -> m (Event Colour, Event Colour)
switchColour                          = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()


             -> m (Event Colour, Event Colour)
switchColour                          = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()


             -> m (Event Colour, Event Colour)
switchColour eSwitch1                 = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()

             -> m (Event Colour, Event Colour)
switchColour eSwitch1                 = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()

             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2        = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2        = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do









    



  
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do





  let eOut1 = _





  let eOut2 = _

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- _



  let eOut1 = _

  bOut2 <- _



  let eOut2 = _

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- _



  let eOut1 = switch bOut1

  bOut2 <- _



  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold _      _



  let eOut1 = switch bOut1

  bOut2 <- hold _     _



  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput _



  let eOut1 = switch bOut1

  bOut2 <- hold _     _



  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput _



  let eOut1 = switch bOut1

  bOut2 <- hold never _



  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput . leftmost $ [
      _      <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold never . leftmost $ [
      _      <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold never . leftmost $ [
      _      <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold never . leftmost $ [
      never  <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold never . leftmost $ [
      never  <$ eSwitch1
    , _      <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event ()
             -> Event ()
             -> Event Colour
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- hold           eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold           never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  bOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold           never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]
  let eOut1 = switch bOut1

  bOut2 <- hold           never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]


  bOut2 <- hold           never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]


  bOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
switchColour eSwitch1 eSwitch2 eInput = do

  eOut1 <- switchPromptly eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]


  eOut2 <- switchPromptly never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]
  let eOut2 = switch bOut2

  pure (eOut1, eOut2)
```

##

```haskell
switchColour :: ReflexM m
             => Event () 
             -> Event () 
             -> Event Colour 
             -> m (Event Colour, Event Colour)
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
