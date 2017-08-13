
# `Dynamic`

##

```haskell
data Dynamic a



 
```

##

```haskell
data Dynamic a

  ~ 

(Event a, Behavior a) 
```

##

```haskell
updated :: Dynamic a 
        -> Event a
```

## 

```haskell
current :: Dynamic a 
        -> Behavior a
```

##

This is what lets `reflex` catch up to the virtual DOM

<!--
##

It manages state, and lets you know when the state has changed ...

##

... without having to poll the state for changes

##

We use `Dynamic` for the things that we want to change

##

Because `Dynamic`s are values we can pass them down through other components, right to where they need to be

##

We construct `Dynamic`s directly rather than combining `Event`s and `Behavior`s for reasons of correctness and efficiency
-->

##

```haskell
hold        :: ReflexM m
            => 
               a 
            -> Event a 
            -> m (Behavior a)
```

##

```haskell
holdDyn     :: ReflexM m
            => 
               a 
            -> Event a 
            -> m (Dynamic  a)
```

##

```haskell
foldDyn     :: ReflexM m
            => (a -> b -> b) 
            -> b 
            -> Event a 
            -> m (Dynamic  b)
```

##

```haskell
foldDyn ($) :: ReflexM m
            => (a -> b -> b) 
            -> b 
            -> Event a 
            -> m (Dynamic  b)
```

##

```haskell
foldDyn ($) :: ReflexM m
            => 
               c 
            -> Event (c -> c)
            -> m (Dynamic  c)
```

##

```haskell
counter :: ReflexM m
        =>

           m (Dynamic Int)
counter             =



 
```

##

```haskell
counter :: ReflexM m
        => Event ()

        -> m (Dynamic Int)
counter             =



 
```

##

```haskell
counter :: ReflexM m
        => Event ()

        -> m (Dynamic Int)
counter eAdd        =



 
```

##

```haskell
counter :: ReflexM m
        => Event ()

        -> m (Dynamic Int)
counter eAdd        =
  foldDyn ($) 0
  
  
   
```

##

```haskell
counter :: ReflexM m
        => Event ()

        -> m (Dynamic Int)
counter eAdd        =
  foldDyn ($) 0                 $
      (+ 1)   <$ eAdd
  
   
```

##

```haskell
counter :: ReflexM m
        => Event ()
        -> Event ()
        -> m (Dynamic Int)
counter eAdd        =
  foldDyn ($) 0                 $
      (+ 1)   <$ eAdd
  
   
```

##

```haskell
counter :: ReflexM m
        => Event ()
        -> Event ()
        -> m (Dynamic Int)
counter eAdd eClear =
  foldDyn ($) 0                 $
      (+ 1)   <$ eAdd
  
   
```

##

```haskell
counter :: ReflexM m
        => Event ()
        -> Event ()
        -> m (Dynamic Int)
counter eAdd eClear =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
  
    ] 
```

##

```haskell
counter :: ReflexM m
        => Event ()
        -> Event ()
        -> m (Dynamic Int)
counter eAdd eClear =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
```

##

```haskell
counter :: ReflexM m
        => Event ()
        -> Event ()
        -> m (Dynamic Int)
counter eAdd eClear =
  foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
```

<div id="examples-dynamic-counter"></div>

##

<!--

`Behavior`s are specified at all points of time

 This is also true of `Dynamic`s

`Event`s are only specified at some unique points of time

So all `Behavior`s have a value before any of the `Event`s fire

It is perfectly valid to have `Event`s that build a `Behavior` and also depend on the `Behavior`

This is where the one frame delay comes in - we are using the old value of the `Behavior` to build the new value of the `Behavior`

This means some dependencies can be loops, which is fine

We just need a way to specify them
-->

```haskell
counter :: ReflexM m
        => 
           Event ()
        -> Event ()
        -> m (Dynamic Int)
counter        eAdd eClear = do



  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter        eAdd eClear = do



  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit eAdd eClear = do



  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit eAdd eClear = do
  let dLimitOK = (<) <$> dCount <*> dLimit


  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit eAdd eClear = do
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAdd
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit eAdd eClear = do
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit eAdd eClear = do
  let dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

<div id="examples-recursiveDo-3"></div>

##

```haskell
limit :: ReflexM m
      => Event () 
      -> Event ()
      -> Event ()
      -> m (Dynamic Int)
limit eStart eAdd eClear = do
  eLoadLimit <- performEvent (loadLimitDb <$ eStart)

  dLimit <- foldDyn ($) 5 . mergeWith (.) $ [
      const   <$> eLoadLimit
    , (+ 1)   <$  eAdd
    , const 0 <$  eClear
    ]
    
  performEvent_ (saveLimitDb <$> update dLimit) 
    
  return dLimit
```

<div id="examples-recursiveDo-1"></div>

##

```haskell
data Settings =
  Settings {
    settingLimit :: Int
  , settingStep  :: Int
  }
```

##

```haskell
counter :: ReflexM m
        => Dynamic Int
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit    eAdd eClear = do
  let 
      
      dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                      eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Settings
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dLimit    eAdd eClear = do
  let 
      
      dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                      eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Settings
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dSettings eAdd eClear = do
  let 
      
      dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                      eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Settings
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dSettings eAdd eClear = do
  let dLimit   = settingsLimit <$> dSettings
      
      dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                      eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Settings
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dSettings eAdd eClear = do
  let dLimit   = settingsLimit <$> dSettings
      dStep    = settingsStep  <$> dSettings
      dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                      eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter :: ReflexM m
        => Dynamic Settings
        -> Event ()
        -> Event ()
        -> m (Dynamic Int)
counter dSettings eAdd eClear = do
  let dLimit   = settingsLimit <$> dSettings
      dStep    = settingsStep  <$> dSettings
      dLimitOK = (<) <$> dCount <*> dLimit
      eAddOK   = gate (current dLimitOK) eAdd

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+  )   <$> tag (current dStep) eAddOK
    , const 0 <$ eClear
    ]
    
  return dCount
```

##

```haskell
counter (Settings <$> dLimit <*> dStep) eAdd eClear
```

<div id="examples-recursiveDo-4"></div>
