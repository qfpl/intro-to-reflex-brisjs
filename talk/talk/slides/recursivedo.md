
# `RecursiveDo` 

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

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => 
           Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter        eAdd eClear =  do


  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                         eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter        eAdd eClear =  do


  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                         eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do


  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$                         eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do


  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ gate _                  eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```

##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ gate _                  eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```
##

```haskell

counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ gate (current dLimitOK) eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```

##

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int
        -> Event t ()
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear =  do
  let dLimitOK = (<) <$> dCount <*> dLimit

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ gate (current dLimitOK) eAdd
    , const 0 <$                         eClear
    ]
    
  pure dCount
```

## 

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int 
        -> Event t () 
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = mdo
  let dLimitOK = (<) <$> dCount <*> dLimit

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ gate (current dLimitOK) eAdd
    , const 0 <$                         eClear
    ]

  pure dCount
```

## 

```haskell
{-# LANGUAGE RecursiveDo #-}
counter :: (Reflex t, MonadFix m, MonadHold t m) 
        => Dynamic t Int 
        -> Event t () 
        -> Event t ()
        -> m (Dynamic t Int)
counter dLimit eAdd eClear = mdo
  let dLimitOK = (<) <$> dCount <*> dLimit

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
      (+ 1)   <$ gate (current dLimitOK) eAdd
    , const 0 <$                         eClear
    ]

  pure dCount
```

<div id="examples-recursiveDo-2"></div>
