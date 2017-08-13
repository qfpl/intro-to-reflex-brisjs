
# `Behavior`

##

```haskell
data Behavior a



 
```

##

```haskell
data Behavior a

  ~

time -> a
```

##

```haskell
hold :: ReflexM m 
     => a
     -> Event a
     -> m (Behavior a)
```

##

```haskell
sampleBlue                = do
  bColour <- hold
  
```

##

```haskell
sampleBlue                = do
  bColour <- hold Blue
  
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue
 
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
 
```

##

```haskell
tag :: Behavior a
    -> Event  b
    -> Event  a
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
 
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
           tag 
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
           tag bColour
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
           tag bColour
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
           tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  return $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  return $ tag bColour eSample
```

<div id="examples-behaviors-sampleBlue1"></div>

<!--
##

The state doesn't change until the frame _after_ the firing of the `Event`s in `hold`.

##

We can see that by sampling from the `Behavior` when any of the buttons are pressed
-->

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput

  return $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [                     ]
  return $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [              eSample]
  return $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [      eInput, eSample]
  return $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  return $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  return $ tag bColour eAny
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  return $ tag bColour eAny
```

<div id="examples-behaviors-sampleBlue2"></div>

##

```haskell
attach          :: 
                   Behavior a 
                -> Event b 
                -> Event (a, b)
```

##

```haskell
attachWith      :: (a -> b -> c) 
                -> Behavior a
                -> Event b
                -> Event c
```

##

```haskell
attachWithMaybe :: (a -> b -> Maybe c) 
                -> Behavior a
                -> Event b
                -> Event c
```

##

```haskell
gate            :: 
                   Behavior Bool
                -> Event a
                -> Event a
```

