
# Switching widgets

##

You often have a choice between hiding widgets or switching them

##

```haskell
textWidget   :: ReflexM m 
             => m (Event Text)
```

<div class="demo" id="examples-switch-demo-text"></div>

```haskell
buttonWidget :: ReflexM m 
             => m (Event Text)
```

<div class="demo" id="examples-switch-demo-button"></div>

```haskell
tickWidget   :: ReflexM m 
             => m (Event Text)
```

<div class="demo" id="examples-switch-demo-tick"></div>

##

```haskell
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let 
    dNotToggle = not <$> dToggle
```

## 

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$> dToggle
    dHide2 = mkHidden <$> dNotToggle












  
```

## 

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$> dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget

  eText2 <- elDynClass "div" dHide2 $
    buttonWidget






  
```

## 

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$> dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget

  eText2 <- elDynClass "div" dHide2 $
    buttonWidget

  let 
    eText = leftmost [
        gate (current dToggle   ) eText1
      , gate (current dNotToggle) eText2

      ]
```

## 

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$> dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget

  eText2 <- elDynClass "div" dHide2 $
    buttonWidget

  let 
    eText = leftmost [
        gate (current dToggle   ) eText1
      , gate (current dNotToggle) eText2
      , "" <$ eSwitch
      ]
```

## 

<div class="demo" id="examples-switch-hide-button"></div>

## 

```haskell
widgetHold :: ReflexM m
           =>         m          a 
           -> Event  (m          a) 
           ->         m (Dynamic a)
```

## 

```haskell
  let
    eToggle = updated     dToggle
    eShow1  = ffilter id  eToggle
    eShow2  = ffilter not eToggle










  
```

## 

```haskell
  let
    eToggle = updated     dToggle
    eShow1  = ffilter id  eToggle
    eShow2  = ffilter not eToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]





  
```

## 

```haskell
  let
    eToggle = updated     dToggle
    eShow1  = ffilter id  eToggle
    eShow2  = ffilter not eToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]

  let
    eText = 
        switch (current deText)
        
  
```

## 

```haskell
  let
    eToggle = updated     dToggle
    eShow1  = ffilter id  eToggle
    eShow2  = ffilter not eToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]

  let
    eText = leftmost [
        switch (current deText)
      , "" <$ eSwitch
      ]
```

## 

<div class="demo" id="examples-switch-hold-button"></div>

##

It becomes clearer why you would want this when we replace `buttonWidget` with `tickWidget`

## With hiding

<div class="demo" id="examples-switch-hide-tick"></div>

## With `widgetHold`

<div class="demo" id="examples-switch-hold-tick"></div>

