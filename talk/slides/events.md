
# `Event`

##

```haskell
data Event a



 
```

##

```haskell
data Event a

  ~

[(time, a)] 
```

<!--
##

has values of type `a` at particular points in time


##

The observable points of time are also known as frames

##

Every external triggered event happens in its own frame

##

So we're dealing with something like event-sourcing ...

##

... if it was created by mathematicians instead of consultants
-->

## 

```haskell
eOutput = eInput
```

<div id="examples-events-frame"></div>

## 

<div id="examples-events-tick"></div>

<!-- 
## 


`Event`s derived from other `Event`s can occur in the same frame.

## 

There is an obvious way that this can happen
-->

## 

```haskell
instance Functor Event where ...
```

##

```haskell
flipColour :: Colour -> Colour
flipColour Red  = Blue
flipColour Blue = Red
```

```haskell
eOutput = flipColour <$> eInput
```

<div id="examples-events-flipper"></div>

##

<!-- We tend to use `<$` often in FRP -->

```haskell
(<$) :: Functor f => a -> f b -> f a
```

##

```haskell
eOutput = Blue <$ eInput
```

<div id="examples-events-blue"></div>

##
<!--
We can create an `Event` which fires in _some_ of the frames that another `Event` is firing in.

##

The simplest way to do that is to filter an `Event` with a predicate:
-->
```haskell
ffilter :: (a -> Bool) 
        -> Event a 
        -> Event a
```

##

```haskell
isRed :: Colour -> Bool
isRed Red  = True
isRed Blue = False
```

```haskell
eOutput = ffilter isRed eInput
```

<div id="examples-events-red"></div>

<!--
##

We can create `Event`s that might be occurring in the same frame as other `Event`s

##

That means when we want to work with multiple `Event`s, we need to be able to handle the case where several `Event`s are active in the same frame.

##

Let's set up some `Event`s that occasionally happen simultaneously
-->

##

<!--Assume we have access to-->
```haskell
eCount :: Event Int
```

##

```haskell
div3 :: Int -> Bool
div3 x = x `mod` 3 == 0
```

```haskell
div5 :: Int -> Bool
div5 x = x `mod` 5 == 0
```

##

<!--Then-->
```haskell
eFizz :: Event Text
eFizz = "Fizz" <$ ffilter div3 eCount
```
<!--and-->
```haskell
eBuzz :: Event Text
eBuzz = "Buzz" <$ ffilter div5 eCount
```
<!--will occasionally collide.-->

##

```haskell
eFizz :: Event Text
eFizz = "Fizz" <$ ffilter div3 eCount
```

```haskell
eBuzz :: Event Text
eBuzz = "Buzz" <$ ffilter div5 eCount
```

<div id="examples-events-fizz-and-buzz"></div>

<!--
##

What can we do about the collisions?
-->

##

```haskell
leftmost :: [Event a] -> Event a
```

##

```haskell
eLeft :: Event Text
eLeft = leftmost [eFizz, eBuzz]
```

<div id="examples-events-leftmost"></div>

##

<!-- Side note: We've been using this in the first set of examples: -->

```haskell
eInput = leftmost [Red <$ eRed, Blue <$ eBlue]
```

<!--
##

That's still not quite what we want for the collisions ...
-->

##

```haskell
mergeWith :: (a -> a -> a) -> [Event a] -> Event a
```

##

```haskell
eMerge :: Event Text
eMerge = mergeWith (<>) [eFizz, eBuzz]
```

<div id="examples-events-mergeWith"></div>

##

```haskell
instance Monoid a => Monoid (Event a) where ...
```

##

```haskell
eMerge :: Event Text
eMerge = eFizz <> eBuzz
```

<div id="examples-events-merge"></div>

<!--
##

We still need to print the numbers if we don't have a Fizz or a Buzz...
-->

##

```haskell
eFizzBuzz :: Event Text
eFizzBuzz = leftmost [eFizz <> eBuzz, eCount]
```

<div id="examples-events-fizzbuzz"></div>

## 

```javascript
var button = document.getElementById("rx-button");
var clicks = Rx.Observable.fromEvent(button, "click");

var ones = clicks.scan(count => count + 1, 0);

var hundreds = ones.map(function(x) { return x * 100; });

var sum = ones.combineLatest(hundreds, function(o, h) {
    return o + h; 
  });

sum.subscribe(function(s) { alert(s); });
```

<button id="rx-button">Click Me</button>

## 

```haskell
  button <- getElementById "reflex-button"
  eClick <- domEvent Click button

  eOnes <- accum (+) 0 (1 <$ eClick)

  let eHundreds = (* 100) <$> eOnes

  let eSum = mergeWith (+) [eOnes, eHundreds]

  alertEvent show eSum
```

<div id="examples-events-clickMe"></div>
