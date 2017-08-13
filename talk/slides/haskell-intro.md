
# Just enough Haskell to be dangerous

## An aside on types

The type systems feels nothing at all like the type systems from Java / C++

## An aside on types

Types are simple tests and documentation that never go out of date

## An aside on types

You mostly don't have to write them

## An aside on types

They can be inferred most of the time

##

```haskell
data Person =
  Person {
    name :: String
  , age :: Int
  }





  
```

##

```haskell
data Person =
  Person {
    name :: String
  , age :: Int
  }

Person :: String -> Int -> Person



  
```

##

```haskell
data Person =
  Person {
    name :: String
  , age :: Int
  }

Person :: String -> Int -> Person

name :: Person -> String

  
```

##

```haskell
data Person =
  Person {
    name :: String
  , age :: Int
  }

Person :: String -> Int -> Person

name :: Person -> String

age :: Person -> Int
```

##

```haskell
data Person =
  Person {
    name :: String
  , age :: Int
  }

happyBirthday :: Person -> Person
happyBirthday (Person name age) = 
  Person name (age + 1)
```

```
> happyBirthday (Person "alice" 31)
Person "alice" 32
```

##

```
data Maybe a =
    Just a 
  | Nothing
```

##

```haskell
class Eq a where
  (==) :: a -> a -> Bool
```

##

```haskell
instance Eq Person where
  (==) (Person n1 a1) (Person n2 a2) =
    n1 == n2 && a1 == a2
```

##

```haskell
instance Eq a => Eq (Maybe a) where
  (==) (Just x1) (Just x2) =
    x1 == x2
  (==) Nothing Nothing =
    True
  (==) _ _ =
    False
```

##

```haskell
class Monoid m where
  mempty :: m
  (<>)   :: m -> m -> m
```

```haskell
instance Monoid (Maybe a) where
  mempty = Nothing
  (<>) (Just x) _ = Just x
  (<>) Nothing  y = y
```

##

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

```haskell
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap _ Nothing  = Nothing
```

##

```haskell
class Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

##

```haskell
loadName :: LoadDB String

loadAge :: LoadDB Int

loadPerson :: LoadDB Person
loadPerson = 
  Person <$> loadName <*> loadAge

  
```

##

```haskell
loadName :: LoadDB String

loadAge :: LoadDB Int

loadPerson :: LoadDB Person
loadPerson = do
  name <- loadName
  age  <- loadAge
  return (Person name age)
```

##

```haskell
parseName :: Parser String

parseAge :: Parser Int

parsePerson :: Parser Person
parsePerson = 
  Person <$> parseName <*> parseAge







  
```

##

```haskell
parseName :: Parser String

parseAge :: Parser Int
  
parsePerson :: Parser Person
parsePerson = do
  parseHeader "Person"

  name <- parseName
  skipSpaces

  age  <- parseAge
  skipSpaces

  return (Person name age)
```

##

```haskell
renderName :: String -> RenderHtml ()

renderAge :: Int -> RenderHtml ()

div :: RenderHtml a -> RenderHtml a

renderPerson :: Person -> RenderHtml ()
renderPerson (Person name age) =
  div ( do
    label "Name" name 
    label "Age" (show age)
  )
```

##

```haskell
renderName :: String -> RenderHtml ()

renderAge :: Int -> RenderHtml ()

div :: RenderHtml a -> RenderHtml a

renderPerson :: Person -> RenderHtml ()
renderPerson (Person name age) =
  div $ do
    label "Name" name 
    label "Age" (show age)
  
```
