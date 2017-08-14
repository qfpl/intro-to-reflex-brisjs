
# Collections

##

```haskell
list 
  ::    Dynamic (Map k v       )
  ->   (Dynamic        v -> m a) 
  -> m (Dynamic (Map k        a))
```

##

<div class="demo" id="examples-list"></div>

##

There are going to be some common elements in how we approach this

##

```haskell
  elClass "div" "todo-list" $ do



















  
```
##

```haskell
  elClass "div" "todo-list" $ do
    eAdd   <- addItem
    dCount <- count eAdd

















  
```
##

```haskell
  elClass "div" "todo-list" $ do
    eAdd   <- addItem
    dCount <- count eAdd

    -- dModel :: Dynamic (Map Int ?)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves -- ?
      ]











  
```
##

```haskell
  elClass "div" "todo-list" $ do
    eAdd   <- addItem
    dCount <- count eAdd

    -- dModel :: Dynamic (Map Int ?)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves -- ?
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv -- ?








  
```

##

```haskell
  elClass "div" "todo-list" $ do
    eAdd   <- addItem
    dCount <- count eAdd

    -- dModel :: Dynamic (Map Int ?)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves -- ?
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv -- ?

    let
      dAllComplete = fmap and dmCompletes -- ?
      dAnyComplete = fmap or  dmCompletes -- ?




  
```

##

```haskell
  elClass "div" "todo-list" $ do
    eAdd   <- addItem
    dCount <- count eAdd

    -- dModel :: Dynamic (Map Int ?)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves -- ?
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv -- ?

    let
      dAllComplete = fmap and dmCompletes -- ?
      dAnyComplete = fmap or  dmCompletes -- ?

    eMarkAllComplete <- markAllComplete dAllComplete
    eClearComplete   <- clearComplete   dAnyComplete

  
```

##

```haskell
  elClass "div" "todo-list" $ do
    eAdd   <- addItem
    dCount <- count eAdd

    -- dModel :: Dynamic (Map Int ?)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves -- ?
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv -- ?

    let
      dAllComplete = fmap and dmCompletes -- ?
      dAnyComplete = fmap or  dmCompletes -- ?

    eMarkAllComplete <- markAllComplete dAllComplete
    eClearComplete   <- clearComplete   dAnyComplete

    pure ()
```

##

There are two ways we can fill in the details

##

1. We can maintain a full model of the items

##

```haskell
data TodoItem =
  TodoItem {
    itemComplete :: Bool
  , itemText     :: Text
  }
```

##

```haskell
todoItem :: ReflexM m 
         => Event Bool 
         -> Event () 
         -> Dynamic TodoItem 
         -> m (Event (TodoItem -> TodoItem), Event ())
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int TodoItem)









     
     
     
     
      
      
      
    ...
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int TodoItem)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd'
        
        
      ]




     
     
     
     
      
      
      
    ...
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int TodoItem)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd'
        
        
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

     
     
     
     
      
      
      
    ...
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int TodoItem)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd'
        
        
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

    let
      eChanges = combineEvents fst dmList
      eRemoves = combineEvents snd dmList
      
      
      
      
    ...
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int TodoItem)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd'
      , updateKeys <$> eChanges
      , removeKeys <$> eRemoves
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

    let
      eChanges = combineEvents fst dmList
      eRemoves = combineEvents snd dmList
      
      
      
      
    ...
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int TodoItem)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd'
      , updateKeys <$> eChanges
      , removeKeys <$> eRemoves
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

    let
      eChanges = combineEvents fst dmList
      eRemoves = combineEvents snd dmList

      dmCompletes  = fmap itemComplete dModel
      dAllComplete = fmap and          dmCompletes
      dAnyComplete = fmap or           dmCompletes
    ...
```

##

2. We can internalize as much state as we can

##

```haskell
todoItem :: ReflexM m 
         => Event Bool 
         -> Event () 
         -> Dynamic Text 
         -> m (Dynamic Bool, Event ())
```

##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int Text)














    ...
```
##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int Text)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd

      ]










    ...
```
##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int Text)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd

      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv







    ...
```
##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int Text)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd

      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

    let
      dmCompletes = combineDynamic fst dmList
      eRemoves    = combineEvent   snd dmList



    ...
```
##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int Text)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

    let
      dmCompletes = combineDynamic fst dmList
      eRemoves    = combineEvent   snd dmList



    ...
```
##

```haskell
todoList :: ReflexM m 
         => m ()
todoList = 
    ... -- dModel :: Dynamic (Map Int Text)
    dModel <- foldDyn ($) Map.empty . mergeWith (.) $ [
        Map.insert <$> current dCount <@> eAdd
      , removeKeys <$> eRemoves
      ]

    dmList <- el "ul" . list dModel $ \dv ->
      todoItem eMarkAllComplete eClearComplete dv

    let
      dmCompletes = combineDynamic fst dmList
      eRemoves    = combineEvent   snd dmList

      dAllComplete = fmap and dmCompletes
      dAnyComplete = fmap or  dmCompletes
    ...
```

##

<div class="demo" id="examples-list-2"></div>
