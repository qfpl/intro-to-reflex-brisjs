For the last bit, with the todo item and the collections

Start with
- an edit box that doesn't signal removes, only text changes
- a todo item that takes in a dyn of complete + text
  and outputs events for changes of complete / text / remove 
- something that wraps them together in a collection at the top level
  - handle the removes when the text goes to empty
  
Add
  - handle the clear complete button from the dyn of completion 
    available in the top level model

Change to
- an edit box that signals removes 
  - and keeps the text state to itself?
- roll those change up to the top
- this means our input model only has the initial values of the item text in it

Do we want to be able to handle initial values for the items?
- in that case we'd want a Map Int (Bool, Text) for our input state to list
  - because we can just use the initial values to kick of the foldDyn
  - may as well keep the model up date then
  - in that case may as well handle eClearComplete and text removal at the top level
  - we have two kinds of events from the children then
    - updates to our collection state that alter it
    - updates to our collection state that remove elements from it

Change to
- pass a clear completed event in and pass dyn completed out of each widget

Change to
- use event writer to gather the changes without having to worry about switching and merging as much
